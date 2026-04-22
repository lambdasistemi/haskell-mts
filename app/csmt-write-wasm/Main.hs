-- |
-- Module      : Main
-- Description : WASM entry point for the CSMT write path
-- Copyright   : (c) Paolo Veronelli, 2026
-- License     : Apache-2.0
--
-- Stdio driver that cross-compiles the CSMT write side (insert +
-- root + inclusion proof) to @wasm32-wasi@. Together with
-- @csmt-verify.wasm@ this lets a browser host run the full
-- build-verify loop without a server.
--
-- The WASM process is stateless; every call receives the prior
-- 'InMemoryDB' blob on stdin, applies a batch of inserts, and
-- returns the updated blob plus the post-insert root and an
-- inclusion proof for the query key. The host (browser) is
-- responsible for persisting the returned blob — typically in
-- IndexedDB — so the tree survives page reloads.
--
-- Wire protocol (all lengths are big-endian unsigned 4-byte):
--
-- Input (stdin):
--
--   * @slen state@               — prior 'InMemoryDB' blob. Pass
--                                  @slen=0@ to start from empty.
--   * @n@                        — number of ops
--   * @n x op@                   — each op is opcode-tagged:
--                                  * @0 klen key vlen value@ → insert
--                                  * @1 klen key@            → delete
--   * @qlen queryKey@            — key to prove / disprove
--
-- Output (stdout):
--
--   * @slen state@   — updated 'InMemoryDB' blob
--   * @root@         — 32-byte post-insert root hash
--   * @vlen value@   — queried key's current value (empty if
--                      the key is not in the tree)
--   * @ptype@        — 1 byte proof type:
--                       * @0@    = inclusion (key present)
--                       * @1@    = exclusion (key absent, witness)
--                       * @0xff@ = none (tree empty, nothing to
--                                   prove either way)
--   * @plen proof@   — CBOR-encoded proof of the given type
--                      (empty when @ptype = 0xff@)
--
-- Malformed input produces an envelope with @slen=0@, zeroed root,
-- @ptype=0xff@, and empty value/proof. The process always exits 0.
module Main (main) where

import Control.Lens (Prism', prism')
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Serialize
    ( Get
    , Put
    , getByteString
    , getWord32be
    , getWord8
    , putByteString
    , putWord32be
    , putWord8
    , runGet
    , runPut
    )
import Data.Word (Word8)
import System.IO (stdin, stdout)

import CSMT.Backend.Pure
    ( InMemoryDB (..)
    , emptyInMemoryDB
    , runPure
    , runPureTransaction
    )
import CSMT.Backend.Standalone
    ( Standalone (..)
    , StandaloneCodecs (..)
    )
import CSMT.Core.CBOR (renderExclusionProof)
import CSMT.Core.Hash (Hash, parseHash, renderHash)
import CSMT.Hashes
    ( byteStringToKey
    , delete
    , fromKVHashes
    , generateInclusionProof
    , insert
    , root
    )
import CSMT.Proof.Exclusion (buildExclusionProof)

-- | Tagged mutation to apply in a batch. Matches the wire
-- opcodes: 0 = insert (klen key vlen value), 1 = delete
-- (klen key). Anything else fails the parse.
data Op
    = OpInsert ByteString ByteString
    | OpDelete ByteString

-- | Identity prism for raw byte payloads.
bytesCodec :: Prism' ByteString ByteString
bytesCodec = prism' id Just

-- | Prism between a 32-byte serialization and a 'Hash' node.
hashCodec :: Prism' ByteString Hash
hashCodec = prism' renderHash parseHash

-- | Codec bundle for raw-byte keys and values.
byteCodecs :: StandaloneCodecs ByteString ByteString Hash
byteCodecs =
    StandaloneCodecs
        { keyCodec = bytesCodec
        , valueCodec = bytesCodec
        , nodeCodec = hashCodec
        }

-- | Length-prefixed 'ByteString' on the wire: 4-byte big-endian
-- length, then the payload.
getLenBytes :: Get ByteString
getLenBytes = do
    n <- fromIntegral <$> getWord32be
    getByteString n

putLenBytes :: ByteString -> Put
putLenBytes bs = do
    putWord32be (fromIntegral (B.length bs))
    putByteString bs

-- | Serialize a @Map ByteString ByteString@ column as
-- @n (klen key vlen value)^n@.
putMapBs :: Map ByteString ByteString -> Put
putMapBs m = do
    putWord32be (fromIntegral (Map.size m))
    mapM_ (\(k, v) -> putLenBytes k *> putLenBytes v) (Map.toAscList m)

getMapBs :: Get (Map ByteString ByteString)
getMapBs = do
    n <- fromIntegral <$> getWord32be
    pairs <-
        sequence
            [ (,) <$> getLenBytes <*> getLenBytes
            | _ <- [1 .. n :: Int]
            ]
    pure (Map.fromList pairs)

-- | Serialize the persistent columns of an 'InMemoryDB'. The
-- in-memory iterator table is transient and is not part of the
-- blob.
putDB :: InMemoryDB -> Put
putDB db = do
    putMapBs (inMemoryCSMT db)
    putMapBs (inMemoryKV db)
    putMapBs (inMemoryJournal db)
    putMapBs (inMemoryMetrics db)

getDB :: Get InMemoryDB
getDB = do
    csmt <- getMapBs
    kv <- getMapBs
    journal <- getMapBs
    metrics <- getMapBs
    pure
        InMemoryDB
            { inMemoryCSMT = csmt
            , inMemoryKV = kv
            , inMemoryJournal = journal
            , inMemoryMetrics = metrics
            , inMemoryIterators = Map.empty
            }

-- | Render the full 'InMemoryDB' blob with its own 4-byte length
-- prefix, so the output envelope can be split field-by-field by a
-- host that does not know any CBOR.
putDBBlob :: InMemoryDB -> Put
putDBBlob db = putLenBytes (runPut (putDB db))

-- | Parse one mutation (opcode + payload).
getOp :: Get Op
getOp = do
    opcode <- getWord8
    case opcode of
        0 -> OpInsert <$> getLenBytes <*> getLenBytes
        1 -> OpDelete <$> getLenBytes
        _ -> fail "unknown op opcode"

-- | Parse the stdin envelope: prior DB blob, ops, query key.
-- An empty prior blob means "start from 'emptyInMemoryDB'".
parseInput
    :: ByteString
    -> Either
        String
        ( InMemoryDB
        , [Op]
        , ByteString
        )
parseInput = runGet $ do
    stateBlob <- getLenBytes
    db <- case B.null stateBlob of
        True -> pure emptyInMemoryDB
        False -> case runGet getDB stateBlob of
            Left err -> fail err
            Right d -> pure d
    n <- fromIntegral <$> getWord32be
    ops <- sequence [getOp | _ <- [1 .. n :: Int]]
    q <- getLenBytes
    pure (db, ops, q)

-- | Proof-type byte. Keep the numeric values in sync with the
-- browser demo.
ptInclusion, ptExclusion, ptNone :: Word8
ptInclusion = 0
ptExclusion = 1
ptNone = 0xff

-- | Encode the response envelope.
encodeResponse
    :: InMemoryDB
    -- ^ updated database
    -> ByteString
    -- ^ root hash
    -> ByteString
    -- ^ looked-up value
    -> Word8
    -- ^ proof type tag
    -> ByteString
    -- ^ proof bytes (may be empty)
    -> ByteString
encodeResponse db r v ptype p = runPut $ do
    putDBBlob db
    putByteString r
    putLenBytes v
    putWord8 ptype
    putLenBytes p

-- | Fallback envelope for malformed input: empty state, zeroed
-- root, no value, no proof.
emptyResponse :: ByteString
emptyResponse =
    encodeResponse
        emptyInMemoryDB
        (B.replicate 32 0)
        B.empty
        ptNone
        B.empty

-- | Apply the ops (insert and delete) in order, then read out the
-- root and the best available proof for the query key:
--
--  * if the key is in the tree → inclusion proof (value + CBOR);
--  * else if the tree has a diverging witness → exclusion proof;
--  * else (the tree is empty) → no proof.
execute
    :: InMemoryDB
    -> [Op]
    -> ByteString
    -> (InMemoryDB, ByteString, ByteString, Word8, ByteString)
execute db0 ops qk =
    let runOp (OpInsert k v) =
            insert
                fromKVHashes
                StandaloneKVCol
                StandaloneCSMTCol
                k
                v
        runOp (OpDelete k) =
            delete
                fromKVHashes
                StandaloneKVCol
                StandaloneCSMTCol
                k
        program = runPureTransaction byteCodecs $ do
            mapM_ runOp ops
            r <- root StandaloneCSMTCol
            pIncl <-
                generateInclusionProof
                    fromKVHashes
                    StandaloneKVCol
                    StandaloneCSMTCol
                    qk
            case pIncl of
                Just _ -> pure (r, pIncl, Nothing)
                Nothing -> do
                    pExcl <-
                        buildExclusionProof
                            []
                            StandaloneCSMTCol
                            (byteStringToKey qk)
                    pure (r, Nothing, pExcl)
        ((rOut, inclOut, exclOut), db1) = runPure db0 program
        rootBs = fromMaybe (B.replicate 32 0) rOut
        (val, ptype, proof) = case inclOut of
            Just (v, pbs) -> (v, ptInclusion, pbs)
            Nothing -> case exclOut of
                Just ep ->
                    (B.empty, ptExclusion, renderExclusionProof ep)
                Nothing -> (B.empty, ptNone, B.empty)
    in  (db1, rootBs, val, ptype, proof)

main :: IO ()
main = do
    raw <- B.hGetContents stdin
    let payload = case parseInput raw of
            Left _ -> emptyResponse
            Right (db, ops, qk) ->
                let (db', r, v, ptype, p) = execute db ops qk
                in  encodeResponse db' r v ptype p
    B.hPut stdout payload

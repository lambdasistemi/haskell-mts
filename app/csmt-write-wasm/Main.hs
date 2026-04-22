-- |
-- Module      : Main
-- Description : WASM entry point for the CSMT write path
-- Copyright   : (c) Paolo Veronelli, 2026
-- License     : Apache-2.0
--
-- Minimal stdio driver that cross-compiles the CSMT write side
-- (insert + root + inclusion proof) to @wasm32-wasi@. Together
-- with @csmt-verify.wasm@ this lets a browser host run the full
-- build-verify loop without a server: feed it a list of
-- key/value pairs, get back a root hash plus an inclusion proof
-- that the verifier can re-check against the same root.
--
-- Wire protocol on stdin (all length-prefixed fields are
-- big-endian 4-byte unsigned):
--
--   * @n@                         — number of inserts
--   * @n x (klen key vlen value)@ — pairs to insert in order
--   * @qlen queryKey@             — key to produce a proof for
--
-- Response on stdout:
--
--   * @root@       — 32-byte post-insert root hash
--   * @vlen value@ — the queried key's stored value
--   * @plen proof@ — CBOR-encoded inclusion proof
--
-- If the queried key is absent @vlen=0@ and @plen=0@; malformed
-- input produces an all-zero envelope. The process always exits
-- 0 — the browser host reads the payload shape, not the exit code.
module Main (main) where

import Control.Lens (Prism', prism')
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Maybe (fromMaybe)
import Data.Serialize
    ( Get
    , getByteString
    , getWord32be
    , putByteString
    , putWord32be
    , runGet
    , runPut
    )
import System.IO (stdin, stdout)

import CSMT.Backend.Pure
    ( emptyInMemoryDB
    , runPure
    , runPureTransaction
    )
import CSMT.Backend.Standalone
    ( Standalone (..)
    , StandaloneCodecs (..)
    )
import CSMT.Core.Hash (Hash, parseHash, renderHash)
import CSMT.Hashes
    ( fromKVHashes
    , generateInclusionProof
    , insert
    , root
    )

-- | Identity prism for raw byte payloads — the wire protocol
-- already speaks in bytes.
bytesCodec :: Prism' ByteString ByteString
bytesCodec = prism' id Just

-- | Prism between a 32-byte hash serialization and a 'Hash' node.
-- 'parseHash' rejects any other length.
hashCodec :: Prism' ByteString Hash
hashCodec = prism' renderHash parseHash

-- | Codec bundle used by the standalone backend: keys and values
-- are raw 'ByteString' payloads, hashes go through 'hashCodec'.
byteCodecs :: StandaloneCodecs ByteString ByteString Hash
byteCodecs =
    StandaloneCodecs
        { keyCodec = bytesCodec
        , valueCodec = bytesCodec
        , nodeCodec = hashCodec
        }

-- | Read a 4-byte big-endian length prefix and the payload.
getLenBytes :: Get ByteString
getLenBytes = do
    n <- fromIntegral <$> getWord32be
    getByteString n

-- | Parse the stdin envelope: /n/ pairs followed by a query key.
parseInput
    :: ByteString
    -> Either String ([(ByteString, ByteString)], ByteString)
parseInput = runGet $ do
    n <- fromIntegral <$> getWord32be
    kvs <-
        sequence
            [ (,) <$> getLenBytes <*> getLenBytes
            | _ <- [1 .. n :: Int]
            ]
    q <- getLenBytes
    pure (kvs, q)

-- | Encode the response: root || len+value || len+proof.
encodeResponse
    :: ByteString
    -- ^ root hash
    -> ByteString
    -- ^ looked-up value
    -> ByteString
    -- ^ inclusion proof
    -> ByteString
encodeResponse r v p = runPut $ do
    putByteString r
    putWord32be (fromIntegral (B.length v))
    putByteString v
    putWord32be (fromIntegral (B.length p))
    putByteString p

-- | Fallback envelope for malformed input: 32 zero bytes, empty
-- value, empty proof.
emptyResponse :: ByteString
emptyResponse = encodeResponse (B.replicate 32 0) B.empty B.empty

-- | Run the write path end-to-end: insert every pair, emit the
-- root hash, and build an inclusion proof for the query key.
execute
    :: [(ByteString, ByteString)]
    -> ByteString
    -> (ByteString, ByteString, ByteString)
execute kvs qk =
    let program = runPureTransaction byteCodecs $ do
            mapM_
                ( uncurry
                    ( insert
                        fromKVHashes
                        StandaloneKVCol
                        StandaloneCSMTCol
                    )
                )
                kvs
            r <- root StandaloneCSMTCol
            p <-
                generateInclusionProof
                    fromKVHashes
                    StandaloneKVCol
                    StandaloneCSMTCol
                    qk
            pure (r, p)
        ((rOut, pOut), _) = runPure emptyInMemoryDB program
        rootBs = fromMaybe (B.replicate 32 0) rOut
        (val, proof) = case pOut of
            Just (v, pbs) -> (v, pbs)
            Nothing -> (B.empty, B.empty)
    in  (rootBs, val, proof)

main :: IO ()
main = do
    raw <- B.hGetContents stdin
    let payload = case parseInput raw of
            Left _ -> emptyResponse
            Right (kvs, qk) ->
                let (r, v, p) = execute kvs qk
                in  encodeResponse r v p
    B.hPut stdout payload

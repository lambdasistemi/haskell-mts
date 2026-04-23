-- |
-- Module      : Main
-- Description : WASM entry point for the MPF write path
--
-- Stateless stdio driver for the pure MPF write path. It mirrors the
-- CSMT write-side envelope: the browser sends the previous in-memory
-- database blob, a batch of insert/delete ops, and a query key; the
-- process returns the updated blob, the post-mutation root, and either
-- an inclusion proof, an exclusion proof, or no proof when the tree is
-- empty.
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
import Database.KV.Transaction (query)
import MPF.Backend.Pure
    ( MPFInMemoryDB (..)
    , emptyMPFInMemoryDB
    , runMPFPure
    , runMPFPureTransaction
    )
import MPF.Backend.Standalone
    ( MPFStandalone (..)
    , MPFStandaloneCodecs (..)
    )
import MPF.Deletion (deleting)
import MPF.Hashes
    ( MPFHash
    , fromHexKVHashes
    , mpfHashing
    , parseMPFHash
    , renderMPFHash
    , root
    )
import MPF.Hashes.Aiken (renderAikenProof)
import MPF.Insertion (inserting)
import MPF.Proof.Exclusion
    ( MPFExclusionProof (..)
    , mkMPFExclusionProof
    , mpfExclusionProofSteps
    )
import MPF.Proof.Insertion (MPFProof (..), mkMPFInclusionProof)
import System.IO (stdin, stdout)

data Op
    = OpInsert ByteString ByteString
    | OpDelete ByteString

bytesCodec :: Prism' ByteString ByteString
bytesCodec = prism' id Just

hashCodec :: Prism' ByteString MPFHash
hashCodec = prism' renderMPFHash parseMPFHash

byteCodecs :: MPFStandaloneCodecs ByteString ByteString MPFHash
byteCodecs =
    MPFStandaloneCodecs
        { mpfKeyCodec = bytesCodec
        , mpfValueCodec = bytesCodec
        , mpfNodeCodec = hashCodec
        }

getLenBytes :: Get ByteString
getLenBytes = do
    n <- fromIntegral <$> getWord32be
    getByteString n

putLenBytes :: ByteString -> Put
putLenBytes bs = do
    putWord32be (fromIntegral (B.length bs))
    putByteString bs

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

putDB :: MPFInMemoryDB -> Put
putDB db = do
    putMapBs (mpfInMemoryMPF db)
    putMapBs (mpfInMemoryKV db)
    putMapBs (mpfInMemoryJournal db)
    putMapBs (mpfInMemoryMetrics db)

getDB :: Get MPFInMemoryDB
getDB = do
    mpf <- getMapBs
    kv <- getMapBs
    journal <- getMapBs
    metrics <- getMapBs
    pure
        MPFInMemoryDB
            { mpfInMemoryMPF = mpf
            , mpfInMemoryKV = kv
            , mpfInMemoryJournal = journal
            , mpfInMemoryMetrics = metrics
            , mpfInMemoryIterators = Map.empty
            }

putDBBlob :: MPFInMemoryDB -> Put
putDBBlob db = putLenBytes (runPut (putDB db))

getOp :: Get Op
getOp = do
    opcode <- getWord8
    case opcode of
        0 -> OpInsert <$> getLenBytes <*> getLenBytes
        1 -> OpDelete <$> getLenBytes
        _ -> fail "unknown op opcode"

parseInput
    :: ByteString
    -> Either String (MPFInMemoryDB, [Op], ByteString)
parseInput = runGet $ do
    stateBlob <- getLenBytes
    db <- case B.null stateBlob of
        True -> pure emptyMPFInMemoryDB
        False -> case runGet getDB stateBlob of
            Left err -> fail err
            Right d -> pure d
    n <- fromIntegral <$> getWord32be
    ops <- sequence [getOp | _ <- [1 .. n :: Int]]
    q <- getLenBytes
    pure (db, ops, q)

ptInclusion, ptExclusion, ptNone :: Word8
ptInclusion = 0
ptExclusion = 1
ptNone = 0xff

encodeResponse
    :: MPFInMemoryDB
    -> ByteString
    -> ByteString
    -> Word8
    -> ByteString
    -> ByteString
encodeResponse db r v ptype p = runPut $ do
    putDBBlob db
    putByteString r
    putLenBytes v
    putWord8 ptype
    putLenBytes p

emptyResponse :: ByteString
emptyResponse =
    encodeResponse
        emptyMPFInMemoryDB
        (B.replicate 32 0)
        B.empty
        ptNone
        B.empty

execute
    :: MPFInMemoryDB
    -> [Op]
    -> ByteString
    -> (MPFInMemoryDB, ByteString, ByteString, Word8, ByteString)
execute db0 ops qk =
    let runOp (OpInsert k v) =
            inserting
                []
                fromHexKVHashes
                mpfHashing
                MPFStandaloneKVCol
                MPFStandaloneMPFCol
                k
                v
        runOp (OpDelete k) =
            deleting
                []
                fromHexKVHashes
                mpfHashing
                MPFStandaloneKVCol
                MPFStandaloneMPFCol
                k
        program = runMPFPureTransaction byteCodecs $ do
            mapM_ runOp ops
            r <- root MPFStandaloneMPFCol []
            v <- query MPFStandaloneKVCol qk
            pIncl <-
                mkMPFInclusionProof
                    []
                    fromHexKVHashes
                    mpfHashing
                    MPFStandaloneMPFCol
                    qk
            pExcl <- case pIncl of
                Just _ -> pure Nothing
                Nothing ->
                    mkMPFExclusionProof
                        []
                        fromHexKVHashes
                        mpfHashing
                        MPFStandaloneMPFCol
                        qk
            pure (r, v, pIncl, pExcl)
        ((rOut, valueOut, inclOut, exclOut), db1) = runMPFPure db0 program
        rootBs = fromMaybe (B.replicate 32 0) rOut
        chooseProof = case rOut of
            Nothing -> (B.empty, ptNone, B.empty)
            Just _ -> case inclOut of
                Just proof ->
                    ( fromMaybe B.empty valueOut
                    , ptInclusion
                    , renderAikenProof (mpfProofSteps proof)
                    )
                Nothing -> case exclOut of
                    Just proof@MPFExclusionWitness{} ->
                        ( B.empty
                        , ptExclusion
                        , renderAikenProof (mpfExclusionProofSteps proof)
                        )
                    _ -> (B.empty, ptNone, B.empty)
        (valueBs, ptype, proofBs) = chooseProof
    in  (db1, rootBs, valueBs, ptype, proofBs)

main :: IO ()
main = do
    raw <- B.hGetContents stdin
    let payload = case parseInput raw of
            Left _ -> emptyResponse
            Right (db, ops, qk) ->
                let (db', r, v, ptype, proofBs) = execute db ops qk
                in  encodeResponse db' r v ptype proofBs
    B.hPut stdout payload

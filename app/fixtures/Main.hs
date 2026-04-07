-- | Generate JSON test fixtures for cross-language testing.
--
-- Outputs inclusion and exclusion proof fixtures as JSON,
-- consumed by the TypeScript verifier tests.
module Main (main) where

import CSMT.Backend.Pure
    ( Pure
    , emptyInMemoryDB
    , pureDatabase
    , runPure
    )
import CSMT.Backend.Standalone
    ( Standalone (..)
    , StandaloneCodecs (..)
    )
import CSMT.Hashes
    ( Hash
    , byteStringToKey
    , fromKVHashes
    , hashHashing
    , isoHash
    , renderHash
    )
import CSMT.Hashes.CBOR
    ( renderExclusionProof
    , renderProof
    )
import CSMT.Insertion (inserting)
import CSMT.Interface qualified as I
import CSMT.Proof.Exclusion
    ( ExclusionProof
    , buildExclusionProof
    )
import CSMT.Proof.Insertion
    ( InclusionProof
    , buildInclusionProof
    )
import Control.Lens (simple)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C8
import Data.Word (Word8)
import Database.KV.Transaction
    ( runTransactionUnguarded
    )
import Numeric (showHex)

-- -----------------------------------------------------------
-- Pure backend helpers
-- -----------------------------------------------------------

bsCodecs :: StandaloneCodecs ByteString ByteString Hash
bsCodecs =
    StandaloneCodecs
        { keyCodec = simple
        , valueCodec = simple
        , nodeCodec = isoHash
        }

type PureM = Pure

insertKV :: ByteString -> ByteString -> PureM ()
insertKV k v =
    runTransactionUnguarded (pureDatabase bsCodecs)
        $ inserting
            []
            fromKVHashes
            hashHashing
            StandaloneKVCol
            StandaloneCSMTCol
            k
            v

getInclusionProof
    :: ByteString
    -> PureM (Maybe (ByteString, InclusionProof Hash))
getInclusionProof k =
    runTransactionUnguarded (pureDatabase bsCodecs)
        $ buildInclusionProof
            []
            fromKVHashes
            StandaloneKVCol
            StandaloneCSMTCol
            k

getExclusionProof
    :: ByteString
    -> PureM (Maybe (ExclusionProof Hash))
getExclusionProof k =
    runTransactionUnguarded (pureDatabase bsCodecs)
        $ buildExclusionProof
            []
            StandaloneCSMTCol
            (byteStringToKey k)

getRootHash :: PureM (Maybe Hash)
getRootHash =
    runTransactionUnguarded (pureDatabase bsCodecs)
        $ I.root hashHashing StandaloneCSMTCol []

-- -----------------------------------------------------------
-- JSON helpers
-- -----------------------------------------------------------

hex :: ByteString -> String
hex = concatMap byteToHex . B.unpack
  where
    byteToHex :: Word8 -> String
    byteToHex w =
        let s = showHex w ""
        in  if length s == 1 then '0' : s else s

jsonStr :: String -> String
jsonStr s = "\"" ++ s ++ "\""

jsonObj :: [(String, String)] -> String
jsonObj fields =
    "{"
        ++ intercalate'
            ","
            [ jsonStr k ++ ":" ++ v
            | (k, v) <- fields
            ]
        ++ "}"

jsonArr :: [String] -> String
jsonArr items = "[" ++ intercalate' "," items ++ "]"

intercalate' :: String -> [String] -> String
intercalate' _ [] = ""
intercalate' _ [x] = x
intercalate' sep (x : xs) = x ++ sep ++ intercalate' sep xs

-- -----------------------------------------------------------
-- Main
-- -----------------------------------------------------------

main :: IO ()
main = do
    let kvs =
            [ ("hello", "world")
            , ("foo", "bar")
            , ("test", "data")
            ]
        absentKeys =
            [ "missing"
            , "absent"
            , "nope"
            ]
        ( (inclusionFixtures, exclusionFixtures, rootH)
            , _
            ) =
                runPure emptyInMemoryDB $ do
                    mapM_ (uncurry insertKV) kvs
                    incls <- mapM mkInclusion kvs
                    excls <- mapM mkExclusion absentKeys
                    mr <- getRootHash
                    pure (incls, excls, mr)

    let rootHashHex = case rootH of
            Just h -> hex (renderHash h)
            Nothing -> ""

    putStrLn
        $ jsonObj
            [
                ( "description"
                , jsonStr
                    "Test fixtures generated from mts"
                )
            , ("rootHash", jsonStr rootHashHex)
            , ("proofs", jsonArr inclusionFixtures)
            ,
                ( "exclusionProofs"
                , jsonArr exclusionFixtures
                )
            ]
  where
    mkInclusion (k, v) = do
        mp <- getInclusionProof k
        case mp of
            Just (_, proof) ->
                pure
                    $ jsonObj
                        [ ("key", jsonStr (C8.unpack k))
                        , ("value", jsonStr (C8.unpack v))
                        , ("cbor", jsonStr (hex (renderProof proof)))
                        ]
            Nothing ->
                error
                    $ "inclusion proof failed: "
                        ++ C8.unpack k

    mkExclusion k = do
        mp <- getExclusionProof k
        case mp of
            Just proof ->
                pure
                    $ jsonObj
                        [
                            ( "targetKey"
                            , jsonStr (C8.unpack k)
                            )
                        ,
                            ( "cbor"
                            , jsonStr
                                ( hex
                                    ( renderExclusionProof
                                        proof
                                    )
                                )
                            )
                        ]
            Nothing ->
                error
                    $ "exclusion proof failed: "
                        ++ C8.unpack k

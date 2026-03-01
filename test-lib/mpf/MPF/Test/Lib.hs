{-# LANGUAGE OverloadedStrings #-}

module MPF.Test.Lib
    ( -- * Test Utilities
      insertMPF
    , deleteMPF
    , insertMPFM
    , deleteMPFM
    , insertByteStringM
    , insertBatchMPFM
    , insertChunkedMPFM
    , insertStreamMPFM
    , getRootHashM
    , evalMPFPure'
    , runMPFPure'
    , mpfHashCodecs
    , fromHexKVIdentity
    , fromHexKVByteString

      -- * Pure Backend (for benchmarking)
    , MPFInMemoryDB
    , MPFPure
    , emptyMPFInMemoryDB
    , runMPFPure

      -- * Proof Utilities
    , proofMPFM
    , verifyMPFM
    , MPFProof
    , MPFProofStep (..)
    , foldMPFProof

      -- * Test Vectors
    , fruitsTestData
    , expectedFullTrieRoot
    , decodeHex
    , encodeHex
    )
where

import Control.Lens (Iso', iso)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8)
import Database.KV.Transaction (query, runTransactionUnguarded)
import MPF.Backend.Pure
    ( MPFInMemoryDB
    , MPFPure
    , emptyMPFInMemoryDB
    , mpfPureDatabase
    , runMPFPure
    )
import MPF.Backend.Standalone
    ( MPFStandalone (..)
    , MPFStandaloneCodecs (..)
    )
import MPF.Deletion (deleting)
import MPF.Hashes
    ( MPFHash
    , MPFHashing (..)
    , mkMPFHash
    , mpfHashing
    , parseMPFHash
    , renderMPFHash
    )
import MPF.Insertion (inserting, insertingBatch, insertingChunked, insertingStream)
import MPF.Interface
    ( FromHexKV (..)
    , HexIndirect (..)
    , HexKey
    , byteStringToHexKey
    , hexKeyPrism
    )
import MPF.Proof.Insertion
    ( MPFProof
    , MPFProofStep (..)
    , foldMPFProof
    , mkMPFInclusionProof
    , verifyMPFInclusionProof
    )

-- | Default codecs for testing with MPFHash
mpfHashCodecs :: MPFStandaloneCodecs HexKey MPFHash MPFHash
mpfHashCodecs =
    MPFStandaloneCodecs
        { mpfKeyCodec = hexKeyPrism
        , mpfValueCodec = isoMPFHash
        , mpfNodeCodec = isoMPFHash
        }

-- | ISO for MPFHash to ByteString
isoMPFHash :: Iso' ByteString MPFHash
isoMPFHash = iso parseMPFHashUnsafe renderMPFHash
  where
    parseMPFHashUnsafe bs = case parseMPFHash bs of
        Just h -> h
        Nothing -> mkMPFHash bs  -- hash it if not 32 bytes

-- | Identity FromHexKV for testing with HexKey and MPFHash
fromHexKVIdentity :: FromHexKV HexKey MPFHash MPFHash
fromHexKVIdentity = FromHexKV{fromHexK = id, fromHexV = id}

-- | FromHexKV for ByteString keys and values
fromHexKVByteString :: FromHexKV ByteString ByteString MPFHash
fromHexKVByteString = FromHexKV{fromHexK = byteStringToHexKey, fromHexV = mkMPFHash}

-- | Insert a key-value pair into an MPF database
insertMPF
    :: MPFInMemoryDB
    -> HexKey
    -> MPFHash
    -> MPFInMemoryDB
insertMPF db k v =
    snd $ runMPFPure db $ insertMPFM k v

-- | Delete a key from an MPF database
deleteMPF
    :: MPFInMemoryDB
    -> HexKey
    -> MPFInMemoryDB
deleteMPF db k =
    snd $ runMPFPure db $ deleteMPFM k

-- | Insert in the Pure monad
insertMPFM :: HexKey -> MPFHash -> MPFPure ()
insertMPFM k v =
    runTransactionUnguarded (mpfPureDatabase mpfHashCodecs)
        $ inserting fromHexKVIdentity mpfHashing MPFStandaloneKVCol MPFStandaloneMPFCol k v

-- | Delete in the Pure monad
deleteMPFM :: HexKey -> MPFPure ()
deleteMPFM k =
    runTransactionUnguarded (mpfPureDatabase mpfHashCodecs)
        $ deleting fromHexKVIdentity mpfHashing MPFStandaloneKVCol MPFStandaloneMPFCol k

-- | Insert a ByteString key-value pair in the Pure monad
-- Hashes both key and value, then uses hash of key as the trie path (Aiken compatible)
insertByteStringM :: ByteString -> ByteString -> MPFPure ()
insertByteStringM k v =
    insertMPFM (byteStringToHexKey $ renderMPFHash $ mkMPFHash k) (mkMPFHash v)

-- | Batch insert multiple key-value pairs in the Pure monad
-- Much faster than sequential inserts - O(n log n) vs O(n²)
insertBatchMPFM :: [(HexKey, MPFHash)] -> MPFPure ()
insertBatchMPFM kvs =
    runTransactionUnguarded (mpfPureDatabase mpfHashCodecs)
        $ insertingBatch fromHexKVIdentity mpfHashing MPFStandaloneKVCol MPFStandaloneMPFCol kvs

-- | Chunked insert for large datasets in the Pure monad
-- Processes items in chunks to bound memory usage
insertChunkedMPFM :: Int -> [(HexKey, MPFHash)] -> MPFPure Int
insertChunkedMPFM chunkSize kvs =
    runTransactionUnguarded (mpfPureDatabase mpfHashCodecs)
        $ insertingChunked fromHexKVIdentity mpfHashing MPFStandaloneKVCol MPFStandaloneMPFCol chunkSize kvs

-- | Streaming insert for very large datasets in the Pure monad
-- Groups by first hex digit to reduce peak memory by ~16x
insertStreamMPFM :: [(HexKey, MPFHash)] -> MPFPure ()
insertStreamMPFM kvs =
    runTransactionUnguarded (mpfPureDatabase mpfHashCodecs)
        $ insertingStream fromHexKVIdentity mpfHashing MPFStandaloneKVCol MPFStandaloneMPFCol kvs

-- | Generate a membership proof for a key in the Pure monad
proofMPFM :: HexKey -> MPFPure (Maybe (MPFProof MPFHash))
proofMPFM k =
    runTransactionUnguarded (mpfPureDatabase mpfHashCodecs)
        $ mkMPFInclusionProof fromHexKVIdentity mpfHashing MPFStandaloneMPFCol k

-- | Verify a membership proof for a key-value pair in the Pure monad
verifyMPFM :: HexKey -> MPFHash -> MPFPure Bool
verifyMPFM k v =
    runTransactionUnguarded (mpfPureDatabase mpfHashCodecs) $ do
        mProof <- mkMPFInclusionProof fromHexKVIdentity mpfHashing MPFStandaloneMPFCol k
        case mProof of
            Nothing -> pure False
            Just proof ->
                verifyMPFInclusionProof fromHexKVIdentity MPFStandaloneMPFCol mpfHashing v proof

-- | Get the root hash from the MPF trie
getRootHashM :: MPFPure (Maybe MPFHash)
getRootHashM =
    runTransactionUnguarded (mpfPureDatabase mpfHashCodecs) $ do
        mi <- query MPFStandaloneMPFCol []
        pure $ case mi of
            Nothing -> Nothing
            Just i -> Just $
                if hexIsLeaf i
                    then leafHash mpfHashing (hexJump i) (hexValue i)  -- Leaf: compute from value hash
                    else hexValue i  -- Branch: already has branch hash

-- | Evaluate a pure MPF computation from empty database
evalMPFPure' :: MPFPure a -> a
evalMPFPure' p = fst $ runMPFPure emptyMPFInMemoryDB p

-- | Run a pure MPF computation from empty database
runMPFPure' :: MPFPure a -> (a, MPFInMemoryDB)
runMPFPure' = runMPFPure emptyMPFInMemoryDB

-- | Test vectors from aiken-lang implementation
-- 30 fruit key-value pairs (must match exactly for compatible root hash)
fruitsTestData :: [(ByteString, ByteString)]
fruitsTestData =
    [ ("apple[uid: 58]", encodeUtf8 "🍎")
    , ("apricot[uid: 0]", encodeUtf8 "🤷")
    , ("banana[uid: 218]", encodeUtf8 "🍌")
    , ("blueberry[uid: 0]", encodeUtf8 "🫐")
    , ("cherry[uid: 0]", encodeUtf8 "🍒")
    , ("coconut[uid: 0]", encodeUtf8 "🥥")
    , ("cranberry[uid: 0]", encodeUtf8 "🤷")
    , ("fig[uid: 68267]", encodeUtf8 "🤷")
    , ("grapefruit[uid: 0]", encodeUtf8 "🤷")
    , ("grapes[uid: 0]", encodeUtf8 "🍇")
    , ("guava[uid: 344]", encodeUtf8 "🤷")
    , ("kiwi[uid: 0]", encodeUtf8 "🥝")
    , ("kumquat[uid: 0]", encodeUtf8 "🤷")
    , ("lemon[uid: 0]", encodeUtf8 "🍋")
    , ("lime[uid: 0]", encodeUtf8 "🤷")
    , ("mango[uid: 0]", encodeUtf8 "🥭")
    , ("orange[uid: 0]", encodeUtf8 "🍊")
    , ("papaya[uid: 0]", encodeUtf8 "🤷")
    , ("passionfruit[uid: 0]", encodeUtf8 "🤷")
    , ("peach[uid: 0]", encodeUtf8 "🍑")
    , ("pear[uid: 0]", encodeUtf8 "🍐")
    , ("pineapple[uid: 12577]", encodeUtf8 "🍍")
    , ("plum[uid: 15492]", encodeUtf8 "🤷")
    , ("pomegranate[uid: 0]", encodeUtf8 "🤷")
    , ("raspberry[uid: 0]", encodeUtf8 "🤷")
    , ("strawberry[uid: 2532]", encodeUtf8 "🍓")
    , ("tangerine[uid: 11]", encodeUtf8 "🍊")
    , ("tomato[uid: 83468]", encodeUtf8 "🍅")
    , ("watermelon[uid: 0]", encodeUtf8 "🍉")
    , ("yuzu[uid: 0]", encodeUtf8 "🤷")
    ]

-- | Expected root hash after inserting all fruits
expectedFullTrieRoot :: ByteString
expectedFullTrieRoot =
    decodeHex "4acd78f345a686361df77541b2e0b533f53362e36620a1fdd3a13e0b61a3b078"

-- | Decode a hex string to ByteString
decodeHex :: ByteString -> ByteString
decodeHex hex = B.pack $ pairBytes $ BC.unpack hex
  where
    pairBytes :: String -> [Word8]
    pairBytes [] = []
    pairBytes [_] = error "decodeHex: odd length hex string"
    pairBytes (a:b:rest) = hexDigit a * 16 + hexDigit b : pairBytes rest

    hexDigit :: Char -> Word8
    hexDigit c
        | c >= '0' && c <= '9' = fromIntegral (fromEnum c - fromEnum '0')
        | c >= 'a' && c <= 'f' = fromIntegral (fromEnum c - fromEnum 'a' + 10)
        | c >= 'A' && c <= 'F' = fromIntegral (fromEnum c - fromEnum 'A' + 10)
        | otherwise = error $ "decodeHex: invalid hex digit: " ++ [c]

-- | Encode a ByteString to hex
encodeHex :: ByteString -> ByteString
encodeHex = BC.pack . concatMap toHex . B.unpack
  where
    toHex :: Word8 -> String
    toHex w = [hexChar (w `div` 16), hexChar (w `mod` 16)]

    hexChar :: Word8 -> Char
    hexChar n
        | n < 10 = toEnum (fromIntegral n + fromEnum '0')
        | otherwise = toEnum (fromIntegral n - 10 + fromEnum 'a')

{-# LANGUAGE StrictData #-}

-- |
-- Module      : CSMT.Verify.Hash
-- Description : Blake2b-256 hash type for CSMT proof verification
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- The concrete 32-byte 'Hash' type plus the 'Hashing' record used by
-- CSMT proof verification. Mirrors @CSMT.Hashes@ on the write side,
-- but reaches the same byte-for-byte hash outputs through the
-- pure-Haskell Blake2b-256 implementation in 'CSMT.Verify.Blake2b'.
-- The sublibrary therefore has no C FFI, which is what lets it
-- cross-compile to WASM without the crypton/memory recipe.
module CSMT.Verify.Hash
    ( Hash (..)
    , renderHash
    , mkHash
    , parseHash
    , hashHashing
    , keyToHash
    , byteStringToKey
    , keyToByteString
    ) where

import Data.Bits (Bits (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Serialize (PutM, runPutM)
import Data.Word (Word8)

import CSMT.Verify.Blake2b (blake2b256)
import CSMT.Verify.Core
    ( Direction (..)
    , Hashing (..)
    , Key
    , putIndirect
    , putKey
    )

-- | A 32-byte Blake2b-256 hash value.
newtype Hash = Hash ByteString
    deriving (Eq, Ord)

instance Show Hash where
    show (Hash h) = "Hash " <> hexEncode h

-- | Extract the raw 'ByteString' from a 'Hash'.
renderHash :: Hash -> ByteString
renderHash (Hash h) = h

-- | Parse a 32-byte 'ByteString' as a 'Hash'. Returns 'Nothing' if
-- the length is not exactly 32 bytes.
parseHash :: ByteString -> Maybe Hash
parseHash bs
    | B.length bs == 32 = Just (Hash bs)
    | otherwise = Nothing

-- | Compute a Blake2b-256 hash of a 'ByteString'.
mkHash :: ByteString -> Hash
mkHash = Hash . blake2b256

runPut :: PutM a -> ByteString
runPut p = snd (runPutM p)

-- | Hashing functions for verifying a CSMT proof with Blake2b-256.
-- 'rootHash' hashes a single serialized indirect value; 'combineHash'
-- concatenates two serialized children and rehashes. Matches
-- @CSMT.Hashes.hashHashing@ on the write side.
hashHashing :: Hashing Hash
hashHashing =
    Hashing
        { rootHash = mkHash . runPut . putIndirect . fmap renderHash
        , combineHash = \left right -> mkHash . runPut $ do
            putIndirect (fmap renderHash left)
            putIndirect (fmap renderHash right)
        }

-- | Convert a 'Key' to its hash representation.
keyToHash :: Key -> Hash
keyToHash = mkHash . runPut . putKey

-- | Expand a 'ByteString' to a 'Key' (one 'Direction' per bit, MSB
-- first). Matches @CSMT.Hashes.byteStringToKey@.
byteStringToKey :: ByteString -> Key
byteStringToKey bs = concatMap byteToDirections (B.unpack bs)
  where
    byteToDirections :: Word8 -> Key
    byteToDirections byte =
        [if testBit byte i then R else L | i <- [7, 6 .. 0]]

-- | Invert 'byteStringToKey': groups every 8 directions into a byte,
-- MSB first.
keyToByteString :: Key -> ByteString
keyToByteString = B.pack . go
  where
    go [] = []
    go ds =
        let (byte, rest) = splitAt 8 ds
            toByte =
                foldl
                    ( \acc (i, d) -> case d of
                        R -> setBit acc i
                        L -> acc
                    )
                    (0 :: Word8)
                    (zip [7, 6 .. 0] byte)
        in  toByte : go rest

hexEncode :: ByteString -> String
hexEncode = concatMap byteHex . B.unpack
  where
    byteHex :: Word8 -> String
    byteHex b =
        [ hex (fromIntegral (b `shiftR` 4))
        , hex (fromIntegral (b .&. 0x0f))
        ]

    hex :: Int -> Char
    hex n
        | n < 10 = toEnum (fromEnum '0' + n)
        | otherwise = toEnum (fromEnum 'a' + n - 10)

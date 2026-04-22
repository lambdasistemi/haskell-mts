{-# LANGUAGE StrictData #-}

-- |
-- Module      : CSMT.Core.Hash
-- Description : Backend-agnostic 32-byte hash wrapper and Hashing helpers
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- The pure, backend-free parts of the CSMT hash layer: the concrete
-- 32-byte 'Hash' newtype, its serialization, and a 'hashingWith'
-- constructor that wires any @ByteString -> Hash@ function into a
-- 'Hashing' 'Hash' record. Concrete backends (pure Blake2b in
-- @csmt-verify@, @crypton@-backed Blake2b in @csmt@) supply their
-- own @mkHash@ and delegate to 'hashingWith'.
module CSMT.Core.Hash
    ( -- * Hash value
      Hash (..)
    , renderHash
    , parseHash

      -- * Backend-agnostic Hashing construction
    , hashingWith
    , keyToHashWith

      -- * Key / byte-string conversions
    , byteStringToKey
    , keyToByteString
    ) where

import Data.Bits (Bits (..), shiftR, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Serialize (PutM, runPutM)
import Data.Word (Word8)

import CSMT.Core.Types
    ( Direction (..)
    , Hashing (..)
    , Key
    , putIndirect
    , putKey
    )

-- | A 32-byte hash value. The hashing algorithm is whatever the
-- caller's backend uses when it builds the 'Hashing' record — for
-- Cardano-aligned CSMTs this is Blake2b-256.
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

runPut :: PutM a -> ByteString
runPut p = snd (runPutM p)

-- | Build a 'Hashing' 'Hash' record from a caller-supplied
-- @ByteString -> Hash@ function. 'rootHash' hashes a single
-- serialized 'Indirect' value; 'combineHash' concatenates two
-- serialized children and rehashes. Both sides of the codebase —
-- the @csmt@ write side and the @csmt-verify@ WASM-safe verifier —
-- agree bit-for-bit on the resulting hashes as long as they supply
-- the same underlying hash function.
hashingWith :: (ByteString -> Hash) -> Hashing Hash
hashingWith mkHash =
    Hashing
        { rootHash = mkHash . runPut . putIndirect . fmap renderHash
        , combineHash = \left right -> mkHash . runPut $ do
            putIndirect (fmap renderHash left)
            putIndirect (fmap renderHash right)
        }

-- | Hash a serialized key under the caller-supplied hash function.
keyToHashWith :: (ByteString -> Hash) -> Key -> Hash
keyToHashWith mkHash = mkHash . runPut . putKey

-- | Expand a 'ByteString' to a 'Key' (one 'Direction' per bit, MSB
-- first).
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

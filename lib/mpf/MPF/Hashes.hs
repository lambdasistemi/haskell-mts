{-# LANGUAGE StrictData #-}

module MPF.Hashes
    ( -- * Hash Type
      MPFHash (..)
    , mkMPFHash
    , renderMPFHash
    , parseMPFHash
    , nullHash

      -- * Hashing Abstraction
    , MPFHashing (..)
    , mpfHashing

      -- * Hash Operations
    , computeLeafHash
    , computeMerkleRoot
    , fromHexKVHashes
    , byteStringToHexKey'
    )
where

import Crypto.Hash (Blake2b_256, hash)
import Data.ByteArray (ByteArray, ByteArrayAccess, convert)
import Data.ByteArray.Encoding (Base (Base64), convertToBase)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.Word (Word8)
import MPF.Interface
    ( FromHexKV (..)
    , HexDigit (..)
    , HexKey
    , byteStringToHexKey
    )

-- | MPF Hash value (32 bytes Blake2b-256)
newtype MPFHash = MPFHash ByteString
    deriving (Eq, Ord, Semigroup, Monoid, ByteArrayAccess, ByteArray)

instance Show MPFHash where
    show (MPFHash h) = BC.unpack $ "MPFHash " <> convertToBase Base64 h

-- | Create a hash from arbitrary data using Blake2b-256
mkMPFHash :: ByteString -> MPFHash
mkMPFHash = MPFHash . convert . hash @ByteString @Blake2b_256

-- | Extract the raw bytes of a hash
renderMPFHash :: MPFHash -> ByteString
renderMPFHash (MPFHash h) = h

-- | Parse a hash from raw bytes (must be exactly 32 bytes)
parseMPFHash :: ByteString -> Maybe MPFHash
parseMPFHash bs
    | B.length bs == 32 = Just (MPFHash bs)
    | otherwise = Nothing

-- | A 32-byte null hash (all zeros)
nullHash :: MPFHash
nullHash = MPFHash $ B.replicate 32 0

-- | MPF-specific hashing operations
data MPFHashing a = MPFHashing
    { leafHash :: HexKey -> a -> a
    -- ^ Hash a leaf node given its suffix and value hash
    , merkleRoot :: [Maybe a] -> a
    -- ^ Compute merkle root of branch children (sparse 16-element array)
    , branchHash :: HexKey -> a -> a
    -- ^ Hash a branch node given its prefix and children's merkle root
    }

-- | Pack a HexKey into bytes (2 nibbles per byte)
-- This is the "hashTail" format used in leaf hashing
packHexKey :: HexKey -> ByteString
packHexKey = B.pack . pairNibbles
  where
    pairNibbles :: [HexDigit] -> [Word8]
    pairNibbles [] = []
    pairNibbles [HexDigit lo] = [lo] -- single nibble
    pairNibbles (HexDigit hi : HexDigit lo : rest) =
        ((hi * 16) + lo) : pairNibbles rest

-- | Encode a HexKey as one byte per nibble
-- This is the "nibbles" format used in branch hashing (Aiken compatibility)
nibbleBytes :: HexKey -> ByteString
nibbleBytes = B.pack . map unHexDigit

-- | Compute leaf hash following MPF specification:
-- For even suffix length: hashHead = 0xff, hashTail = all digits
-- For odd suffix length: hashHead = 0x00 || first digit, hashTail = remaining digits
-- hash = digest(hashHead || hashTail || valueDigest)
computeLeafHash :: HexKey -> MPFHash -> MPFHash
computeLeafHash suffix valueDigest =
    mkMPFHash $ hashHead <> hashTail <> renderMPFHash valueDigest
  where
    (hashHead, hashTail) = case suffix of
        [] -> (B.singleton 0xff, B.empty)
        (HexDigit d : ds)
            | even (length suffix) ->
                (B.singleton 0xff, packHexKey suffix)
            | otherwise ->
                (B.pack [0x00, d], packHexKey ds)

-- | Compute merkle root of a sparse 16-element array
-- Uses pairwise hashing to reduce to single value
-- Missing children are represented as nullHash
computeMerkleRoot :: [Maybe MPFHash] -> MPFHash
computeMerkleRoot children =
    let
        -- Ensure exactly 16 elements, pad with Nothing
        padded = take 16 $ children ++ repeat Nothing
        -- Convert Nothing to nullHash
        hashes = map (maybe nullHash id) padded
    in
        pairwiseReduce hashes

-- | Reduce list by pairwise hashing until single element remains
pairwiseReduce :: [MPFHash] -> MPFHash
pairwiseReduce [] = nullHash
pairwiseReduce [h] = h
pairwiseReduce hs = pairwiseReduce $ pairUp hs
  where
    pairUp [] = []
    pairUp [h] = [h]
    pairUp (a : b : rest) =
        mkMPFHash (renderMPFHash a <> renderMPFHash b) : pairUp rest

-- | The standard MPF hashing implementation using Blake2b-256
mpfHashing :: MPFHashing MPFHash
mpfHashing =
    MPFHashing
        { leafHash = computeLeafHash
        , merkleRoot = computeMerkleRoot
        , branchHash = \prefix merkle ->
            mkMPFHash $ nibbleBytes prefix <> renderMPFHash merkle
        }

-- | Default FromHexKV for ByteString keys and values
fromHexKVHashes :: FromHexKV ByteString ByteString MPFHash
fromHexKVHashes =
    FromHexKV
        { fromHexK = byteStringToHexKey
        , fromHexV = mkMPFHash
        }

-- | Alias for byteStringToHexKey for convenient import
byteStringToHexKey' :: ByteString -> HexKey
byteStringToHexKey' = byteStringToHexKey

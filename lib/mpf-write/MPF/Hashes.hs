{-# LANGUAGE StrictData #-}

module MPF.Hashes
    ( -- * Hash Type
      MPFHash (..)
    , mkMPFHash
    , renderMPFHash
    , parseMPFHash
    , nullHash
    , isoMPFHash

      -- * Hashing Abstraction
    , MPFHashing (..)
    , mpfHashing

      -- * Hash Operations
    , computeLeafHash
    , computeMerkleRoot
    , aikenKeyPath
    , fromHexKVHashes
    , fromHexKVAikenHashes
    , byteStringToHexKey'

      -- * Key Encoding
    , packHexKey
    , nibbleBytes

      -- * Merkle Proof
    , merkleProof

      -- * Root Hash
    , root
    )
where

import CSMT.Verify.Blake2b (blake2b256)
import Control.Lens (Iso', iso)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Database.KV.Transaction
    ( GCompare
    , Selector
    , Transaction
    , query
    )
import MPF.Hashes.Types (MPFHash (..), renderMPFHash)
import MPF.Interface
    ( FromHexKV (..)
    , HexDigit (..)
    , HexIndirect (..)
    , HexKey
    , byteStringToHexKey
    )

-- | Create a hash from arbitrary data using Blake2b-256
mkMPFHash :: ByteString -> MPFHash
mkMPFHash = MPFHash . blake2b256

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
        hashes = map (fromMaybe nullHash) padded
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
        , hexTreePrefix = const []
        }

-- | Derive the Aiken-compatible trie path for a raw key.
--
-- Upstream Aiken routes keys by @blake2b_256(key)@ rendered as 64 hex
-- nibbles, not by the raw UTF-8 bytes directly.
aikenKeyPath :: ByteString -> HexKey
aikenKeyPath = byteStringToHexKey . renderMPFHash . mkMPFHash

-- | Aiken-compatible FromHexKV for raw ByteString keys and values.
--
-- The key remains unmodified in the KV column, but the MPF tree path is
-- derived from 'aikenKeyPath' so generated proofs verify against the
-- Aiken proof format.
fromHexKVAikenHashes :: FromHexKV ByteString ByteString MPFHash
fromHexKVAikenHashes =
    FromHexKV
        { fromHexK = aikenKeyPath
        , fromHexV = mkMPFHash
        , hexTreePrefix = const []
        }

-- | Alias for byteStringToHexKey for convenient import
byteStringToHexKey' :: ByteString -> HexKey
byteStringToHexKey' = byteStringToHexKey

-- | SMT proof: 4 intermediate hashes that, together with the
-- element at position @me@, reconstruct the merkle root.
--
-- Binary search through the 16-element pairwise reduction
-- tree, collecting the opposite subtree's root at each of
-- 4 levels (pivot starts at 8, halves each step).
merkleProof :: [Maybe MPFHash] -> Int -> [MPFHash]
merkleProof children me =
    let
        padded = take 16 $ children ++ repeat Nothing
        hashes = map (fromMaybe nullHash) padded
    in
        go hashes me 8 []
  where
    go _ _ 0 acc = reverse acc
    go hs pos pivot acc =
        let
            (left, right) = splitAt pivot hs
        in
            if pos < pivot
                then
                    go
                        left
                        pos
                        (pivot `div` 2)
                        (subtreeRoot right : acc)
                else
                    go
                        right
                        (pos - pivot)
                        (pivot `div` 2)
                        (subtreeRoot left : acc)

    subtreeRoot :: [MPFHash] -> MPFHash
    subtreeRoot = pairwiseReduce

-- | Get the root hash of the MPF tree, if it exists.
root
    :: (Monad m, GCompare d)
    => Selector d HexKey (HexIndirect MPFHash)
    -> HexKey
    -> Transaction m cf d ops (Maybe ByteString)
root sel prefix = do
    mi <- query sel prefix
    pure $ case mi of
        Nothing -> Nothing
        Just i ->
            Just
                $ renderMPFHash
                $ if hexIsLeaf i
                    then leafHash mpfHashing (hexJump i) (hexValue i)
                    else hexValue i

-- | Isomorphism between ByteString and MPFHash.
isoMPFHash :: Iso' ByteString MPFHash
isoMPFHash = iso MPFHash renderMPFHash

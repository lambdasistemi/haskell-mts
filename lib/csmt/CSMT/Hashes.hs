{-# LANGUAGE StrictData #-}

-- |
-- Module      : CSMT.Hashes
-- Description : Blake2b-256 based hashing for CSMT
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- This module provides a concrete hash implementation for CSMTs using
-- Blake2b-256. It includes:
--
-- * 'Hash' - A 32-byte hash value
-- * 'hashHashing' - Hashing functions for tree operations
-- * Serialization functions for proofs and hashes
-- * High-level API for insert, delete, and proof operations
module CSMT.Hashes
    ( mkHash
    , addHash
    , Hash (..)
    , renderHash
    , parseHash
    , insert
    , root
    , generateInclusionProof
    , verifyInclusionProof
    , renderProof
    , parseProof
    , delete
    , hashHashing
    , keyToHash
    , byteStringToKey
    , keyToByteString
    , isoHash
    , fromKVHashes
    )
where

import CSMT.Deletion (deleting)
import CSMT.Hashes.CBOR (parseProof, renderProof)
import CSMT.Hashes.Types (Hash (..), renderHash)
import CSMT.Insertion (inserting)
import CSMT.Interface
    ( Direction (..)
    , FromKV (..)
    , Hashing (..)
    , Indirect (..)
    , Key
    , putIndirect
    , putKey
    )
import CSMT.Interface qualified as Interface
import CSMT.Proof.Insertion qualified as Proof
import Control.Lens (Iso', iso)
import Crypto.Hash (Blake2b_256, hash)
import Data.Bifunctor (second)
import Data.Bits (Bits (..))
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Serialize.Extra (evalPutM)
import Data.Word (Word8)
import Database.KV.Transaction (GCompare, Selector, Transaction)

-- | Compute a Blake2b-256 hash of a ByteString.
mkHash :: ByteString -> Hash
mkHash = convert . hash @ByteString @Blake2b_256

-- | Hashing functions for building CSMT with Blake2b-256.
hashHashing :: Hashing Hash
hashHashing =
    Hashing
        { rootHash = mkHash . evalPutM . putIndirect
        , combineHash = \left right -> mkHash . evalPutM $ do
            putIndirect left
            putIndirect right
        }

-- | Combine two hashes by concatenating and rehashing.
addHash :: Hash -> Hash -> Hash
addHash (Hash h1) (Hash h2) = mkHash (h1 <> h2)

-- | Parse a 32-byte ByteString as a Hash. Returns Nothing if length is wrong.
parseHash :: ByteString -> Maybe Hash
parseHash bs
    | B.length bs == 32 = Just (Hash bs)
    | otherwise = Nothing

-- | Convert a Key to its hash representation.
keyToHash :: Key -> Hash
keyToHash = mkHash . evalPutM . putKey

-- | Insert a key-value pair using Blake2b-256 hashing.
insert
    :: (Monad m, Ord k, GCompare d)
    => FromKV k v Hash
    -> Selector d k v
    -> Selector d Key (Indirect Hash)
    -> k
    -> v
    -> Transaction m cf d ops ()
insert csmt = inserting [] csmt hashHashing

-- | Delete a key-value pair using Blake2b-256 hashing.
delete
    :: (Monad m, Ord k, GCompare d)
    => FromKV k v Hash
    -> Selector d k v
    -> Selector d Key (Indirect Hash)
    -> k
    -> Transaction m cf d ops ()
delete csmt = deleting [] csmt hashHashing

-- | Convert a ByteString to a Key by expanding each byte to 8 directions.
byteStringToKey :: ByteString -> Key
byteStringToKey bs = concatMap byteToDirections (B.unpack bs)

-- | Convert a byte to 8 directions (one per bit, MSB first).
byteToDirections :: Word8 -> Key
byteToDirections byte = [if testBit byte i then R else L | i <- [7, 6 .. 0]]

-- | Convert a Key back to a ByteString (inverse of 'byteStringToKey').
-- Groups every 8 directions into a byte (MSB first).
keyToByteString :: Key -> ByteString
keyToByteString = B.pack . go
  where
    go [] = []
    go ds =
        let (byte, rest) = splitAt 8 ds
            toByte =
                foldl
                    (\acc (i, d) -> case d of R -> setBit acc i; L -> acc)
                    (0 :: Word8)
                    (zip [7, 6 .. 0] byte)
        in  toByte : go rest

-- | Get the root hash of the tree, if it exists.
root
    :: (Monad m, GCompare d)
    => Selector d Key (Indirect Hash)
    -> Transaction m cf d ops (Maybe ByteString)
root csmt = do
    mi <- Interface.root hashHashing csmt []
    case mi of
        Nothing -> return Nothing
        Just v -> return (Just $ renderHash v)

-- | Generate an inclusion proof for a key.
-- Looks up the value from the KV column and returns both the value
-- and the serialized proof, ensuring consistency with the current tree state.
generateInclusionProof
    :: (Monad m, Ord k, GCompare d)
    => FromKV k v Hash
    -> Selector d k v
    -- ^ KV column to look up the value
    -> Selector d Key (Indirect Hash)
    -- ^ CSMT column for tree traversal
    -> k
    -> Transaction m cf d ops (Maybe (v, ByteString))
generateInclusionProof csmt kvSel csmtSel k = do
    mp <- Proof.buildInclusionProof [] csmt kvSel csmtSel hashHashing k
    pure $ fmap (second renderProof) mp

-- | Verify an inclusion proof from a serialized ByteString.
-- Returns True if the proof is internally consistent.
verifyInclusionProof :: ByteString -> Bool
verifyInclusionProof proofBs =
    case parseProof proofBs of
        Nothing -> False
        Just proof -> Proof.verifyInclusionProof hashHashing proof

-- | Isomorphism between ByteString and Hash.
isoHash :: Iso' ByteString Hash
isoHash = iso Hash renderHash

-- | Default FromKV for ByteString keys and values with Blake2b-256 hashing.
fromKVHashes :: FromKV ByteString ByteString Hash
fromKVHashes =
    FromKV
        { isoK = iso byteStringToKey keyToByteString
        , fromV = mkHash
        , treePrefix = const []
        }

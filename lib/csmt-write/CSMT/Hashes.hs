{-# LANGUAGE StrictData #-}

-- |
-- Module      : CSMT.Hashes
-- Description : Blake2b-256 wiring for CSMT (pure Haskell)
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- Wires the pure-Haskell Blake2b-256 from "CSMT.Verify.Blake2b"
-- into the backend-agnostic CSMT algebra in @csmt-core@. The write
-- path and the verifier now share a single hash implementation,
-- so they cannot diverge. Using the pure implementation here also
-- removes the @crypton@ / @memory@ C-FFI dependency from the
-- write path, which lets it cross-compile to GHC's WASM backend.
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

import Control.Lens (Iso', iso)
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Database.KV.Transaction (GCompare, Selector, Transaction)

import CSMT.Core.CBOR (parseProof, renderProof)
import CSMT.Core.Hash
    ( Hash (..)
    , byteStringToKey
    , hashingWith
    , keyToByteString
    , keyToHashWith
    , parseHash
    , renderHash
    )
import CSMT.Core.Proof qualified as Proof
import CSMT.Deletion (deleting)
import CSMT.Insertion (inserting)
import CSMT.Interface
    ( FromKV (..)
    , Hashing
    , Indirect
    , Key
    )
import CSMT.Interface qualified as Interface
import CSMT.Proof.Insertion qualified as ProofInsertion
import CSMT.Verify.Blake2b (blake2b256)

-- | Compute a Blake2b-256 hash of a 'ByteString' using the
-- pure-Haskell implementation in "CSMT.Verify.Blake2b".
mkHash :: ByteString -> Hash
mkHash = Hash . blake2b256

-- | 'Hashing' record wired to 'mkHash'. Shares its implementation
-- with @CSMT.Verify.hashHashing@, so write and verify sides are
-- guaranteed identical by construction.
hashHashing :: Hashing Hash
hashHashing = hashingWith mkHash

-- | Combine two hashes by concatenating and rehashing.
addHash :: Hash -> Hash -> Hash
addHash (Hash h1) (Hash h2) = mkHash (h1 <> h2)

-- | Convert a 'Key' to its hash representation.
keyToHash :: Key -> Hash
keyToHash = keyToHashWith mkHash

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
    mp <- ProofInsertion.buildInclusionProof [] csmt kvSel csmtSel k
    pure $ fmap (second renderProof) mp

-- | Verify an inclusion proof from a serialized ByteString
-- against a trusted root hash.
verifyInclusionProof :: ByteString -> ByteString -> Bool
verifyInclusionProof trustedRootBs proofBs =
    case (parseHash trustedRootBs, parseProof proofBs) of
        (Just trustedRoot, Just proof) ->
            Proof.verifyInclusionProof
                hashHashing
                trustedRoot
                proof
        _ -> False

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

{-# LANGUAGE StrictData #-}

-- |
-- Module      : CSMT.Hashes
-- Description : Blake2b-256 (crypton-backed) wiring for CSMT
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- Wires the @crypton@ C-FFI Blake2b-256 into the backend-agnostic
-- CSMT algebra in @csmt-core@. The WASM-safe verifier in
-- @csmt-verify@ uses the same 'Hashing' shape but routes through
-- a pure-Haskell Blake2b-256 instead. Both sides produce
-- byte-identical hashes (validated by @CSMT.VerifySpec@).
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
import Crypto.Hash (Blake2b_256, hash)
import Data.Bifunctor (second)
import Data.ByteArray (convert)
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

-- | Compute a Blake2b-256 hash of a 'ByteString' using @crypton@'s
-- C implementation.
mkHash :: ByteString -> Hash
mkHash bs = Hash (convert (hash @ByteString @Blake2b_256 bs))

-- | 'Hashing' record wired to 'mkHash'. Matches
-- @CSMT.Verify.hashHashing@ byte-for-byte — both route through
-- 'CSMT.Core.Hash.hashingWith' on top of the same Blake2b-256
-- output.
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

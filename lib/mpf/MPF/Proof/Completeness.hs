{-# LANGUAGE StrictData #-}

-- |
-- Module      : MPF.Proof.Completeness
-- Description : Completeness proofs for MPFs
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- This module provides completeness proofs for Merkle Patricia
-- Forests. A completeness proof proves that a set of leaves
-- comprises ALL leaves in the tree.
--
-- The proof is an 'MPFCompose' tree structure that can be scanned
-- to recompute the root hash. The verifier checks that the leaves
-- in the proof match the provided leaves, and that the computed
-- root hash matches the trusted root.
module MPF.Proof.Completeness
    ( collectMPFLeaves
    , generateMPFCompletenessProof
    , foldMPFCompletenessProof
    , extractLeaves
    )
where

import Data.List (sort)
import Data.Map.Strict qualified as Map
import Database.KV.Transaction
    ( GCompare
    , Selector
    , Transaction
    , query
    )
import MPF.Hashes (MPFHashing (..))
import MPF.Insertion (MPFCompose (..), fetchChildTree, scanMPFCompose)
import MPF.Interface
    ( HexDigit (..)
    , HexIndirect (..)
    , HexKey
    , mkLeafIndirect
    , prefixHex
    )

-- |
-- Collect all leaf values from the MPF trie.
-- The prefix scopes the query to a subtree.
--
-- Returns leaves with their full key path as 'hexJump' and
-- their value hash as 'hexValue'.
collectMPFLeaves
    :: (Monad m, GCompare d)
    => Selector d HexKey (HexIndirect a)
    -> HexKey
    -- ^ Prefix (use @[]@ for root)
    -> Transaction m cf d op [HexIndirect a]
collectMPFLeaves sel prefix = navigate prefix
  where
    navigate currentKey = do
        mi <- query sel currentKey
        case mi of
            Nothing -> pure []
            Just HexIndirect{hexJump, hexValue, hexIsLeaf}
                | hexIsLeaf ->
                    pure [mkLeafIndirect hexJump hexValue]
                | otherwise -> do
                    let base = currentKey <> hexJump
                    concat
                        <$> mapM
                            ( \d -> do
                                cs <- navigate (base <> [d])
                                pure
                                    $ map
                                        (prefixHex (hexJump ++ [d]))
                                        cs
                            )
                            allDigits
    allDigits = [HexDigit n | n <- [0 .. 15]]

-- |
-- Generate a completeness proof for the entire MPF trie.
-- The prefix scopes the query to a subtree.
--
-- Returns the trie as an 'MPFCompose' tree, or 'Nothing' if
-- the tree is empty.
generateMPFCompletenessProof
    :: (Monad m, GCompare d)
    => Selector d HexKey (HexIndirect a)
    -> HexKey
    -- ^ Prefix (use @[]@ for root)
    -> Transaction m cf d op (Maybe (MPFCompose a))
generateMPFCompletenessProof sel prefix = do
    mi <- query sel prefix
    case mi of
        Nothing -> pure Nothing
        Just HexIndirect{hexJump, hexValue, hexIsLeaf}
            | hexIsLeaf ->
                pure
                    $ Just
                    $ MPFComposeLeaf
                    $ mkLeafIndirect hexJump hexValue
            | otherwise -> do
                children <- fetchChildTree sel (prefix <> hexJump)
                pure $ Just $ MPFComposeBranch hexJump children

-- |
-- Verify a completeness proof by computing the tree root hash.
--
-- Extracts leaves from the proof, checks they match the provided
-- leaves (sorted), then scans the proof tree to compute the root
-- hash. Returns 'Nothing' if the leaves do not match.
foldMPFCompletenessProof
    :: (Ord a)
    => MPFHashing a
    -> [HexIndirect a]
    -> MPFCompose a
    -> Maybe a
foldMPFCompletenessProof hashing leaves proof =
    let extracted = extractLeaves proof
        (rootIndirect, _) = scanMPFCompose [] hashing proof
        computedRoot = hexValue rootIndirect
    in  if sort extracted == sort leaves
            then Just computedRoot
            else Nothing

-- |
-- Extract all leaves from an 'MPFCompose' tree with their full
-- key paths reconstructed.
extractLeaves :: MPFCompose a -> [HexIndirect a]
extractLeaves = go []
  where
    go pfx (MPFComposeLeaf HexIndirect{hexJump, hexValue}) =
        [mkLeafIndirect (pfx ++ hexJump) hexValue]
    go pfx (MPFComposeBranch jmp children) =
        concatMap
            (\(d, c) -> go (pfx ++ jmp ++ [d]) c)
            (Map.toList children)

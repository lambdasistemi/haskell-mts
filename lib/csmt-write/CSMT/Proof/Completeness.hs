{-# LANGUAGE StrictData #-}

-- |
-- Module      : CSMT.Proof.Completeness
-- Description : Completeness proofs for CSMTs (write side)
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- Database-backed completeness-proof generation. The proof type and
-- the pure verification fold live in "CSMT.Core.Completeness"; this
-- module adds only the transactional walkers that read entries from
-- the KV store: 'generateProof', 'collectValues', 'queryPrefix'.
--
-- This split mirrors the inclusion-proof split between
-- "CSMT.Core.Proof" (pure) and "CSMT.Proof.Insertion" (database).
module CSMT.Proof.Completeness
    ( -- * Types and pure fold (re-exported from "CSMT.Core.Completeness")
      CompletenessProof (..)
    , foldCompletenessProof
    , foldMergeOps

      -- * Database-backed generation
    , collectValues
    , generateProof
    , queryPrefix
    )
where

import CSMT.Core.Completeness
    ( CompletenessProof (..)
    , foldCompletenessProof
    , foldMergeOps
    )
import CSMT.Interface
    ( Direction (..)
    , Indirect (..)
    , Key
    , compareKeys
    , oppositeDirection
    , prefix
    )
import CSMT.Proof.Insertion (ProofStep (..))
import Database.KV.Transaction
    ( GCompare
    , Selector
    , Transaction
    , query
    )

-- |
-- Collect all leaf values from a subtree rooted at a prefix.
--
-- Navigates from the tree root to the target prefix, handling path
-- compression (jumps that span beyond the prefix). Once the prefix
-- is consumed, collects all leaves below that point.
collectValues
    :: (Monad m, GCompare d)
    => Selector d Key (Indirect a)
    -> Key
    -- ^ Prefix (use @[]@ for root)
    -> Key
    -> Transaction m cf d op [Indirect a]
collectValues sel = navigate
  where
    navigate currentKey remainingPrefix = do
        mi <- query sel currentKey
        case mi of
            Nothing -> pure []
            Just (Indirect fullJump val) ->
                let (_, prefixRest, jumpRest) =
                        compareKeys remainingPrefix fullJump
                in  case prefixRest of
                        -- Prefix consumed: collect everything below
                        [] -> do
                            let base = currentKey <> fullJump
                            l <- navigate (base <> [L]) []
                            r <- navigate (base <> [R]) []
                            if null l && null r
                                then pure [Indirect jumpRest val]
                                else
                                    pure
                                        $ prefix jumpRest
                                            <$> ( (prefix [L] <$> l)
                                                    <> (prefix [R] <$> r)
                                                )
                        -- Jump consumed, prefix continues
                        (d : rest)
                            | null jumpRest ->
                                navigate
                                    (currentKey <> fullJump <> [d])
                                    rest
                            -- Divergence: no entries under this prefix
                            | otherwise -> pure []

-- |
-- Generate a completeness proof for a subtree rooted at a prefix.
--
-- Navigates from the tree root to the target prefix, collecting
-- inclusion proof steps (sibling hashes) at each branch. Then
-- generates merge operations for the subtree below the prefix.
generateProof
    :: forall m d a cf op
     . (Monad m, GCompare d)
    => Selector d Key (Indirect a)
    -> Key
    -- ^ Prefix (use @[]@ for root)
    -> Key
    -> Transaction m cf d op (Maybe (CompletenessProof a))
generateProof sel pfx targetPrefix = do
    result <- navigate 0 pfx targetPrefix []
    pure $ case result of
        Nothing -> Nothing
        Just (mergeOps, _, inclusionSteps) ->
            Just
                CompletenessProof
                    { cpMergeOps = mergeOps
                    , cpInclusionSteps = reverse inclusionSteps
                    }
  where
    navigate
        :: Int
        -> Key
        -> Key
        -> [ProofStep a]
        -> Transaction
            m
            cf
            d
            op
            (Maybe ([(Int, Int)], (Int, Int), [ProofStep a]))
    navigate n currentKey remainingPrefix steps = do
        mi <- query sel currentKey
        case mi of
            Nothing -> pure Nothing
            Just (Indirect fullJump _) ->
                let (_, prefixRest, jumpRest) =
                        compareKeys remainingPrefix fullJump
                in  case prefixRest of
                        -- Prefix consumed: generate merge ops below
                        [] -> do
                            r <- go n currentKey fullJump
                            pure $ case r of
                                Nothing -> Nothing
                                Just (ops, idx) ->
                                    Just (ops, idx, steps)
                        -- Jump consumed, prefix continues
                        (d : rest)
                            | null jumpRest -> do
                                -- Collect sibling for inclusion proof
                                let sibKey =
                                        currentKey
                                            <> fullJump
                                            <> [oppositeDirection d]
                                msib <- query sel sibKey
                                case msib of
                                    Nothing -> pure Nothing
                                    Just sib -> do
                                        -- Query child to get its jump
                                        let childKey =
                                                currentKey
                                                    <> fullJump
                                                    <> [d]
                                        mchild <- query sel childKey
                                        case mchild of
                                            Nothing -> pure Nothing
                                            Just (Indirect childJump _) ->
                                                let step =
                                                        ProofStep
                                                            { stepConsumed =
                                                                1
                                                                    + length
                                                                        childJump
                                                            , stepSibling =
                                                                sib
                                                            }
                                                in  navigate
                                                        n
                                                        ( currentKey
                                                            <> fullJump
                                                            <> [d]
                                                        )
                                                        rest
                                                        (step : steps)
                            -- Divergence: no entries under this prefix
                            | otherwise -> pure Nothing
    go
        :: Int
        -> Key
        -> Key
        -> Transaction
            m
            cf
            d
            op
            (Maybe ([(Int, Int)], (Int, Int)))
    go n key jmp = do
        let base = key <> jmp
            leftKey = base <> [L]
            rightKey = base <> [R]
        ml <- goChild n leftKey
        case ml of
            Nothing -> pure $ Just ([], (n + 1, n))
            Just (lxs, (n', li)) -> do
                mr <- goChild n' rightKey
                case mr of
                    Nothing -> error "Right subtree missing"
                    Just (rxs, (n'', ri)) ->
                        pure
                            $ Just
                                ( lxs
                                    ++ rxs
                                    ++ [(li, ri)]
                                , (n'', n)
                                )
    goChild
        :: Int
        -> Key
        -> Transaction
            m
            cf
            d
            op
            (Maybe ([(Int, Int)], (Int, Int)))
    goChild n key = do
        mi <- query sel key
        case mi of
            Nothing -> pure Nothing
            Just (Indirect jmp _) -> go n key jmp

-- |
-- Query the effective subtree root at a prefix.
--
-- Navigates from the tree root to the target prefix, returning the
-- 'Indirect' at the prefix boundary with the remaining jump as the
-- new jump field. Returns 'Nothing' if no entries exist under the
-- prefix.
queryPrefix
    :: (Monad m, GCompare d)
    => Selector d Key (Indirect a)
    -> Key
    -- ^ Prefix (use @[]@ for root)
    -> Key
    -> Transaction m cf d op (Maybe (Indirect a))
queryPrefix sel = navigate
  where
    navigate currentKey remainingPrefix = do
        mi <- query sel currentKey
        case mi of
            Nothing -> pure Nothing
            Just (Indirect fullJump val) ->
                let (_, prefixRest, jumpRest) =
                        compareKeys remainingPrefix fullJump
                in  case prefixRest of
                        [] -> pure $ Just (Indirect jumpRest val)
                        (d : rest)
                            | null jumpRest ->
                                navigate
                                    (currentKey <> fullJump <> [d])
                                    rest
                            | otherwise -> pure Nothing

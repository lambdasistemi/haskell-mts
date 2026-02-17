{-# LANGUAGE StrictData #-}

-- |
-- Module      : CSMT.Proof.Completeness
-- Description : Completeness proofs for CSMTs
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- This module provides functionality for completeness proofs - proving
-- that a set of values comprises the entire tree contents.
--
-- A completeness proof is a sequence of merge operations that, when
-- applied to a list of leaf values, produces the root hash. This allows
-- clients to verify they have received all values in the tree.
module CSMT.Proof.Completeness
    ( CompletenessProof
    , foldProof
    , collectValues
    , generateProof
    , queryPrefix
    )
where

import CSMT.Interface
    ( Direction (..)
    , Indirect (..)
    , Key
    , compareKeys
    , prefix
    )
import Data.Map.Strict qualified as Map
import Database.KV.Transaction
    ( GCompare
    , Selector
    , Transaction
    , query
    )

-- |
-- A completeness proof as a sequence of merge operations.
--
-- Each pair (i, j) indicates that values at indices i and j should be
-- combined to produce a parent hash. The final result should match the root.
type CompletenessProof = [(Int, Int)]

-- | A function to compose two indirect values into a combined hash.
type Compose a = Indirect a -> Indirect a -> a

-- |
-- Verify a completeness proof by folding the merge operations.
--
-- Takes a list of leaf values and applies the proof's merge operations
-- to compute the root. Returns 'Nothing' if the proof is invalid.
--
-- This function is intended for client-side verification where the client
-- receives the list of all values and the proof.
foldProof
    :: Compose a
    -- ^ Function to compose two Indirect values
    -> [Indirect a]
    -- ^ List of indirect values (leaves of the CSMT)
    -> CompletenessProof
    -- ^ Proof steps (merge operations to apply)
    -> Maybe (Indirect a)
    -- ^ Computed root, or Nothing if proof is invalid
foldProof compose values = go (Map.fromList $ zip [0 ..] values)
  where
    go m [] = Just $ m Map.! 0
    go m ((i, j) : xs) =
        let (Indirect pri vi) = m Map.! i
            (Indirect prj vj) = m Map.! j
        in  case compareKeys pri prj of
                (common, _ : si, _ : sj) ->
                    let
                        v =
                            Indirect
                                { jump = common
                                , value =
                                    compose
                                        Indirect{jump = si, value = vi}
                                        Indirect{jump = sj, value = vj}
                                }
                        m' = Map.insert i v m
                    in
                        go m' xs
                _ -> Nothing

-- | Error type for malformed trees (unused, kept for documentation).
data TreeWithDifferentLengthsError = TreeWithDifferentLengthsError
    deriving (Show)

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
    -> Transaction m cf d op [Indirect a]
collectValues sel targetPrefix = navigate [] targetPrefix
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
-- Navigates from the tree root to the target prefix, then generates
-- a sequence of merge operations that, when applied to the collected
-- leaf values, will produce the subtree root hash.
generateProof
    :: forall m d a cf op
     . (Monad m, GCompare d)
    => Selector d Key (Indirect a)
    -> Key
    -> Transaction m cf d op (Maybe CompletenessProof)
generateProof sel targetPrefix =
    fmap (fmap fst) $ navigate 0 [] targetPrefix
  where
    navigate
        :: Int
        -> Key
        -> Key
        -> Transaction
            m
            cf
            d
            op
            (Maybe (CompletenessProof, (Int, Int)))
    navigate n currentKey remainingPrefix = do
        mi <- query sel currentKey
        case mi of
            Nothing -> pure Nothing
            Just (Indirect fullJump _) ->
                let (_, prefixRest, jumpRest) =
                        compareKeys remainingPrefix fullJump
                in  case prefixRest of
                        -- Prefix consumed: generate proof below
                        [] -> go n currentKey fullJump
                        -- Jump consumed, prefix continues
                        (d : rest)
                            | null jumpRest ->
                                navigate
                                    n
                                    (currentKey <> fullJump <> [d])
                                    rest
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
            (Maybe (CompletenessProof, (Int, Int)))
    go n key jump = do
        let base = key <> jump
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
            (Maybe (CompletenessProof, (Int, Int)))
    goChild n key = do
        mi <- query sel key
        case mi of
            Nothing -> pure Nothing
            Just (Indirect jump _) -> go n key jump

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
    -> Transaction m cf d op (Maybe (Indirect a))
queryPrefix sel targetPrefix = navigate [] targetPrefix
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

{-# LANGUAGE StrictData #-}

-- |
-- Module      : CSMT.Proof.Completeness
-- Description : Completeness proofs for CSMTs
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- This module provides completeness proofs - proving that a set of
-- values comprises ALL leaves under a given prefix in the tree.
--
-- A completeness proof contains:
--
-- * Merge operations that reconstruct the subtree root from leaves
-- * Inclusion proof steps anchoring the subtree root to the tree root
--
-- The verifier provides the prefix and a trusted root hash. The fold
-- function derives the root jump internally from the prefix, the
-- subtree jump (from the merge ops result), and the step consumed
-- counts.
module CSMT.Proof.Completeness
    ( CompletenessProof (..)
    , foldCompletenessProof
    , foldMergeOps
    , collectValues
    , generateProof
    , queryPrefix
    )
where

import CSMT.Interface
    ( Direction (..)
    , Hashing (..)
    , Indirect (..)
    , Key
    , addWithDirection
    , compareKeys
    , oppositeDirection
    , prefix
    )
import CSMT.Proof.Insertion (ProofStep (..))
import Data.Map.Strict qualified as Map
import Database.KV.Transaction
    ( GCompare
    , Selector
    , Transaction
    , query
    )

-- |
-- A completeness proof for a subtree under a given prefix.
--
-- Contains merge operations to reconstruct the subtree root from
-- leaves, and inclusion steps to anchor the subtree root to the
-- tree root. The prefix is not stored — the verifier already knows
-- it. The root jump is derived during verification.
data CompletenessProof a = CompletenessProof
    { cpMergeOps :: [(Int, Int)]
    -- ^ Merge operations: each @(i, j)@ combines leaves at those
    -- indices. Applied to the leaf list, these reconstruct the
    -- subtree root.
    , cpInclusionSteps :: [ProofStep a]
    -- ^ Inclusion proof steps from the subtree root outward to
    -- the tree root. Empty when the prefix covers the whole tree
    -- (prefix = []) or when the root jump subsumes the prefix.
    }
    deriving (Show, Eq)

-- | A function to compose two indirect values into a combined hash.
type Compose a = Indirect a -> Indirect a -> a

-- |
-- Fold merge operations over a list of leaves.
--
-- Returns the subtree root as an 'Indirect', or 'Nothing' if the
-- proof is invalid (e.g. key comparison fails).
foldMergeOps
    :: Compose a
    -> [Indirect a]
    -> [(Int, Int)]
    -> Maybe (Indirect a)
foldMergeOps compose values = go (Map.fromList $ zip [0 ..] values)
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

-- |
-- Verify a completeness proof by computing the tree root hash.
--
-- The verifier provides the prefix (which they chose) and the
-- leaves (which they received). The root jump is derived from
-- the prefix, the subtree jump, and the inclusion step consumed
-- counts.
--
-- Returns 'Nothing' if the proof is malformed.
foldCompletenessProof
    :: Hashing a
    -> Key
    -- ^ The prefix this proof covers (verifier provides this)
    -> [Indirect a]
    -- ^ Leaves under the prefix
    -> CompletenessProof a
    -> Maybe a
    -- ^ Computed tree root hash
foldCompletenessProof
    hashing
    prefixKey
    leaves
    CompletenessProof{cpMergeOps, cpInclusionSteps} = do
        subtreeRoot <- case leaves of
            [single] | null cpMergeOps -> Just single
            _ -> foldMergeOps (combineHash hashing) leaves cpMergeOps
        let Indirect subtreeJump subtreeValue = subtreeRoot
            fullKey = prefixKey ++ subtreeJump
            totalConsumed =
                sum (map stepConsumed cpInclusionSteps)
            rootJumpLen = length fullKey - totalConsumed
            rootJump = take rootJumpLen fullKey
            keyAfterRoot = drop rootJumpLen fullKey
            rootValue =
                foldInclusionSteps
                    hashing
                    subtreeValue
                    (reverse keyAfterRoot)
                    cpInclusionSteps
        pure $ rootHash hashing (Indirect rootJump rootValue)

-- |
-- Fold inclusion steps from subtree outward to root.
--
-- Consumes key bits in reverse (from subtree toward root) and
-- combines with sibling hashes at each level. Same logic as
-- 'computeRootHash' from "CSMT.Proof.Insertion".
foldInclusionSteps
    :: Hashing a -> a -> Key -> [ProofStep a] -> a
foldInclusionSteps _ acc _ [] = acc
foldInclusionSteps
    hashing
    acc
    revKey
    (ProofStep{stepConsumed, stepSibling} : rest) =
        let (consumedRev, remainingRev) = splitAt stepConsumed revKey
            consumed = reverse consumedRev
        in  case consumed of
                (direction : stepJump) ->
                    foldInclusionSteps
                        hashing
                        ( addWithDirection
                            hashing
                            direction
                            (Indirect stepJump acc)
                            stepSibling
                        )
                        remainingRev
                        rest
                [] ->
                    error
                        "foldInclusionSteps: invalid step with zero consumed bits"

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

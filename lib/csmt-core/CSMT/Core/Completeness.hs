{-# LANGUAGE StrictData #-}

-- |
-- Module      : CSMT.Core.Completeness
-- Description : Pure completeness-proof type and verification fold
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- Backend-agnostic completeness-proof types and verification logic
-- shared by both the @csmt-write@ (database-backed) library and the
-- @csmt-verify@ (WASM-safe) sublibrary. The types and the pure
-- verification fold live here; the database-backed
-- @generateProof@/@collectValues@/@queryPrefix@ stay in
-- "CSMT.Proof.Completeness" because they require a transactional
-- KV store.
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
module CSMT.Core.Completeness
    ( CompletenessProof (..)
    , foldCompletenessProof
    , foldMergeOps
    ) where

import Data.Map.Strict qualified as Map

import CSMT.Core.Proof (ProofStep (..))
import CSMT.Core.Types
    ( Hashing (..)
    , Indirect (..)
    , Key
    , addWithDirection
    , compareKeys
    )

-- | A completeness proof for a subtree under a given prefix.
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

-- | Fold merge operations over a list of leaves.
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

-- | Verify a completeness proof by computing the tree root hash.
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

-- | Fold inclusion steps from subtree outward to root.
--
-- Consumes key bits in reverse (from subtree toward root) and
-- combines with sibling hashes at each level. Same logic as
-- @computeRootHash@ from "CSMT.Core.Proof".
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
                        "foldInclusionSteps: invalid step \
                        \with zero consumed bits"

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
-- A completeness proof can take one of two shapes:
--
-- * 'CompletenessWitness' — the prefix has at least one leaf;
--   the proof carries merge operations to reconstruct the
--   subtree root from the leaves, plus inclusion-proof steps
--   anchoring the subtree root to the tree root.
--
-- * 'CompletenessEmpty' — the prefix has no leaves under the
--   trusted root; the proof carries a single 'ExclusionProof'
--   whose target is the prefix itself. The witness's path
--   diverges from the prefix within a jump, which the verifier
--   interprets as "no key has the prefix as its prefix" — the
--   exact emptiness claim.
--
-- The verifier supplies the prefix and a trusted root hash. The
-- fold function dispatches on the constructor and the supplied
-- leaf list (empty leaves ⇒ 'CompletenessEmpty', non-empty
-- leaves ⇒ 'CompletenessWitness'); a mismatch fails the proof.
module CSMT.Core.Completeness
    ( CompletenessProof (..)
    , foldCompletenessProof
    , foldMergeOps
    ) where

import Data.List (isPrefixOf)
import Data.Map.Strict qualified as Map

import CSMT.Core.Exclusion
    ( ExclusionProof (..)
    , verifyExclusionProof
    )
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
-- 'CompletenessWitness' is the populated case (one or more
-- leaves). 'CompletenessEmpty' is the absent-prefix case, where
-- the prefix has no entries under the trusted root.
data CompletenessProof a
    = -- | Populated subtree: merge operations reconstruct the
      -- subtree root from the leaves, and inclusion steps
      -- anchor the subtree root to the tree root. The prefix
      -- is not stored — the verifier already knows it. The
      -- root jump is derived during verification.
      CompletenessWitness
        { cpMergeOps :: [(Int, Int)]
        -- ^ Merge operations: each @(i, j)@ combines leaves
        -- at those indices. Applied to the leaf list, these
        -- reconstruct the subtree root.
        , cpInclusionSteps :: [ProofStep a]
        -- ^ Inclusion proof steps from the subtree root
        -- outward to the tree root. Empty when the prefix
        -- covers the whole tree (prefix = []) or when the
        -- root jump subsumes the prefix.
        }
    | -- | Absent prefix: an 'ExclusionProof' whose target key
      -- is the prefix. Verified by 'verifyExclusionProof',
      -- which checks the embedded inclusion-proof witness
      -- against the trusted root and confirms the divergence
      -- happens within a jump. A divergence within a jump is
      -- exactly the guarantee that no key extends the prefix.
      CompletenessEmpty (ExclusionProof a)
    deriving (Show, Eq)

-- | A function to compose two indirect values into a combined hash.
type Compose a = Indirect a -> Indirect a -> a

-- | Fold merge operations over a list of leaves.
--
-- Returns the subtree root as an 'Indirect', or 'Nothing' if
-- the proof is invalid (e.g. key comparison fails).
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

-- | Verify a completeness proof against a trusted root hash.
--
-- The verifier provides the prefix (which they chose) and the
-- leaves (which they received). The proof must match the leaf
-- shape:
--
-- * non-empty leaves ⇒ 'CompletenessWitness';
-- * empty leaves ⇒ 'CompletenessEmpty'.
--
-- For 'CompletenessWitness', each leaf's @jump@ field MUST be
-- absolute (start with @prefixKey@). The fold strips the prefix
-- internally before reconstructing the subtree root, then walks
-- the inclusion steps back to the column root.
--
-- For 'CompletenessEmpty', the embedded 'ExclusionProof' is
-- verified directly; the result is the trusted root on success
-- (so the caller's @==@ check trivially holds) and 'Nothing' on
-- failure.
--
-- Returns 'Nothing' on shape mismatch or any malformed input
-- (including a witness leaf whose jump does not start with
-- @prefixKey@).
foldCompletenessProof
    :: Eq a
    => Hashing a
    -> a
    -- ^ Trusted root (used for the empty case; threaded through
    -- so the same caller path works for both variants).
    -> Key
    -- ^ The prefix this proof covers (verifier provides this)
    -> [Indirect a]
    -- ^ Leaves under the prefix, with jumps in absolute form
    -- (each jump starts with @prefixKey@)
    -> CompletenessProof a
    -> Maybe a
    -- ^ Recomputed tree root, or 'Nothing' on shape mismatch /
    -- malformed proof.
foldCompletenessProof
    hashing
    _trustedRoot
    prefixKey
    leaves
    (CompletenessWitness cpMergeOps cpInclusionSteps) = do
        relativeLeaves <- traverse (stripLeafPrefix prefixKey) leaves
        subtreeRoot <- case relativeLeaves of
            [single] | null cpMergeOps -> Just single
            [] -> Nothing
            _ ->
                foldMergeOps
                    (combineHash hashing)
                    relativeLeaves
                    cpMergeOps
        let Indirect subtreeJumpRel subtreeValue = subtreeRoot
            -- 'subtreeJumpRel' is the part of the subtree node's
            -- absolute key beyond @prefixKey@. The full absolute
            -- key for the subtree node is therefore
            -- @prefixKey ++ subtreeJumpRel@; that is what the
            -- inclusion-step consumed counts walk back to the
            -- column root.
            fullKey = prefixKey ++ subtreeJumpRel
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
      where
        stripLeafPrefix
            :: Key -> Indirect a -> Maybe (Indirect a)
        stripLeafPrefix p (Indirect jmp val)
            | p `isPrefixOf` jmp =
                Just (Indirect (drop (length p) jmp) val)
            | otherwise = Nothing
foldCompletenessProof
    hashing
    trustedRoot
    prefixKey
    leaves
    (CompletenessEmpty exclusion) =
        case (leaves, exclusion) of
            ([], ExclusionWitness{epTargetKey})
                | epTargetKey == prefixKey
                , verifyExclusionProof hashing trustedRoot exclusion ->
                    Just trustedRoot
            _ -> Nothing

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

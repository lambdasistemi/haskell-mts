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
--
-- The leaves are the leaves of the tree under the prefix, by
-- ascending key as @collectValues@ returns them: the merge
-- operations index that list, so a list in another order is
-- another statement and is refused. The fold accepts a leaf
-- list only if the proof rebuilds, from exactly those leaves and
-- no others, the node where the prefix ends, and anchors that
-- node to the root with steps that fit it. No proof makes the
-- fold throw.
--
-- A tree holds keys of one fixed length. Leaf and branch hashes
-- are not domain-separated, so a branch of the tree given as a
-- leaf with a shorter key passes for a leaf, and the fold cannot
-- tell: callers that accept leaves from outside check the length
-- of their keys.
module CSMT.Core.Completeness
    ( CompletenessProof (..)
    , foldCompletenessProof
    , foldMergeOps
    ) where

import Control.Monad (guard)
import Data.List (isPrefixOf)
import Data.Map.Strict qualified as Map

import CSMT.Core.Exclusion
    ( ExclusionProof (..)
    , verifyExclusionProof
    )
import CSMT.Core.Proof
    ( InclusionProof (..)
    , ProofStep (..)
    , computeRootHash
    )
import CSMT.Core.Types
    ( Hashing (..)
    , Indirect (..)
    , Key
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
        -- ^ Merge operations: each @(i, j)@ combines the
        -- leaves at those indices into the one at @i@ and
        -- removes the one at @j@. Applied to the leaf list,
        -- they reconstruct the subtree root; every leaf is
        -- merged, none twice.
        , cpInclusionSteps :: [ProofStep a]
        -- ^ Inclusion proof steps from the subtree root
        -- outward to the tree root. Empty when the prefix
        -- covers the whole tree (prefix = []) or when the
        -- root jump subsumes the prefix. The first step
        -- consumes the direction bit and the whole jump of
        -- the node where the prefix ends.
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
-- Each operation @(i, j)@ merges the values at @i@ and @j@ into
-- the one at @i@ and removes the one at @j@. Returns the subtree
-- root as an 'Indirect', or 'Nothing' if the proof is invalid:
-- an index that names no value, a value merged with itself or
-- with one that was merged away, keys that do not diverge, or
-- values left over once the operations end. The root is the one
-- value that remains.
foldMergeOps
    :: Compose a
    -> [Indirect a]
    -> [(Int, Int)]
    -> Maybe (Indirect a)
foldMergeOps compose values = go (Map.fromList $ zip [0 ..] values)
  where
    go live [] = case Map.elems live of
        [root] -> Just root
        _ -> Nothing
    go live ((i, j) : xs) = do
        left <- Map.lookup i live
        right <- Map.lookup j live
        merged <- merge left right
        go (Map.insert i merged (Map.delete j live)) xs
    merge (Indirect pri vi) (Indirect prj vj) =
        case compareKeys pri prj of
            (common, _ : si, _ : sj) ->
                Just
                    Indirect
                        { jump = common
                        , value =
                            compose
                                Indirect{jump = si, value = vi}
                                Indirect{jump = sj, value = vj}
                        }
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
-- absolute (start with @prefixKey@), and the leaves MUST be in
-- ascending key order. The fold strips the prefix internally
-- before reconstructing the subtree root, then walks the
-- inclusion steps back to the column root. The steps are each of
-- at least one bit, together no more than the key of the subtree
-- root, and the first of them reaches back over the direction bit
-- of the node where the prefix ends: a proof built for a longer
-- prefix does not pass for a shorter one that the node does not
-- cover.
--
-- For 'CompletenessEmpty', the embedded 'ExclusionProof' is
-- verified directly; the result is the trusted root on success
-- (so the caller's @==@ check trivially holds) and 'Nothing' on
-- failure.
--
-- Returns 'Nothing' on shape mismatch or any malformed input
-- (including a witness leaf whose jump does not start with
-- @prefixKey@). No input makes it throw.
foldCompletenessProof
    :: Eq a
    => Hashing a
    -> a
    -- ^ Trusted root (used for the empty case; threaded through
    -- so the same caller path works for both variants).
    -> Key
    -- ^ The prefix this proof covers (verifier provides this)
    -> [Indirect a]
    -- ^ Leaves under the prefix, in ascending key order, with
    -- jumps in absolute form (each jump starts with @prefixKey@)
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
        guard $ strictlyAscending (jump <$> leaves)
        relativeLeaves <- traverse (stripLeafPrefix prefixKey) leaves
        Indirect subtreeJumpRel subtreeValue <-
            foldMergeOps
                (combineHash hashing)
                relativeLeaves
                cpMergeOps
        let
            -- The absolute key of the subtree node is
            -- @prefixKey ++ subtreeJumpRel@; the inclusion steps
            -- walk it back to the column root.
            fullKey = prefixKey ++ subtreeJumpRel
            consumed = stepConsumed <$> cpInclusionSteps
            totalConsumed = sum (toInteger <$> consumed)
        guard $ all (>= 1) consumed
        guard $ totalConsumed <= toInteger (length fullKey)
        -- The first step reaches back over the direction bit and the
        -- whole jump of the node where the prefix ends, so the prefix
        -- ends inside that node and the leaves are all of its.
        guard $ case consumed of
            first : _ -> first > length subtreeJumpRel
            [] -> True
        pure
            $ computeRootHash
                hashing
                InclusionProof
                    { proofKey = fullKey
                    , proofValue = subtreeValue
                    , proofSteps = cpInclusionSteps
                    , proofRootJump =
                        take
                            (length fullKey - fromInteger totalConsumed)
                            fullKey
                    }
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

stripLeafPrefix :: Key -> Indirect a -> Maybe (Indirect a)
stripLeafPrefix p (Indirect jmp val)
    | p `isPrefixOf` jmp = Just (Indirect (drop (length p) jmp) val)
    | otherwise = Nothing

strictlyAscending :: Ord k => [k] -> Bool
strictlyAscending ks = and (zipWith (<) ks (drop 1 ks))

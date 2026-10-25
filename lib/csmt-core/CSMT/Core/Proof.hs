{-# LANGUAGE StrictData #-}

-- |
-- Module      : CSMT.Core.Proof
-- Description : Pure Merkle inclusion-proof types and verification
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- Inclusion-proof types and backend-agnostic verification logic
-- shared by both the @csmt@ (database-backed) library and the
-- @csmt-verify@ (WASM-safe) sublibrary. Neither of them owns the
-- definition; both re-export from here.
--
-- A tree holds keys of one fixed length. Leaf and branch hashes are
-- not domain-separated, so for a key of another length a verifier
-- cannot tell a leaf value from a branch hash: a branch hash given
-- as the value of such a key can be accepted. Callers that accept
-- keys from outside check their length before trusting a verdict.
module CSMT.Core.Proof
    ( -- * Types
      InclusionProof (..)
    , ProofStep (..)
    , StepView (..)

      -- * Verification
    , verifyInclusionProof
    , computeRootHash
    , foldProof
    ) where

import CSMT.Core.Types
    ( Direction
    , Hashing (..)
    , Indirect (..)
    , Key
    , addWithDirection
    )
import Data.List (isPrefixOf)

-- | A single step in an inclusion proof. Records how many key
-- bits are consumed at this step (direction + jump length) and
-- the sibling's indirect value needed to recompute the parent.
data ProofStep a = ProofStep
    { stepConsumed :: Int
    , stepSibling :: Indirect a
    }
    deriving (Show, Eq)

-- | An inclusion proof for a key-value pair. Contains the path
-- data needed to recompute the root hash from a key-value pair.
-- Verification requires an externally-supplied trusted root hash.
data InclusionProof a = InclusionProof
    { proofKey :: Key
    , proofValue :: a
    , proofSteps :: [ProofStep a]
    , proofRootJump :: Key
    }
    deriving (Show, Eq)

-- | What the callback sees at each proof step during a fold.
data StepView = StepView
    { svDirection :: Direction
    , svJump :: Key
    }
    deriving (Show, Eq)

-- | Verify an inclusion proof against a trusted root hash.
--
-- The proof must bind the whole key: the root jump is exactly the
-- leading bits of 'proofKey' and the steps consume exactly the
-- rest of it, each step at least one bit and at most what is left.
-- A proof that does not is refused, whatever it hashes to. No proof
-- makes this function throw.
verifyInclusionProof
    :: Eq a => Hashing a -> a -> InclusionProof a -> Bool
verifyInclusionProof hashing trustedRoot proof =
    wellFormed walk && trustedRoot == walkRoot walk
  where
    walk = walkProof hashing proof (\() _ -> ()) ()

-- | Fold over an inclusion proof's steps with a callback,
-- returning both the computed root hash and the final accumulator.
--
-- The fold is total. The walk ends at the first step that cannot
-- be taken, because its size is below one or reaches past the
-- key; the result then belongs to no tree. Only
-- 'verifyInclusionProof' tells whether a proof binds its key, so a
-- root computed here is compared with a trusted root through it.
foldProof
    :: Hashing a
    -> InclusionProof a
    -> (acc -> StepView -> acc)
    -> acc
    -> (a, acc)
foldProof hashing proof step acc0 =
    (walkRoot walk, walkAcc walk)
  where
    walk = walkProof hashing proof step acc0

-- | Recompute the root hash from an inclusion proof. For a proof
-- that does not bind its key the result belongs to no tree; see
-- 'foldProof'.
computeRootHash :: Hashing a -> InclusionProof a -> a
computeRootHash hashing proof =
    fst $ foldProof hashing proof (\() _ -> ()) ()

-- | The outcome of walking a proof from its leaf to its root.
data Walk a acc = Walk
    { wellFormed :: Bool
    -- ^ the root jump is the leading bits of the key and the steps,
    -- each of at least one bit, consume the rest of the key exactly
    , walkRoot :: a
    , walkAcc :: acc
    }

walkProof
    :: Hashing a
    -> InclusionProof a
    -> (acc -> StepView -> acc)
    -> acc
    -> Walk a acc
walkProof
    hashing
    InclusionProof{proofKey, proofValue, proofSteps, proofRootJump}
    step
    acc0 =
        Walk
            { wellFormed = rootJumpBound && keyConsumed
            , walkRoot =
                rootHash hashing (Indirect proofRootJump rootValue)
            , walkAcc = accFinal
            }
      where
        rootJumpBound = proofRootJump `isPrefixOf` proofKey
        keyAfterRoot = drop (length proofRootJump) proofKey
        (rootValue, accFinal, keyConsumed) =
            go proofValue acc0 (reverse keyAfterRoot) proofSteps
        go hashAcc acc revKey [] = (hashAcc, acc, null revKey)
        go hashAcc acc revKey (ProofStep{stepConsumed, stepSibling} : rest) =
            let (consumedRev, remainingRev) =
                    splitAt stepConsumed revKey
            in  case reverse consumedRev of
                    direction : stepJump
                        | length consumedRev == stepConsumed ->
                            let sv =
                                    StepView
                                        { svDirection = direction
                                        , svJump = stepJump
                                        }
                                acc' = step acc sv
                            in  go
                                    ( addWithDirection
                                        hashing
                                        direction
                                        (Indirect stepJump hashAcc)
                                        stepSibling
                                    )
                                    acc'
                                    remainingRev
                                    rest
                    _ -> (hashAcc, acc, False)

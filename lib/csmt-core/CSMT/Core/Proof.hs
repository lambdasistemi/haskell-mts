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
verifyInclusionProof
    :: Eq a => Hashing a -> a -> InclusionProof a -> Bool
verifyInclusionProof hashing trustedRoot proof =
    trustedRoot == computeRootHash hashing proof

-- | Fold over an inclusion proof's steps with a callback,
-- returning both the computed root hash and the final accumulator.
foldProof
    :: Hashing a
    -> InclusionProof a
    -> (acc -> StepView -> acc)
    -> acc
    -> (a, acc)
foldProof
    hashing
    InclusionProof{proofKey, proofValue, proofSteps, proofRootJump}
    step
    acc0 =
        let keyAfterRoot = drop (length proofRootJump) proofKey
            (rootValue, accFinal) =
                go proofValue acc0 (reverse keyAfterRoot) proofSteps
        in  (rootHash hashing (Indirect proofRootJump rootValue), accFinal)
      where
        go hashAcc acc _ [] = (hashAcc, acc)
        go hashAcc acc revKey (ProofStep{stepConsumed, stepSibling} : rest) =
            let (consumedRev, remainingRev) =
                    splitAt stepConsumed revKey
                consumed = reverse consumedRev
            in  case consumed of
                    (direction : stepJump) ->
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
                    [] ->
                        error
                            "foldProof: invalid proof step \
                            \with zero consumed bits"

-- | Recompute the root hash from an inclusion proof.
computeRootHash :: Hashing a -> InclusionProof a -> a
computeRootHash hashing proof =
    fst $ foldProof hashing proof (\() _ -> ()) ()

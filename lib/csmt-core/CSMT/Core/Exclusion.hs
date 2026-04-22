{-# LANGUAGE StrictData #-}

-- |
-- Module      : CSMT.Core.Exclusion
-- Description : Pure Merkle exclusion-proof type and verification
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- Exclusion-proof type and backend-agnostic verification logic,
-- shared by the @csmt@ (database-backed) library and the
-- @csmt-verify@ (WASM-safe) sublibrary.
module CSMT.Core.Exclusion
    ( ExclusionProof (..)
    , verifyExclusionProof
    ) where

import CSMT.Core.Proof
    ( InclusionProof (..)
    , ProofStep (..)
    , verifyInclusionProof
    )
import CSMT.Core.Types
    ( Hashing
    , Key
    )

-- | An exclusion proof for a key in a CSMT.
data ExclusionProof a
    = -- | The tree is empty; any key is trivially absent.
      ExclusionEmpty
    | -- | A witness key diverges from the target within a jump.
      ExclusionWitness
        { epTargetKey :: Key
        , epWitnessProof :: InclusionProof a
        }
    deriving (Show, Eq)

-- | Verify an exclusion proof against a trusted root hash.
--
-- For 'ExclusionEmpty': always 'True' (empty tree trivially
-- excludes any key). Callers that need to distinguish an
-- empty tree from a populated one should inspect the root
-- separately.
--
-- For 'ExclusionWitness': verifies the witness inclusion proof
-- against the trusted root, then checks that the target key
-- diverges from the witness key within a jump region (not at
-- a branch boundary).
verifyExclusionProof
    :: Eq a => Hashing a -> a -> ExclusionProof a -> Bool
verifyExclusionProof _ _ ExclusionEmpty = True
verifyExclusionProof
    hashing
    trustedRoot
    ExclusionWitness{epTargetKey, epWitnessProof} =
        let hashValid =
                verifyInclusionProof
                    hashing
                    trustedRoot
                    epWitnessProof
            divergenceValid =
                checkKeyDivergence
                    epTargetKey
                    (proofKey epWitnessProof)
                    (proofRootJump epWitnessProof)
                    ( map
                        stepConsumed
                        (proofSteps epWitnessProof)
                    )
        in  hashValid && divergenceValid

checkKeyDivergence
    :: Key
    -- ^ Target key
    -> Key
    -- ^ Witness key
    -> Key
    -- ^ Root jump
    -> [Int]
    -- ^ stepConsumed values (leaf-to-root order)
    -> Bool
checkKeyDivergence targetKey witnessKey rootJump consumedList =
    case firstDivergence targetKey witnessKey of
        Nothing -> False
        Just divPos ->
            let branchPositions =
                    scanBranchPositions
                        (length rootJump)
                        (reverse consumedList)
            in  divPos `notElem` branchPositions
                    && divPos < length witnessKey

firstDivergence :: Key -> Key -> Maybe Int
firstDivergence [] [] = Nothing
firstDivergence [] _ = Nothing
firstDivergence _ [] = Nothing
firstDivergence (a : as') (b : bs)
    | a /= b = Just 0
    | otherwise = (+ 1) <$> firstDivergence as' bs

scanBranchPositions :: Int -> [Int] -> [Int]
scanBranchPositions = go
  where
    go _ [] = []
    go pos (consumed : rest) =
        pos : go (pos + consumed) rest

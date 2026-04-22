-- |
-- Module      : CSMT.Proof.Insertion
-- Description : Merkle inclusion proof generation (write side)
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- Inclusion-proof generation against a database-backed CSMT. The
-- proof types and pure verification logic live in
-- 'CSMT.Core.Proof'; this module adds only the transactional
-- walker that reads siblings from the KV store.
module CSMT.Proof.Insertion
    ( -- * Types (re-exported from 'CSMT.Core.Proof')
      InclusionProof (..)
    , ProofStep (..)
    , StepView (..)

      -- * Verification (re-exported from 'CSMT.Core.Proof')
    , verifyInclusionProof
    , computeRootHash
    , foldProof

      -- * Generation
    , buildInclusionProof
    )
where

import CSMT.Core.Proof
    ( InclusionProof (..)
    , ProofStep (..)
    , StepView (..)
    , computeRootHash
    , foldProof
    , verifyInclusionProof
    )
import CSMT.Interface
    ( FromKV (..)
    , Indirect (..)
    , Key
    , oppositeDirection
    )
import Control.Lens (view)
import Control.Monad (guard)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.List (isPrefixOf)
import Database.KV.Transaction
    ( GCompare
    , Selector
    , Transaction
    , query
    )

-- |
-- Generate an inclusion proof for a key in the CSMT.
--
-- Looks up the value from the KV column and traverses from root to the
-- target key, collecting sibling hashes at each branch. Returns 'Nothing'
-- if the key is not in the tree.
--
-- Returns both the raw value and the proof, ensuring the proof matches
-- the current state of the tree.
buildInclusionProof
    :: (Monad m, Ord k, GCompare d)
    => Key
    -- ^ Prefix (use @[]@ for root)
    -> FromKV k v a
    -> Selector d k v
    -- ^ KV column to look up the value
    -> Selector d Key (Indirect a)
    -- ^ CSMT column for tree traversal
    -> k
    -> Transaction m cf d ops (Maybe (v, InclusionProof a))
buildInclusionProof pfx FromKV{isoK, fromV, treePrefix} kvSel csmtSel k =
    runMaybeT $ do
        v <- MaybeT $ query kvSel k
        let key = treePrefix v <> view isoK k
            value = fromV v
        Indirect rootJump _ <- MaybeT $ query csmtSel pfx
        guard $ isPrefixOf rootJump key
        steps <- go rootJump $ drop (length rootJump) key
        let proofData =
                InclusionProof
                    { proofKey = key
                    , proofValue = value
                    , proofSteps = reverse steps
                    , proofRootJump = rootJump
                    }
        pure (v, proofData)
  where
    go _ [] = pure []
    go u (x : ks) = do
        Indirect jump _ <- MaybeT $ query csmtSel (u <> [x])
        guard $ isPrefixOf jump ks
        stepSibling <- MaybeT $ query csmtSel (u <> [oppositeDirection x])
        let step =
                ProofStep
                    { stepConsumed = 1 + length jump
                    , stepSibling
                    }
        (step :)
            <$> go
                (u <> (x : jump))
                (drop (length jump) ks)

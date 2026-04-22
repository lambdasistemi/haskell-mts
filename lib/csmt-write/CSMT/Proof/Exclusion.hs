-- |
-- Module      : CSMT.Proof.Exclusion
-- Description : Merkle exclusion proof generation (write side)
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- Exclusion-proof generation against a database-backed CSMT. The
-- 'ExclusionProof' type and pure verification live in
-- 'CSMT.Core.Exclusion'; this module adds the transactional walker
-- that finds a witness leaf whose path diverges from the target
-- key within a jump.
module CSMT.Proof.Exclusion
    ( -- * Types (re-exported from 'CSMT.Core.Exclusion')
      ExclusionProof (..)

      -- * Verification (re-exported from 'CSMT.Core.Exclusion')
    , verifyExclusionProof

      -- * Generation
    , buildExclusionProof
    )
where

import CSMT.Core.Exclusion
    ( ExclusionProof (..)
    , verifyExclusionProof
    )
import CSMT.Core.Proof
    ( InclusionProof (..)
    , ProofStep (..)
    )
import CSMT.Interface
    ( Direction (..)
    , Indirect (..)
    , Key
    , oppositeDirection
    )
import Data.List (isPrefixOf)
import Database.KV.Transaction
    ( GCompare
    , Selector
    , Transaction
    , query
    )

-- |
-- Generate an exclusion proof for a target key.
--
-- Takes a target tree key (already converted via 'isoK') and
-- walks the CSMT to find a witness whose path diverges from
-- the target within a jump.
--
-- Returns 'Nothing' if the key exists in the tree.
buildExclusionProof
    :: (Monad m, GCompare d)
    => Key
    -- ^ Prefix (use @[]@ for root)
    -> Selector d Key (Indirect a)
    -- ^ CSMT column
    -> Key
    -- ^ Target key (tree key, not external key)
    -> Transaction m cf d ops (Maybe (ExclusionProof a))
buildExclusionProof pfx csmtSel targetKey = do
    mroot <- query csmtSel pfx
    case mroot of
        Nothing -> pure $ Just ExclusionEmpty
        Just rootIndirect@(Indirect rootJump _) ->
            if not (rootJump `isPrefixOf` targetKey)
                then do
                    mproof <-
                        buildWitnessFromNode
                            pfx
                            csmtSel
                            rootIndirect
                            []
                    pure $ mkExclusion <$> mproof
                else do
                    let remaining =
                            drop (length rootJump) targetKey
                        pos = pfx <> rootJump
                    walkTree
                        pfx
                        csmtSel
                        targetKey
                        pos
                        remaining
                        []
  where
    mkExclusion wp =
        ExclusionWitness
            { epTargetKey = targetKey
            , epWitnessProof = wp
            }

-- | Walk tree following target key bits, collecting proof steps.
-- When a jump diverges, descend to a leaf for the witness.
walkTree
    :: (Monad m, GCompare d)
    => Key
    -> Selector d Key (Indirect a)
    -> Key
    -> Key
    -> Key
    -> [ProofStep a]
    -> Transaction m cf d ops (Maybe (ExclusionProof a))
walkTree _ _ _ _ [] _ = pure Nothing
walkTree
    pfx
    csmtSel
    targetKey
    pos
    (d : rest)
    stepsAcc = do
        let childPos = pos <> [d]
            sibPos = pos <> [oppositeDirection d]
        mchild <- query csmtSel childPos
        msib <- query csmtSel sibPos
        case (mchild, msib) of
            (Just (Indirect childJump childVal), Just sib) ->
                let step =
                        ProofStep
                            { stepConsumed =
                                1 + length childJump
                            , stepSibling = sib
                            }
                    stepsAcc' = step : stepsAcc
                in  if childJump `isPrefixOf` rest
                        then do
                            let newPos =
                                    childPos <> childJump
                                newRest =
                                    drop
                                        (length childJump)
                                        rest
                            walkTree
                                pfx
                                csmtSel
                                targetKey
                                newPos
                                newRest
                                stepsAcc'
                        else do
                            mproof <-
                                buildWitnessFromNode
                                    childPos
                                    csmtSel
                                    (Indirect childJump childVal)
                                    stepsAcc'
                            pure
                                $ fmap
                                    ( \wp ->
                                        ExclusionWitness
                                            { epTargetKey =
                                                targetKey
                                            , epWitnessProof = wp
                                            }
                                    )
                                    mproof
            _ -> pure Nothing

-- | Build a witness inclusion proof by descending from a node
-- to a leaf, collecting proof steps along the way.
buildWitnessFromNode
    :: (Monad m, GCompare d)
    => Key
    -> Selector d Key (Indirect a)
    -> Indirect a
    -> [ProofStep a]
    -> Transaction m cf d ops (Maybe (InclusionProof a))
buildWitnessFromNode pos csmtSel =
    descend pos
  where
    descend currentPos (Indirect jmp val) acc = do
        let base = currentPos <> jmp
        ml <- query csmtSel (base <> [L])
        mr <- query csmtSel (base <> [R])
        case (ml, mr) of
            (Nothing, Nothing) ->
                let witnessKey = base
                    rj = deriveRootJump witnessKey acc
                    steps = acc
                in  pure
                        $ Just
                            InclusionProof
                                { proofKey = witnessKey
                                , proofValue = val
                                , proofSteps = steps
                                , proofRootJump = rj
                                }
            (Just leftChild, Just rightChild) -> do
                let step =
                        ProofStep
                            { stepConsumed =
                                1 + length (jump leftChild)
                            , stepSibling = rightChild
                            }
                descend
                    (base <> [L])
                    leftChild
                    (step : acc)
            _ -> pure Nothing

-- | Derive the root jump from a witness key and its steps.
-- rootJump = first N bits of the key, where N = len(key) - sum(stepConsumed)
deriveRootJump :: Key -> [ProofStep a] -> Key
deriveRootJump witnessKey steps =
    let totalConsumed = sum (map stepConsumed steps)
        rootJumpLen = length witnessKey - totalConsumed
    in  take rootJumpLen witnessKey

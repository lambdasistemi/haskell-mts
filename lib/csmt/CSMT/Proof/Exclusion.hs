{-# LANGUAGE StrictData #-}

-- |
-- Module      : CSMT.Proof.Exclusion
-- Description : Merkle exclusion proof generation and verification
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- Exclusion proofs demonstrate that a specific key does NOT exist
-- in the tree. The proof embeds an inclusion proof for a witness
-- key whose path diverges from the target key within a jump
-- (path compression region). Since jumps have no branching, the
-- divergence proves the target key has nowhere to exist.
--
-- Verification is a single fold: the inclusion proof's leaf-to-root
-- hash walk runs a divergence callback at each step, producing
-- both a root hash check and a divergence proof in one pass.
module CSMT.Proof.Exclusion
    ( ExclusionProof (..)
    , buildExclusionProof
    , verifyExclusionProof
    )
where

import CSMT.Interface
    ( Direction (..)
    , Hashing (..)
    , Indirect (..)
    , Key
    , oppositeDirection
    )
import CSMT.Proof.Insertion
    ( InclusionProof (..)
    , ProofStep (..)
    , verifyInclusionProof
    )
import Data.List (isPrefixOf)
import Database.KV.Transaction
    ( GCompare
    , Selector
    , Transaction
    , query
    )

-- -----------------------------------------------------------
-- Types
-- -----------------------------------------------------------

-- | An exclusion proof for a key in a CSMT.
data ExclusionProof a
    = -- | The tree is empty — any key is trivially absent.
      ExclusionEmpty
    | -- | A witness key diverges from the target within a jump.
      ExclusionWitness
        { epTargetKey :: Key
        -- ^ The key proven absent
        , epWitnessProof :: InclusionProof a
        -- ^ Inclusion proof for the witness leaf
        }
    deriving (Show, Eq)

-- -----------------------------------------------------------
-- Verification
-- -----------------------------------------------------------

-- |
-- Verify an exclusion proof against a trusted root hash.
--
-- For 'ExclusionEmpty': always returns 'True' (empty tree
-- trivially excludes any key). The caller should check the
-- root hash is empty separately if needed.
--
-- For 'ExclusionWitness': verifies the witness inclusion
-- proof against the trusted root, then checks that the
-- target key diverges from the witness key within a jump
-- region (not at a branch boundary).
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
                    (map stepConsumed (proofSteps epWitnessProof))
        in  hashValid && divergenceValid

-- | Check that the target key diverges from the witness key
-- within a jump region, not at a branch boundary.
--
-- Branch boundaries are determined by the proof structure:
-- the root jump is a jump region, then each step starts with
-- a direction bit (branch) followed by a jump.
checkKeyDivergence
    :: Key
    -- ^ Target key
    -> Key
    -- ^ Witness key
    -> Key
    -- ^ Root jump
    -> [Int]
    -- ^ stepConsumed values (leaf-to-root order, as stored
    -- in proofSteps — reversed here for root-to-leaf scan)
    -> Bool
checkKeyDivergence targetKey witnessKey rootJump consumedList =
    -- Find first position where keys differ
    case firstDivergence targetKey witnessKey of
        Nothing -> False -- keys are identical → key exists
        Just divPos ->
            -- Check divPos is within a jump region
            let branchPositions =
                    scanBranchPositions
                        (length rootJump)
                        (reverse consumedList)
            in  divPos `notElem` branchPositions
                    && divPos < length witnessKey

-- | Find the first position where two keys differ.
firstDivergence :: Key -> Key -> Maybe Int
firstDivergence [] [] = Nothing
firstDivergence [] _ = Nothing
firstDivergence _ [] = Nothing
firstDivergence (a : as') (b : bs)
    | a /= b = Just 0
    | otherwise = (+ 1) <$> firstDivergence as' bs

-- | Compute the branch boundary positions from the proof
-- structure. Each step's direction bit is at a branch.
-- Expects consumed values in root-to-leaf order. The branch
-- positions are at cumulative offsets from the root jump.
scanBranchPositions :: Int -> [Int] -> [Int]
scanBranchPositions =
    go
  where
    go _ [] = []
    go pos (consumed : rest) =
        pos : go (pos + consumed) rest

-- -----------------------------------------------------------
-- Generation
-- -----------------------------------------------------------

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
                    -- Root jump diverges: descend to any leaf
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
walkTree _ _ _ _ [] _ =
    -- Target key fully consumed in the tree — key exists
    pure Nothing
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
                            -- Jump matches — continue deeper
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
                            -- Jump diverges — find witness leaf
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
            _ ->
                -- Missing child or sibling — malformed tree
                pure Nothing

-- | Build a witness inclusion proof by descending from a node
-- to a leaf, collecting proof steps along the way.
buildWitnessFromNode
    :: (Monad m, GCompare d)
    => Key
    -- ^ Current position in the tree
    -> Selector d Key (Indirect a)
    -> Indirect a
    -- ^ The node at the current position
    -> [ProofStep a]
    -- ^ Steps accumulated so far (in reverse, leaf-first)
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
                -- Leaf node: build the inclusion proof
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
                -- Branch: descend left, use right as sibling
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
            _ ->
                -- One child missing — malformed tree
                pure Nothing

-- | Derive the root jump from a witness key and its steps.
-- rootJump = first N bits of the key, where N = len(key) - sum(stepConsumed)
deriveRootJump :: Key -> [ProofStep a] -> Key
deriveRootJump witnessKey steps =
    let totalConsumed = sum (map stepConsumed steps)
        rootJumpLen = length witnessKey - totalConsumed
    in  take rootJumpLen witnessKey

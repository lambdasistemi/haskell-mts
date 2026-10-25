-- |
-- Module      : CSMT.Test.MalformedExclusion
-- Description : Random trees and damaged exclusion proofs
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- Generators for properties about exclusion proofs that are not
-- what the tree produced. The trees are those of
-- "CSMT.Test.Malformed" and every honest proof comes from
-- 'buildExclusionProof'. The proofs given to the verifier are
-- close to honest ones: the target of a proof is switched to a key
-- the tree holds, the witness is the honest proof of another key,
-- the proof of a branch of the tree posing as a witness, a witness
-- of another tree or a witness damaged in any field.
module CSMT.Test.MalformedExclusion
    ( -- * Honest proofs
      exclusionOf

      -- * The model of a tree
    , startsAHeldKey

      -- * Targets
    , genAbsentTarget
    , genHeldTarget

      -- * Damaged proofs
    , genHeldExclusion
    , genForeignExclusion
    , genWildExclusion
    , innerNodeProof
    , shrinkExclusion

      -- * Counterexample rendering
    , describeExclusion
    )
where

import CSMT.Backend.Pure (pureDatabase)
import CSMT.Backend.Standalone (Standalone (StandaloneCSMTCol))
import CSMT.Hashes (Hash, hashHashing)
import CSMT.Interface
    ( Indirect (..)
    , Key
    , addWithDirection
    )
import CSMT.Proof.Exclusion
    ( ExclusionProof (..)
    , buildExclusionProof
    )
import CSMT.Proof.Insertion
    ( InclusionProof (..)
    , ProofStep (..)
    )
import CSMT.Test.Lib
    ( evalPureFromEmptyDB
    , hashCodecs
    , insertMHash
    )
import CSMT.Test.Malformed
    ( Tree (..)
    , damageStructure
    , describeProof
    , genBits
    , genHash
    , genNear
    , genWild
    , mkTree
    , renderKey
    , shrinkProof
    )
import Data.List (isPrefixOf, nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Database.KV.Transaction (runTransactionUnguarded)
import Test.QuickCheck
    ( Gen
    , choose
    , elements
    , frequency
    , oneof
    , suchThat
    , vectorOf
    )

-- | The proof the producer builds for a key. The tree is built
-- again from its pairs, in key order.
exclusionOf :: Tree -> Key -> Maybe (ExclusionProof Hash)
exclusionOf Tree{treeLeaves} target = evalPureFromEmptyDB $ do
    mapM_ (uncurry insertMHash) (Map.toList treeLeaves)
    runTransactionUnguarded (pureDatabase hashCodecs)
        $ buildExclusionProof [] StandaloneCSMTCol target

-- | Whether the key is held by the tree or is the beginning of a
-- held key. The tree has no leaves under a key for which this is
-- false.
startsAHeldKey :: Tree -> Key -> Bool
startsAHeldKey Tree{treeLeaves} target =
    any (target `isPrefixOf`) (Map.keys treeLeaves)

-- | A key of the length of the tree's keys that the tree does not
-- hold, usually a neighbour of a held key.
genAbsentTarget :: Tree -> Gen Key
genAbsentTarget Tree{treeKeyBits, treeLeaves} = do
    k0 <- elements (Map.keys treeLeaves)
    genNear treeKeyBits k0
        `suchThat` (`Map.notMember` treeLeaves)

-- | A key the tree holds, or the beginning of one.
genHeldTarget :: Tree -> Gen Key
genHeldTarget Tree{treeLeaves} = do
    k <- elements (Map.keys treeLeaves)
    frequency
        [ (3, pure k)
        , (2, (`take` k) <$> choose (0, length k - 1))
        ]

-- | The proof of a branch of the tree, from the honest proof of a
-- held key with its first steps dropped and the hash of the branch
-- they lead to as the value. The key is shorter than those the tree
-- holds. Leaf and branch hashes are not domain-separated, so this
-- is a valid inclusion proof for the root.
innerNodeProof :: InclusionProof Hash -> Int -> InclusionProof Hash
innerNodeProof
    proof@InclusionProof
        { proofKey
        , proofValue
        , proofSteps
        , proofRootJump
        }
    n =
        proof
            { proofKey =
                take (length proofKey - sum (stepConsumed <$> dropped)) proofKey
            , proofValue =
                climb
                    proofValue
                    (reverse (drop (length proofRootJump) proofKey))
                    dropped
            , proofSteps = rest
            }
      where
        (dropped, rest) = splitAt n proofSteps
        climb hash _ [] = hash
        climb hash revKey (ProofStep{stepConsumed, stepSibling} : more) =
            case reverse consumed of
                direction : stepJump ->
                    climb
                        ( addWithDirection
                            hashHashing
                            direction
                            (Indirect stepJump hash)
                            stepSibling
                        )
                        remaining
                        more
                [] -> hash
          where
            (consumed, remaining) = splitAt stepConsumed revKey

genInnerNode :: Tree -> Gen (InclusionProof Hash)
genInnerNode Tree{treeProofs} = do
    proof <- elements (Map.elems treeProofs)
    innerNodeProof proof <$> choose (0, length (proofSteps proof))

-- | Any witness for a statement about the tree: the honest proof of
-- a held key, the proof of a branch, either of them damaged, the
-- proof of a key of another tree, or any damaged proof.
genWitness :: Tree -> Tree -> Gen (InclusionProof Hash)
genWitness t o =
    frequency
        [ (4, elements (Map.elems (treeProofs t)))
        , (3, genInnerNode t)
        , (2, elements (Map.elems (treeProofs t)) >>= damageStructure t o)
        , (2, genInnerNode t >>= damageStructure t o)
        , (2, elements (Map.elems (treeProofs o)))
        , (1, genWild t o)
        ]

-- | An exclusion proof for a target the tree holds or begins, to be
-- refused: the witness is any witness.
genHeldExclusion :: Tree -> Tree -> Gen (ExclusionProof Hash)
genHeldExclusion t o =
    ExclusionWitness <$> genHeldTarget t <*> genWitness t o

-- | An exclusion proof of a key that the tree holds, built for a
-- tree that does not hold it: the same pairs without the key, with
-- some neighbours of it, or the neighbours alone.
genForeignExclusion :: Tree -> Gen (ExclusionProof Hash)
genForeignExclusion Tree{treeKeyBits, treeLeaves} = do
    k <- elements (Map.keys treeLeaves)
    let rest = Map.toList (Map.delete k treeLeaves)
    neighbours <-
        oneof
            [ pure []
            , do
                n <- choose (1, 3)
                ks <- vectorOf n (genNear treeKeyBits k)
                traverse
                    (\k' -> (k',) <$> genHash)
                    (nub (filter (/= k) ks))
            ]
    let leaves = Map.toList (Map.fromList (rest <> neighbours))
    others <-
        if null leaves
            then do
                k' <- genNear treeKeyBits k `suchThat` (/= k)
                (\v -> [(k', v)]) <$> genHash
            else pure leaves
    pure
        $ fromMaybe
            ExclusionEmpty
            (exclusionOf (mkTree treeKeyBits others) k)

-- | An exclusion proof with any field damaged, to be given to the
-- verifier without any expectation about its verdict: any target,
-- including keys of any length, and any witness.
genWildExclusion :: Tree -> Tree -> Gen (ExclusionProof Hash)
genWildExclusion t@Tree{treeKeyBits, treeLeaves} o =
    frequency
        [ (1, pure ExclusionEmpty)
        , (9, ExclusionWitness <$> genTarget <*> witness)
        ]
  where
    witness = oneof [genWitness t o, genWild t o]
    genTarget =
        oneof
            [ genHeldTarget t
            , genAbsentTarget t
            , choose (0, treeKeyBits + 9) >>= genBits
            , (<>)
                <$> elements (Map.keys treeLeaves)
                <*> (choose (1, 9) >>= genBits)
            ]

-- | Smaller exclusion proofs for a failing one. The target is kept,
-- so a shrunk counterexample stays in the class it was generated
-- for.
shrinkExclusion :: ExclusionProof Hash -> [ExclusionProof Hash]
shrinkExclusion ExclusionEmpty = []
shrinkExclusion (ExclusionWitness target witness) =
    ExclusionWitness target <$> shrinkProof witness

-- | An exclusion proof with keys as bit strings.
describeExclusion :: ExclusionProof Hash -> String
describeExclusion ExclusionEmpty = "ExclusionEmpty"
describeExclusion (ExclusionWitness target witness) =
    "target "
        <> renderKey target
        <> " witness "
        <> describeProof witness

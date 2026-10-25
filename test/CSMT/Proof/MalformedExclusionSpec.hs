-- |
-- Module      : CSMT.Proof.MalformedExclusionSpec
-- Description : Exclusion proofs that are not what the tree produced
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- A verifier is asked about exclusion proofs it did not build.
-- Starting from honest proofs of random trees and switching the
-- target to a key the tree holds, giving the witness of another
-- key, of another tree, or the proof of a branch of the tree, and
-- damaging any field, no proof may exclude a key the tree holds or
-- begins, none is accepted for the root of a tree that holds the
-- key it excludes, and none makes the verifier throw. Every key a
-- tree does not hold is excluded by the proof built for it.
--
-- Every tree holds keys of one length. Leaf and branch hashes are
-- not domain-separated, so a branch of the tree can pass as the
-- witness of a shorter key; the properties use such witnesses only
-- against targets that the tree holds or begins.
module CSMT.Proof.MalformedExclusionSpec (spec) where

import CSMT.Hashes (Hash, byteStringToKey, hashHashing, mkHash)
import CSMT.Interface (Key)
import CSMT.Proof.Exclusion
    ( ExclusionProof (..)
    , verifyExclusionProof
    )
import CSMT.Proof.Insertion
    ( InclusionProof (..)
    , verifyInclusionProof
    )
import CSMT.Test.Malformed
    ( Tree (..)
    , describeTree
    , genHash
    , genTree
    , genTreeOfKeyBits
    , honestProof
    , mkTree
    )
import CSMT.Test.MalformedExclusion
    ( describeExclusion
    , exclusionOf
    , genAbsentTarget
    , genForeignExclusion
    , genHeldExclusion
    , genWildExclusion
    , innerNodeProof
    , shrinkExclusion
    )
import Control.Exception (SomeException, evaluate, try)
import Data.ByteString qualified as B
import Data.Either (fromRight)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
    ( Gen
    , Property
    , choose
    , conjoin
    , counterexample
    , cover
    , elements
    , forAll
    , forAllShrink
    , ioProperty
    , oneof
    , property
    , vectorOf
    , (===)
    )

type Scenario = (Tree, ExclusionProof Hash)

genScenario
    :: (Tree -> Tree -> Gen (ExclusionProof Hash)) -> Gen Scenario
genScenario genProof = do
    t <- genTree
    o <- oneof [genTree, genTreeOfKeyBits (treeKeyBits t)]
    (t,) <$> genProof t o

shrinkScenario :: Scenario -> [Scenario]
shrinkScenario (t, e) = (t,) <$> shrinkExclusion e

excludes :: Tree -> ExclusionProof Hash -> Bool
excludes t = verifyExclusionProof hashHashing (treeRoot t)

-- | Whether the witness proof of the exclusion proof is a valid
-- inclusion proof for the root of the tree, so that the verdict is
-- left to the comparison of the target with the witness.
witnessVerifies :: Tree -> ExclusionProof Hash -> Bool
witnessVerifies t = \case
    ExclusionEmpty -> False
    ExclusionWitness{epWitnessProof} ->
        verifyInclusionProof hashHashing (treeRoot t) epWitnessProof

-- | The verdict, where an exception counts as a refusal: not
-- throwing is a property of its own.
accepts :: Tree -> ExclusionProof Hash -> IO Bool
accepts t e =
    fromRight False
        <$> try @SomeException (evaluate (excludes t e))

describing :: Tree -> ExclusionProof Hash -> Property -> Property
describing t e =
    counterexample (describeTree t <> "\n" <> describeExclusion e)

refusedFor
    :: (Tree -> Tree -> Gen (ExclusionProof Hash)) -> Property
refusedFor genProof =
    forAllShrink (genScenario genProof) shrinkScenario $ \(t, e) ->
        ioProperty $ do
            ok <- accepts t e
            pure $ describing t e (property (not ok))

byteKey :: Word8 -> Key
byteKey = byteStringToKey . B.singleton

leaf :: Word8 -> Hash
leaf = mkHash . B.singleton

held :: Tree -> Key -> InclusionProof Hash
held t k =
    fromMaybe
        (error "held: key is not in the tree")
        (honestProof t k)

spec :: Spec
spec = describe "exclusion proofs over random trees" $ do
    prop
        "a key the tree does not hold is excluded by the proof built for it"
        $ forAll genTree
        $ \t ->
            forAll (vectorOf 6 (genAbsentTarget t)) $ \ks ->
                conjoin
                    [ case exclusionOf t k of
                        Nothing ->
                            counterexample
                                ( describeTree t
                                    <> "\nno proof built for an absent key"
                                )
                                (property False)
                        Just e -> describing t e (property (excludes t e))
                    | k <- ks
                    ]
    prop "a key the tree holds or begins is never excluded"
        $ forAllShrink
            (genScenario genHeldExclusion)
            shrinkScenario
        $ \(t, e) ->
            cover
                30
                (witnessVerifies t e)
                "the witness proves a node of the tree, the target decides"
                $ ioProperty
                $ do
                    ok <- accepts t e
                    pure $ describing t e (property (not ok))
    prop
        "the proof of a branch of the tree passes for a proof of the root"
        $ forAll genTree
        $ \t ->
            forAll (elements (Map.elems (treeProofs t))) $ \p ->
                forAll (choose (0, length (proofSteps p))) $ \n ->
                    counterexample (describeTree t)
                        $ verifyInclusionProof
                            hashHashing
                            (treeRoot t)
                            (innerNodeProof p n)
                            === True
    prop
        "a proof built for a tree without the key is refused for a tree that holds it"
        $ forAllShrink
            ( do
                t <- genTree
                (t,) <$> genForeignExclusion t
            )
            shrinkScenario
        $ \(t, e) -> ioProperty $ do
            ok <- accepts t e
            pure $ describing t e (property (not ok))
    prop "ExclusionEmpty is refused for the root of every tree"
        $ forAll genTree
        $ \t ->
            describing
                t
                ExclusionEmpty
                (property (not (excludes t ExclusionEmpty)))
    prop "ExclusionEmpty is refused for every root"
        $ forAll ((,) <$> genTree <*> genHash)
        $ \(t, h) ->
            property
                $ not
                    (verifyExclusionProof hashHashing h ExclusionEmpty)
                    && not (excludes t ExclusionEmpty)
    prop "no exclusion proof makes the verifier throw"
        $ forAllShrink (genScenario genWildExclusion) shrinkScenario
        $ \(t, e) -> ioProperty $ do
            r <-
                try @SomeException
                    $ evaluate
                    $ fromEnum (excludes t e)
            pure
                $ describing t e
                $ counterexample (either show (const "") r)
                $ property
                $ either (const False) (const True) r
    describe "kept counterexamples" $ do
        let pair =
                mkTree
                    8
                    [(byteKey 0x80, leaf 1), (byteKey 0x81, leaf 2)]
            single = mkTree 8 [(byteKey 0x01, leaf 1)]
            wide =
                mkTree
                    8
                    [ (byteKey 0x00, leaf 1)
                    , (byteKey 0x01, leaf 2)
                    , (byteKey 0x80, leaf 3)
                    ]
            witness0x80 = held pair (byteKey 0x80)
        it "ExclusionEmpty is not accepted for a tree that holds the key"
            $ excludes pair ExclusionEmpty
            `shouldBe` False
        it "ExclusionEmpty is not accepted for a tree of one key"
            $ excludes single ExclusionEmpty
            `shouldBe` False
        it "ExclusionEmpty is not accepted for a root nobody holds"
            $ verifyExclusionProof hashHashing (leaf 9) ExclusionEmpty
            `shouldBe` False
        it "a key that diverges at a branch of the witness is not excluded"
            $ excludes
                pair
                (ExclusionWitness (byteKey 0x81) witness0x80)
            `shouldBe` False
        it "a key is not excluded by its own proof"
            $ excludes
                pair
                (ExclusionWitness (byteKey 0x80) witness0x80)
            `shouldBe` False
        it "a held key is not excluded by the proof of a branch above it"
            $ let branch =
                    innerNodeProof
                        (held wide (byteKey 0x00))
                        1
              in  excludes wide (ExclusionWitness (byteKey 0x01) branch)
                    `shouldBe` False
        it "a held key is not excluded by the proof of the root"
            $ let root' =
                    innerNodeProof
                        (held wide (byteKey 0x00))
                        (length (proofSteps (held wide (byteKey 0x00))))
              in  excludes wide (ExclusionWitness (byteKey 0x80) root')
                    `shouldBe` False
        it "the beginning of a held key is not excluded"
            $ excludes
                pair
                (ExclusionWitness (take 7 (byteKey 0x81)) witness0x80)
            `shouldBe` False
        it
            "a proof built for the tree without the key is refused for the tree with it"
            $ let without = mkTree 8 [(byteKey 0x80, leaf 1)]
                  e = exclusionOf without (byteKey 0x81)
              in  fmap (excludes pair) e `shouldBe` Just False
        it "a key that diverges inside a jump is excluded"
            $ fmap (excludes wide) (exclusionOf wide (byteKey 0x40))
            `shouldBe` Just True

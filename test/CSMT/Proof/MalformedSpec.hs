-- |
-- Module      : CSMT.Proof.MalformedSpec
-- Description : Inclusion proofs that are not what the tree produced
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- A verifier is asked about proofs it did not build. Starting from
-- honest proofs of random trees (the key's own, a neighbour's, those
-- of another tree) and damaging any of their fields, no proof may be
-- accepted for a statement the tree does not hold, a held statement
-- may be proved only by the proof the tree produces, and no proof,
-- whatever its step sizes, may make the verifier throw. Honest
-- proofs of every member of every tree are accepted.
--
-- Every tree holds keys of one length. Leaf and branch hashes are
-- not domain-separated, so the properties never use a node hash of
-- the tree as the value of a key of another length.
module CSMT.Proof.MalformedSpec (spec) where

import CSMT.Hashes
    ( Hash
    , byteStringToKey
    , hashHashing
    , mkHash
    , renderHash
    )
import CSMT.Interface (Direction (..), Indirect (..), Key)
import CSMT.Proof.Insertion
    ( InclusionProof (..)
    , ProofStep (..)
    , computeRootHash
    , foldProof
    , verifyInclusionProof
    )
import CSMT.Test.Malformed
    ( Tree (..)
    , describeProof
    , describeTree
    , genAbsentKey
    , genHeldStatement
    , genOtherLengthKey
    , genTree
    , genTreeOfKeyBits
    , genWild
    , genWrongValue
    , honestProof
    , mkTree
    , shrinkProof
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
    , conjoin
    , counterexample
    , forAll
    , forAllShrink
    , ioProperty
    , oneof
    , property
    , (===)
    )

type Scenario = (Tree, InclusionProof Hash)

genScenario
    :: (Tree -> Tree -> Gen (InclusionProof Hash)) -> Gen Scenario
genScenario genProof = do
    t <- genTree
    o <- oneof [genTree, genTreeOfKeyBits (treeKeyBits t)]
    (t,) <$> genProof t o

shrinkScenario :: Scenario -> [Scenario]
shrinkScenario (t, p) = (t,) <$> shrinkProof p

verifies :: Tree -> InclusionProof Hash -> Bool
verifies t = verifyInclusionProof hashHashing (treeRoot t)

-- | The verdict, where an exception counts as a refusal: not
-- throwing is a property of its own.
accepts :: Tree -> InclusionProof Hash -> IO Bool
accepts t p =
    fromRight False
        <$> try @SomeException (evaluate (verifies t p))

describing :: Tree -> InclusionProof Hash -> Property -> Property
describing t p =
    counterexample (describeTree t <> "\n" <> describeProof p)

refusedFor
    :: (Tree -> Tree -> Gen (InclusionProof Hash)) -> Property
refusedFor genProof =
    forAllShrink (genScenario genProof) shrinkScenario $ \(t, p) ->
        ioProperty $ do
            ok <- accepts t p
            pure $ describing t p (property (not ok))

insertAt :: Int -> Key -> Key -> Key
insertAt i extra k = let (a, b) = splitAt i k in a <> extra <> b

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
spec = describe "inclusion proofs over random trees" $ do
    prop "every member is proved by the proof built for it"
        $ forAll genTree
        $ \t ->
            conjoin
                [ describing t p
                    $ ( proofKey p
                      , Just (proofValue p)
                      , verifies t p
                      )
                        === (k, Map.lookup k (treeLeaves t), True)
                | (k, p) <- Map.toList (treeProofs t)
                ]
    prop "a wrong value for a held key is never proved"
        $ refusedFor genWrongValue
    prop "a key the tree does not hold is never proved"
        $ refusedFor genAbsentKey
    prop "a key of another length is never proved"
        $ refusedFor genOtherLengthKey
    prop "a held statement is proved only by the proof built for it"
        $ forAllShrink
            (genScenario genHeldStatement)
            shrinkScenario
        $ \(t, p) -> ioProperty $ do
            ok <- accepts t p
            pure
                $ describing t p
                $ property
                $ not ok
                    || Just p == Map.lookup (proofKey p) (treeProofs t)
    prop "no proof makes the verifier throw"
        $ forAllShrink (genScenario genWild) shrinkScenario
        $ \(t, p) -> ioProperty $ do
            let verdict = verifies t p
                (hashed, steps) =
                    foldProof hashHashing p (\n _ -> n + 1 :: Int) 0
            r <-
                try @SomeException
                    $ evaluate
                    $ fromEnum verdict
                        + steps
                        + B.length (renderHash hashed)
                        + B.length
                            ( renderHash
                                (computeRootHash hashHashing p)
                            )
            pure
                $ describing t p
                $ counterexample (either show (const "") r)
                $ property
                $ either (const False) (const True) r
    describe "kept counterexamples" $ do
        let single = mkTree 8 [(byteKey 0x01, leaf 1)]
            pair =
                mkTree
                    8
                    [(byteKey 0x80, leaf 1), (byteKey 0x81, leaf 2)]
            singleProof = held single (byteKey 0x01)
            pairProof = held pair (byteKey 0x81)
            withExtraStep size =
                pairProof
                    { proofSteps =
                        proofSteps pairProof
                            <> [ProofStep size (Indirect [] (leaf 3))]
                    }
            withStep f =
                pairProof
                    { proofSteps = f <$> proofSteps pairProof
                    }
        it "the proof of one key does not prove another key"
            $ verifies single singleProof{proofKey = byteKey 0x02}
            `shouldBe` False
        it "a root jump that is not the leading bits of the key is refused"
            $ verifies pair pairProof{proofKey = byteKey 0x01}
            `shouldBe` False
        it "key bits left over after the last step are refused"
            $ verifies
                pair
                pairProof
                    { proofKey =
                        insertAt
                            (length (proofRootJump pairProof))
                            [L]
                            (proofKey pairProof)
                    }
            `shouldBe` False
        it "a key shorter than the root jump is refused"
            $ verifies
                single
                singleProof{proofKey = take 7 (proofKey singleProof)}
            `shouldBe` False
        it "a step consuming no bits is refused, not thrown"
            $ verifies pair (withStep (\s -> s{stepConsumed = 0}))
            `shouldBe` False
        it "a step with a negative size is refused, not thrown"
            $ verifies pair (withStep (\s -> s{stepConsumed = -1}))
            `shouldBe` False
        it "a step reaching past the end of the key is refused"
            $ verifies pair (withStep (\s -> s{stepConsumed = 2}))
            `shouldBe` False
        it "a step of no bits added to an honest proof is refused, not thrown"
            $ verifies pair (withExtraStep 0)
            `shouldBe` False
        it "a step of size -1 added to an honest proof is refused, not thrown"
            $ verifies pair (withExtraStep (-1))
            `shouldBe` False
        it "more steps than key bits are refused, not thrown"
            $ verifies
                pair
                pairProof{proofSteps = proofSteps pairProof <> proofSteps pairProof}
            `shouldBe` False

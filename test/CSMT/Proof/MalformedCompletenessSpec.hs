-- |
-- Module      : CSMT.Proof.MalformedCompletenessSpec
-- Description : Completeness proofs that are not what the tree produced
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- A verifier of completeness is asked about a prefix, the leaves it
-- received and a proof it did not build. Starting from the honest
-- statements of random trees and damaging the leaves, the prefix,
-- the merge operations and the inclusion steps, or giving the proof
-- of another prefix, no statement is accepted unless its leaves are
-- exactly the leaves the tree holds under the prefix, in ascending
-- key order, and none makes a verifier throw. The proof built for a
-- prefix verifies, through the fold and through the bytes.
--
-- The order of the leaves is part of the statement: they are given
-- as 'CSMT.Proof.Completeness.collectValues' returns them, by
-- ascending key, and the merge operations index that list.
--
-- Every tree holds keys of one length, and so do the leaves given.
-- Leaf and branch hashes are not domain-separated, so a branch of
-- the tree given as a leaf with a shorter key is outside what a
-- verifier can refuse; the generators never give the hash of a node
-- as a leaf.
module CSMT.Proof.MalformedCompletenessSpec (spec) where

import CSMT.Core.CBOR (renderCompletenessProof)
import CSMT.Core.Completeness
    ( CompletenessProof (..)
    , foldCompletenessProof
    , foldMergeOps
    )
import CSMT.Hashes
    ( Hash
    , byteStringToKey
    , hashHashing
    , mkHash
    , renderHash
    )
import CSMT.Interface
    ( Direction (..)
    , Hashing (..)
    , Indirect (..)
    , Key
    )
import CSMT.Proof.Insertion (ProofStep (..))
import CSMT.Test.Malformed
    ( Tree (..)
    , describeTree
    , genTree
    , genTreeOfKeyBits
    , mkTree
    )
import CSMT.Test.MalformedBytes (damageBytes)
import CSMT.Test.MalformedCompleteness
    ( Statement (..)
    , collectedUnder
    , completenessOf
    , damageSteps
    , describeStatement
    , genDamagedStatement
    , genOtherPrefix
    , genPrefix
    , genWildStatement
    , honestStatement
    , leavesUnder
    , shrinkStatement
    )
import CSMT.Verify qualified as Verify
import Control.Exception (SomeException, evaluate, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
    ( Gen
    , Property
    , conjoin
    , counterexample
    , cover
    , discard
    , elements
    , forAll
    , forAllShrink
    , frequency
    , ioProperty
    , oneof
    , property
    , suchThat
    , tabulate
    , (===)
    )

-- | Whether the fold accepts the statement for the tree.
accepts :: Tree -> Statement -> Bool
accepts Tree{treeRoot} Statement{stPrefix, stLeaves, stProof} =
    foldCompletenessProof
        hashHashing
        treeRoot
        stPrefix
        stLeaves
        stProof
        == Just treeRoot

-- | Whether the byte-level verifier accepts the leaves for the
-- tree, with these bytes as the proof.
acceptsBytes :: Tree -> Key -> [Indirect Hash] -> ByteString -> Bool
acceptsBytes Tree{treeRoot} =
    Verify.verifyCompletenessProof (renderHash treeRoot)

-- | Whether the byte-level verifier accepts the statement with its
-- proof rendered.
acceptsRendered :: Tree -> Statement -> Bool
acceptsRendered t Statement{stPrefix, stLeaves, stProof} =
    acceptsBytes t stPrefix stLeaves (renderCompletenessProof stProof)

-- | The verdict, where an exception counts as a refusal: not
-- throwing is a property of its own.
verdict :: Bool -> IO Bool
verdict b = fromRight False <$> try @SomeException (evaluate b)

genOther :: Tree -> Gen Tree
genOther t = oneof [genTree, genTreeOfKeyBits (treeKeyBits t)]

type Scenario = (Tree, Statement, [String])

genScenario :: Gen Scenario
genScenario = do
    t <- genTree
    o <- genOther t
    (s, names) <- genDamagedStatement t o
    pure (t, s, names)

shrinkScenario :: Scenario -> [Scenario]
shrinkScenario (t, s, names) =
    [(t, s', names) | s' <- shrinkStatement s]

describing :: Tree -> Statement -> Property -> Property
describing t s =
    counterexample (describeTree t <> "\n" <> describeStatement s)

-- | The bytes of the proof, damaged as bytes about half the time.
genProofBytes :: Statement -> Gen ByteString
genProofBytes Statement{stProof} =
    frequency
        [ (1, pure rendered)
        , (1, damageBytes rendered)
        ]
  where
    rendered = renderCompletenessProof stProof

leavesOf :: Tree -> Statement -> [Indirect Hash]
leavesOf t Statement{stPrefix} = leavesUnder t stPrefix

byteKey :: Word8 -> Key
byteKey = byteStringToKey . B.singleton

leaf :: Word8 -> Hash
leaf = mkHash . B.singleton

-- | The statement the producer gives for the prefix of a tree.
honest :: Tree -> Key -> Statement
honest t p =
    fromMaybe
        (error "honest: the producer builds no proof for the prefix")
        (honestStatement t p)

-- | The statement is refused by the fold and by the byte-level
-- verifier, and neither throws.
refused :: Tree -> Statement -> IO ()
refused t s = do
    evaluate (accepts t s) `shouldReturn` False
    evaluate (acceptsRendered t s) `shouldReturn` False

-- | The statement is accepted by the fold and by the byte-level
-- verifier.
accepted :: Tree -> Statement -> IO ()
accepted t s = do
    evaluate (accepts t s) `shouldReturn` True
    evaluate (acceptsRendered t s) `shouldReturn` True

withOps
    :: ([(Int, Int)] -> [(Int, Int)]) -> Statement -> Statement
withOps f s@Statement{stProof = CompletenessWitness ops steps} =
    s{stProof = CompletenessWitness (f ops) steps}
withOps _ s = s

withSteps
    :: ([ProofStep Hash] -> [ProofStep Hash]) -> Statement -> Statement
withSteps f s@Statement{stProof = CompletenessWitness ops steps} =
    s{stProof = CompletenessWitness ops (f steps)}
withSteps _ s = s

stepsOf :: Statement -> [ProofStep Hash]
stepsOf Statement{stProof = CompletenessWitness _ steps} = steps
stepsOf _ = []

mergeOpsOf :: Statement -> [(Int, Int)]
mergeOpsOf Statement{stProof = CompletenessWitness ops _} = ops
mergeOpsOf _ = []

spec :: Spec
spec = describe "completeness proofs over random trees" $ do
    prop
        "the leaves of an accepted statement are the leaves under its prefix"
        $ forAllShrink genScenario shrinkScenario
        $ \(t, s, names) ->
            tabulate "damages" names
                $ ioProperty
                $ do
                    ok <- verdict (accepts t s)
                    pure
                        $ describing t s
                        $ property
                        $ not ok || stLeaves s == leavesOf t s
    prop
        "the leaves of a statement accepted from bytes are the leaves under its prefix"
        $ forAll genScenario
        $ \(t, s, names) ->
            forAll (genProofBytes s) $ \bytes ->
                tabulate "damages" names
                    $ ioProperty
                    $ do
                        ok <-
                            verdict
                                ( acceptsBytes
                                    t
                                    (stPrefix s)
                                    (stLeaves s)
                                    bytes
                                )
                        pure
                            $ describing t s
                            $ counterexample (show bytes)
                            $ property
                            $ not ok || stLeaves s == leavesOf t s
    prop "no statement makes a verifier throw"
        $ forAll genTree
        $ \t ->
            forAll (genOther t) $ \o ->
                forAll (genWildStatement t o) $ \s ->
                    forAll (genProofBytes s) $ \bytes ->
                        ioProperty $ do
                            r <-
                                try @SomeException
                                    $ evaluate
                                    $ fromEnum (accepts t s)
                                        + fromEnum (acceptsRendered t s)
                                        + fromEnum
                                            ( acceptsBytes
                                                t
                                                (stPrefix s)
                                                (stLeaves s)
                                                bytes
                                            )
                            pure
                                $ describing t s
                                $ counterexample (either show (const "") r)
                                $ property
                                $ either (const False) (const True) r
    prop
        "a proof for one prefix is refused for another prefix unless the leaves under both coincide"
        $ forAll genTree
        $ \t ->
            forAll (genOther t) $ \o ->
                forAll (genPrefix t) $ \p ->
                    forAll (genOtherPrefix t o p) $ \p' ->
                        case completenessOf t p of
                            Nothing ->
                                counterexample "no proof is built" False
                            Just proof ->
                                forAll
                                    ( elements
                                        [leavesUnder t p, leavesUnder t p']
                                    )
                                    $ \ls ->
                                        let s = Statement p' ls proof
                                        in  ioProperty $ do
                                                ok <- verdict (accepts t s)
                                                pure
                                                    $ describing t s
                                                    $ property
                                                    $ not ok
                                                        || leavesUnder t p
                                                            == leavesUnder t p'
    prop
        "steps other than those built for the prefix are refused for the leaves under it"
        $ forAll genTree
        $ \t ->
            forAll (genOther t) $ \o ->
                forAll (genPrefix t) $ \p ->
                    case honestStatement t p of
                        Just s@Statement{stProof = CompletenessWitness _ steps} ->
                            forAll
                                ( damageSteps t o s
                                    `suchThat` ((/= steps) . stepsOf)
                                )
                                $ \s' -> ioProperty $ do
                                    ok <- verdict (accepts t s')
                                    pure
                                        $ describing t s'
                                        $ property
                                        $ not ok
                        _ -> discard
    prop
        "merge operations that do not merge every leaf into the first are refused, not thrown"
        $ forAll genTree
        $ \t ->
            forAll (genOther t) $ \o ->
                forAll (genWildStatement t o) $ \s ->
                    ioProperty $ do
                        let ops = mergeOpsOf s
                            leaves = stLeaves s
                        r <-
                            try @SomeException
                                $ evaluate
                                $ foldMergeOps (combineHash hashHashing) leaves ops
                        pure
                            $ describing t s
                            $ counterexample (either show (const "") r)
                            $ property
                            $ case r of
                                Left _ -> False
                                Right Nothing -> True
                                Right (Just _) -> length ops + 1 == length leaves
    prop "the proof built for a prefix verifies"
        $ forAll genTree
        $ \t ->
            forAll (genPrefix t) $ \p ->
                let ls = leavesUnder t p
                    steps = maybe [] stepsOf (honestStatement t p)
                in  cover 5 (null p) "the empty prefix"
                        $ cover 10 (null ls) "no leaf under the prefix"
                        $ cover 10 (length ls == 1) "one leaf"
                        $ cover 10 (length steps >= 2) "two steps or more"
                        $ counterexample (describeTree t)
                        $ case honestStatement t p of
                            Nothing ->
                                counterexample "no proof is built" False
                            Just s ->
                                describing t s
                                    $ conjoin
                                        [ counterexample "fold" (accepts t s === True)
                                        , counterexample "bytes" (acceptsRendered t s === True)
                                        , counterexample "collected leaves" (collectedUnder t p === ls)
                                        ]
    describe "kept counterexamples" $ do
        let pair =
                mkTree
                    8
                    [(byteKey 0x80, leaf 1), (byteKey 0x81, leaf 2)]
            three =
                mkTree
                    8
                    [ (byteKey 0x00, leaf 1)
                    , (byteKey 0x01, leaf 2)
                    , (byteKey 0x80, leaf 3)
                    ]
            skewed =
                mkTree
                    8
                    [ (byteKey 0x00, leaf 1)
                    , (byteKey 0x40, leaf 2)
                    , (byteKey 0x41, leaf 3)
                    ]
            twins =
                mkTree
                    8
                    [(byteKey 0x00, leaf 1), (byteKey 0x80, leaf 1)]
            single = mkTree 8 [(byteKey 0x01, leaf 1)]
        it "a leaf that no merge operation uses is refused"
            $ let s = honest pair []
              in  refused
                    pair
                    s{stLeaves = stLeaves s <> [Indirect (byteKey 0xc0) (leaf 3)]}
        it
            "the leaves under a prefix do not pass for the leaves under a shorter prefix"
            $ refused three (honest three [L]){stPrefix = []}
        it
            "the leaves under a prefix do not pass for the leaves under a sibling prefix"
            $ refused three (honest three [L]){stPrefix = [R]}
        it
            "the leaves under a prefix do not pass for the leaves under a longer prefix"
            $ refused
                three
                (honest three [L])
                    { stPrefix = replicate 8 L
                    , stLeaves = [Indirect (byteKey 0x00) (leaf 1)]
                    }
        it "a prefix inside the jump of the subtree root is accepted"
            $ accepted
                three
                (honest three [L]){stPrefix = [L, L, L]}
        it
            "the proof built for a prefix inside the jump is accepted for the jump"
            $ accepted
                three
                (honest three [L, L, L]){stPrefix = [L]}
        it "leaves in descending order are refused"
            $ let s = honest twins []
              in  refused twins s{stLeaves = reverse (stLeaves s)}
        it "leaves moved with their merge operations are refused"
            $ let s = honest skewed []
              in  case stLeaves s of
                    [a, b, c] ->
                        refused
                            skewed
                            s
                                { stLeaves = [a, c, b]
                                , stProof = CompletenessWitness [(2, 1), (0, 2)] []
                                }
                    _ -> error "three leaves expected"
        it "a leaf listed twice is refused"
            $ let s = honest pair []
              in  case stLeaves s of
                    a : _ -> refused pair s{stLeaves = [a, a]}
                    [] -> error "leaves expected"
        it "no leaves are refused for a prefix that holds leaves"
            $ refused three (honest three [L]){stLeaves = []}
        it
            "a merge operation on a leaf that is not listed is refused, not thrown"
            $ refused pair (withOps (const [(0, 2)]) (honest pair []))
        it "a merge operation on a negative leaf is refused, not thrown"
            $ refused pair (withOps (const [(-1, 0)]) (honest pair []))
        it "a merge operation on a single leaf is refused, not thrown"
            $ refused single (withOps (const [(0, 1)]) (honest single []))
        it "a leaf merged with itself is refused"
            $ refused pair (withOps (const [(0, 0)]) (honest pair []))
        it "a merge operation used twice is refused"
            $ refused pair (withOps (<> [(0, 1)]) (honest pair []))
        it "a step of no bits is refused, not thrown"
            $ refused
                three
                ( withSteps
                    (fmap (\st -> st{stepConsumed = 0}))
                    (honest three [L])
                )
        it "a step of negative size is refused, not thrown"
            $ refused
                three
                ( withSteps
                    (fmap (\st -> st{stepConsumed = -1}))
                    (honest three [L])
                )
        it "a step reaching past the key is refused"
            $ refused
                three
                ( withSteps
                    (fmap (\st -> st{stepConsumed = stepConsumed st + 1}))
                    (honest three [R])
                )
        it "steps that consume more bits than the key has are refused"
            $ let s = honest three [L]
              in  refused three (withSteps (<> stepsOf s) s)
        it
            "a proof of an absent prefix is refused for a prefix that holds leaves"
            $ let s = honest three [R, R]
              in  refused three s{stPrefix = [R], stLeaves = leavesUnder three [R]}
        it "a proof of an absent prefix is refused for another absent prefix"
            $ let s = honest three [R, R]
              in  refused three s{stPrefix = [R, R, R]}
        it "a proof of an absent prefix is refused with leaves"
            $ let s = honest three [R, R]
              in  refused three s{stLeaves = leavesUnder three []}
        it "the honest statements of the trees verify"
            $ mapM_
                (\(t, p) -> accepted t (honest t p))
                [ (pair, [])
                , (three, [])
                , (three, [L])
                , (three, [R])
                , (three, [R, R])
                , (three, byteKey 0x01)
                , (skewed, [])
                , (skewed, [L])
                , (single, [])
                , (single, byteKey 0x01)
                ]
        it "the leaves under a prefix are those of the tree"
            $ leavesUnder three [L]
            `shouldBe` [ Indirect (byteKey 0x00) (leaf 1)
                       , Indirect (byteKey 0x01) (leaf 2)
                       ]

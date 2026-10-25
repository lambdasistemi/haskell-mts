-- |
-- Module      : CSMT.Proof.MalformedBytesSpec
-- Description : Proof bytes that are not what a proof renders to
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- A verifier of serialized proofs is given bytes it did not render.
-- Starting from the bytes of honest proofs of random trees, of
-- proofs with fields damaged and of noise, and damaging them as
-- bytes and as CBOR, no bytes make a parser or a byte-level verifier
-- throw, bytes that are not exactly the rendering of a proof are not
-- parsed, and bytes that verify state what the tree holds. The bytes
-- of honest proofs verify, and any proof, however malformed, is
-- parsed back from its own bytes.
module CSMT.Proof.MalformedBytesSpec (spec) where

import CSMT.Core.CBOR
    ( parseCompletenessProof
    , parseExclusionProof
    , parseProof
    , renderCompletenessProof
    , renderExclusionProof
    , renderProof
    )
import CSMT.Core.Completeness (CompletenessProof (..))
import CSMT.Hashes (Hash, byteStringToKey, mkHash, renderHash)
import CSMT.Interface (Key)
import CSMT.Proof.Exclusion (ExclusionProof (..))
import CSMT.Proof.Insertion (InclusionProof (..))
import CSMT.Test.Malformed
    ( Tree (..)
    , describeTree
    , genHash
    , genTree
    , genTreeOfKeyBits
    , genWild
    , honestProof
    , mkTree
    )
import CSMT.Test.MalformedBytes
    ( describeBytes
    , genCompletenessBytes
    , genExclusionBytes
    , genInclusionBytes
    , genNoise
    , shrinkBytes
    )
import CSMT.Test.MalformedExclusion
    ( exclusionOf
    , genAbsentTarget
    , genForeignExclusion
    , genWildExclusion
    , startsAHeldKey
    )
import CSMT.Verify qualified as Verify
import Control.Exception (SomeException, evaluate, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Word (Word8)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
    ( Gen
    , Property
    , conjoin
    , counterexample
    , cover
    , elements
    , forAll
    , forAllShrink
    , frequency
    , ioProperty
    , oneof
    , property
    , suchThat
    , vectorOf
    , (===)
    )

type Scenario = (Tree, ByteString)

genScenario :: Gen Scenario
genScenario = do
    t <- genTree
    o <- oneof [genTree, genTreeOfKeyBits (treeKeyBits t)]
    b <-
        frequency
            [ (5, genInclusionBytes t o)
            , (5, genExclusionBytes t o)
            , (2, genCompletenessBytes t o)
            , (1, genNoise)
            ]
    pure (t, b)

shrinkScenario :: Scenario -> [Scenario]
shrinkScenario (t, b) = (t,) <$> shrinkBytes b

rootBytes :: Tree -> ByteString
rootBytes = renderHash . treeRoot

describing :: Scenario -> Property -> Property
describing (t, b) =
    counterexample (describeTree t <> "\nbytes " <> describeBytes b)

-- | What every parser and byte-level verifier says of the bytes,
-- reduced to a number that needs all of it.
outcome :: Tree -> ByteString -> Int
outcome t b =
    length (show (parseProof b))
        + length (show (parseExclusionProof b))
        + length (show (parseCompletenessProof b))
        + fromEnum (Verify.verifyInclusionProof (rootBytes t) b)
        + fromEnum (Verify.verifyExclusionProof (rootBytes t) b)

-- | The bytes are, when they parse, exactly what the proof renders
-- to.
canonical :: ByteString -> Property
canonical b =
    conjoin
        [ counterexample "inclusion proof"
            $ maybe True ((== b) . renderProof) (parseProof b)
                === True
        , counterexample "exclusion proof"
            $ maybe True ((== b) . renderExclusionProof) (parseExclusionProof b)
                === True
        , counterexample "completeness proof"
            $ maybe
                True
                ((== b) . renderCompletenessProof)
                (parseCompletenessProof b)
                === True
        ]

-- | The tree holds what the bytes say, or the bytes do not verify.
inclusionBytesHold :: Tree -> ByteString -> Property
inclusionBytesHold t b
    | Verify.verifyInclusionProof (rootBytes t) b =
        case parseProof b of
            Nothing -> counterexample "verified bytes do not parse" False
            Just p ->
                conjoin
                    [ counterexample "not the rendering" (renderProof p === b)
                    , counterexample "statement the tree does not hold"
                        $ Map.lookup (proofKey p) (treeLeaves t)
                            === Just (proofValue p)
                    ]
    | otherwise = property True

-- | The tree has no leaf under the target the bytes exclude, or the
-- bytes do not verify.
exclusionBytesHold :: Tree -> ByteString -> Property
exclusionBytesHold t b
    | Verify.verifyExclusionProof (rootBytes t) b =
        case parseExclusionProof b of
            Nothing -> counterexample "verified bytes do not parse" False
            Just e ->
                conjoin
                    [ counterexample
                        "not the rendering"
                        (renderExclusionProof e === b)
                    , counterexample "excludes a key the tree begins"
                        $ case e of
                            ExclusionEmpty -> False
                            ExclusionWitness{epTargetKey} ->
                                not (startsAHeldKey t epTargetKey)
                    ]
    | otherwise = property True

-- | The bytes with the byte at the position, which must be the one
-- expected, replaced by the given ones.
replaceAt :: Int -> Word8 -> [Word8] -> ByteString -> ByteString
replaceAt i expected new b
    | B.index b i == expected =
        B.concat [B.take i b, B.pack new, B.drop (i + 1) b]
    | otherwise =
        error
            ( "replaceAt: byte "
                <> show i
                <> " is "
                <> show (B.index b i)
                <> ", not "
                <> show expected
            )

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
spec = describe "proof bytes" $ do
    prop "the bytes of the honest proofs of a tree verify"
        $ forAll genTree
        $ \t ->
            forAll (vectorOf 4 (genAbsentTarget t)) $ \ks ->
                conjoin
                    $ [ counterexample (describeTree t)
                            $ Verify.verifyInclusionProof
                                (rootBytes t)
                                (renderProof p)
                                === True
                      | p <- Map.elems (treeProofs t)
                      ]
                        <> [ counterexample (describeTree t)
                                $ fmap
                                    ( Verify.verifyExclusionProof (rootBytes t)
                                        . renderExclusionProof
                                    )
                                    (exclusionOf t k)
                                    === Just True
                           | k <- ks
                           ]
    prop
        "the bytes of a proof built for a tree without the key are refused for a tree that holds it"
        $ forAll genTree
        $ \t ->
            forAll (genForeignExclusion t) $ \e ->
                counterexample (describeTree t)
                    $ Verify.verifyExclusionProof
                        (rootBytes t)
                        (renderExclusionProof e)
                        === False
    prop
        "the bytes of an inclusion proof are refused for a tree that holds another value for the key"
        $ forAll genTree
        $ \t ->
            forAll (elements (Map.toList (treeLeaves t))) $ \(k, v) ->
                forAll (genHash `suchThat` (/= v)) $ \v' ->
                    let other =
                            mkTree (treeKeyBits t)
                                $ Map.toList
                                $ Map.insert k v' (treeLeaves t)
                    in  counterexample (describeTree t)
                            $ fmap
                                ( Verify.verifyInclusionProof
                                    (rootBytes other)
                                    . renderProof
                                )
                                (honestProof t k)
                                === Just False
    prop "a proof, however malformed, is parsed back from its bytes"
        $ forAll genTree
        $ \t ->
            forAll (genTreeOfKeyBits (treeKeyBits t)) $ \o ->
                forAll
                    ((,,) <$> genWild t o <*> genWildExclusion t o <*> genWild t o)
                    $ \(p, e, q) ->
                        conjoin
                            [ parseProof (renderProof p) === Just p
                            , parseExclusionProof (renderExclusionProof e)
                                === Just e
                            , parseCompletenessProof
                                (renderCompletenessProof (CompletenessEmpty e))
                                === Just (CompletenessEmpty e)
                            , parseCompletenessProof
                                ( renderCompletenessProof
                                    (CompletenessWitness [(0, 1)] (proofSteps q))
                                )
                                === Just
                                    (CompletenessWitness [(0, 1)] (proofSteps q))
                            ]
    prop "no bytes make a parser or a byte-level verifier throw"
        $ forAllShrink genScenario shrinkScenario
        $ \s@(t, b) -> ioProperty $ do
            r <- try @SomeException (evaluate (outcome t b))
            pure
                $ describing s
                $ counterexample (either show (const "") r)
                $ property
                $ either (const False) (const True) r
    prop "bytes that parse are exactly what the proof renders to"
        $ forAllShrink genScenario shrinkScenario
        $ \s@(_, b) -> describing s (canonical b)
    prop
        "bytes that verify as an inclusion proof state what the tree holds"
        $ forAllShrink genScenario shrinkScenario
        $ \s@(t, b) ->
            cover
                5
                (Verify.verifyInclusionProof (rootBytes t) b)
                "bytes verified as an inclusion proof"
                $ describing s (inclusionBytesHold t b)
    prop
        "bytes that verify as an exclusion proof exclude a key the tree does not begin"
        $ forAllShrink genScenario shrinkScenario
        $ \s@(t, b) ->
            cover
                5
                (Verify.verifyExclusionProof (rootBytes t) b)
                "bytes verified as an exclusion proof"
                $ describing s (exclusionBytesHold t b)
    describe "kept counterexamples" $ do
        let pair =
                mkTree
                    8
                    [(byteKey 0x80, leaf 1), (byteKey 0x81, leaf 2)]
            wide =
                mkTree
                    8
                    [ (byteKey 0x00, leaf 1)
                    , (byteKey 0x01, leaf 2)
                    , (byteKey 0x80, leaf 3)
                    ]
            inclusion = renderProof (held pair (byteKey 0x81))
            exclusion =
                maybe
                    (error "no proof for an absent key")
                    renderExclusionProof
                    (exclusionOf wide (byteKey 0x40))
            includes = Verify.verifyInclusionProof (rootBytes pair)
            excludes = Verify.verifyExclusionProof (rootBytes wide)
            refusedInclusion b =
                (parseProof b, includes b) `shouldBe` (Nothing, False)
            refusedExclusion b =
                (parseExclusionProof b, excludes b)
                    `shouldBe` (Nothing, False)
        it "the honest bytes are the ones the cases below change"
            $ ( B.unpack (B.take 3 inclusion)
              , B.unpack (B.take 3 exclusion)
              , includes inclusion
              , excludes exclusion
              )
            `shouldBe` ([0x84, 0x88, 0x01], [0x83, 0x01, 0x88], True, True)
        it "a byte after an inclusion proof is refused"
            $ refusedInclusion (inclusion <> B.singleton 0)
        it "a byte after an exclusion proof is refused"
            $ refusedExclusion (exclusion <> B.singleton 0)
        it "the bytes of two inclusion proofs one after the other are refused"
            $ refusedInclusion (inclusion <> inclusion)
        it "an inclusion proof whose list claims one more item is refused"
            $ refusedInclusion (replaceAt 0 0x84 [0x85] inclusion)
        it "an inclusion proof whose list claims one item less is refused"
            $ refusedInclusion (replaceAt 0 0x84 [0x83] inclusion)
        it "an exclusion proof whose list claims one more item is refused"
            $ refusedExclusion (replaceAt 0 0x83 [0x84] exclusion)
        it "an exclusion proof whose list claims one item less is refused"
            $ refusedExclusion (replaceAt 0 0x83 [0x82] exclusion)
        it "an inclusion proof whose key list claims more bits is refused"
            $ refusedInclusion (replaceAt 1 0x88 [0x89] inclusion)
        it "a direction written in the long form of a number is refused"
            $ refusedInclusion (replaceAt 3 0x00 [0x18, 0x00] inclusion)
        it "a direction of 2 is refused"
            $ refusedInclusion (replaceAt 3 0x00 [0x02] inclusion)
        it "a direction of 256 is refused"
            $ refusedInclusion (replaceAt 3 0x00 [0x19, 0x01, 0x00] inclusion)
        it "an exclusion proof of an unknown kind is refused"
            $ refusedExclusion (replaceAt 1 0x01 [0x02] exclusion)
        it "an exclusion proof of the first kind with a witness is refused"
            $ refusedExclusion (replaceAt 1 0x01 [0x00] exclusion)
        it "an inclusion proof cut by one byte is refused"
            $ refusedInclusion (B.init inclusion)
        it "an exclusion proof cut by one byte is refused"
            $ refusedExclusion (B.init exclusion)
        it "no bytes are refused"
            $ do
                refusedInclusion B.empty
                refusedExclusion B.empty
        it "a list of 2^63 - 1 items is refused, not read"
            $ do
                let huge =
                        B.pack
                            [0x9b, 0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]
                refusedInclusion huge
                refusedExclusion huge
        it "a key of 2^63 - 1 directions is refused, not read"
            $ refusedInclusion
                ( B.pack
                    [0x84, 0x9b, 0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]
                )
        it
            "the bytes of an empty exclusion proof do not exclude a key the tree holds"
            $ ( parseExclusionProof (renderExclusionProof ExclusionEmpty)
              , Verify.verifyExclusionProof
                    (rootBytes pair)
                    (renderExclusionProof ExclusionEmpty)
              )
            `shouldBe` (Just ExclusionEmpty, False)
        it "no honest proof is refused for being non-canonical"
            $ ( isNothing (parseProof inclusion)
              , isNothing (parseExclusionProof exclusion)
              )
            `shouldBe` (False, False)

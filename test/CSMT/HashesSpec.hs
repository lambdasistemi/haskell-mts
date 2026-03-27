module CSMT.HashesSpec (spec) where

import CSMT.Hashes
    ( Hash (..)
    , addHash
    , byteStringToKey
    , isoHash
    , keyToByteString
    , keyToHash
    , mkHash
    , parseHash
    , parseProof
    , renderHash
    , renderProof
    , verifyInclusionProof
    )
import CSMT.Interface (Direction (..), Indirect (..))
import CSMT.Proof.Insertion
    ( InclusionProof (..)
    , ProofStep (..)
    )
import Control.Lens (review, view)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
    ( Gen
    , Testable (..)
    , arbitrary
    , elements
    , forAll
    , listOf
    , vectorOf
    , (===)
    , (==>)
    )

-- | Generate an arbitrary ByteString
genBS :: Gen ByteString
genBS = B.pack <$> listOf (elements [0 .. 255])

-- | Generate a 32-byte ByteString
genBS32 :: Gen ByteString
genBS32 = B.pack <$> vectorOf 32 arbitrary

-- | Generate an arbitrary Hash
genHash :: Gen Hash
genHash = mkHash <$> genBS

genProofs :: Gen (InclusionProof Hash)
genProofs = do
    proofKey <- listOf $ elements [L, R]
    proofValue <- mkHash <$> genBS
    proofRootHash <- mkHash <$> genBS
    proofRootJump <- listOf $ elements [L, R]
    proofSteps <- listOf $ do
        stepConsumed <-
            (+ 1) . length <$> listOf (elements [L, R])
        siblingValue <- mkHash <$> genBS
        siblingJump <- listOf $ elements [L, R]
        return
            $ ProofStep
                { stepConsumed
                , stepSibling =
                    Indirect
                        { jump = siblingJump
                        , value = siblingValue
                        }
                }
    return
        $ InclusionProof
            { proofKey
            , proofValue
            , proofRootHash
            , proofSteps
            , proofRootJump
            }

spec :: Spec
spec = describe "Hashes" $ do
    describe "proof serialization" $ do
        prop "renders and parses proofs correctly"
            $ forAll genProofs
            $ \proof ->
                parseProof (renderProof proof) === Just proof

    describe "key conversion" $ do
        prop "keyToByteString . byteStringToKey == id"
            $ forAll genBS
            $ \bs ->
                keyToByteString (byteStringToKey bs)
                    === bs

        prop "byteStringToKey produces 8 directions per byte"
            $ forAll genBS
            $ \bs ->
                length (byteStringToKey bs)
                    === B.length bs * 8

    describe "mkHash" $ do
        prop "produces 32-byte output"
            $ forAll genBS
            $ \bs ->
                B.length (renderHash (mkHash bs)) === 32

        prop "is deterministic"
            $ forAll genBS
            $ \bs -> mkHash bs === mkHash bs

    describe "parseHash" $ do
        prop "accepts 32-byte input"
            $ forAll genBS32
            $ \bs -> parseHash bs === Just (Hash bs)

        prop "rejects non-32-byte input"
            $ forAll genBS
            $ \bs ->
                B.length bs
                    /= 32
                    ==> parseHash bs
                        === Nothing

        prop "roundtrips with renderHash"
            $ forAll genHash
            $ \h -> parseHash (renderHash h) === Just h

    describe "addHash" $ do
        prop "produces 32-byte output"
            $ forAll ((,) <$> genHash <*> genHash)
            $ \(h1, h2) ->
                B.length (renderHash (addHash h1 h2))
                    === 32

        prop "is deterministic"
            $ forAll ((,) <$> genHash <*> genHash)
            $ \(h1, h2) ->
                addHash h1 h2 === addHash h1 h2

    describe "keyToHash" $ do
        prop "produces 32-byte output"
            $ forAll (listOf $ elements [L, R])
            $ \key ->
                B.length (renderHash (keyToHash key))
                    === 32

    describe "isoHash" $ do
        prop "view then review is identity"
            $ forAll genBS32
            $ \bs ->
                renderHash (view isoHash bs) === bs

        prop "review then view is identity"
            $ forAll genHash
            $ \h ->
                view isoHash (review isoHash h)
                    === h

    describe "verifyInclusionProof" $ do
        it "rejects empty input"
            $ verifyInclusionProof ""
            `shouldBe` False

        it "rejects garbage input"
            $ verifyInclusionProof "not-a-proof"
            `shouldBe` False

        prop "rejects random bytes"
            $ forAll genBS
            $ \bs ->
                B.length bs
                    < 100
                    ==> not
                        (verifyInclusionProof bs)
                        `shouldSatisfy` id

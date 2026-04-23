{-# LANGUAGE OverloadedStrings #-}

module MPF.VerifySpec (spec) where

import Control.Monad (forM_)
import Data.Bits (xor)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import MPF.Hashes (MPFHash, mkMPFHash, renderMPFHash)
import MPF.Hashes.Aiken (renderAikenProof)
import MPF.Interface (byteStringToHexKey)
import MPF.Proof.Exclusion
    ( MPFExclusionProof (..)
    , mpfExclusionProofSteps
    )
import MPF.Proof.Insertion (MPFProof (..))
import MPF.Test.Lib
    ( expectedFullTrieRoot
    , fruitsTestData
    , insertByteStringM
    , proofExcludeMPFM
    , proofMPFM
    , runMPFPure'
    )
import MPF.Verify
    ( verifyAikenExclusionProof
    , verifyAikenInclusionProof
    )
import Test.Hspec

proofPath :: ByteString -> ByteString
proofPath = renderMPFHash . mkMPFHash

mkFruitInclusionProof :: ByteString -> Maybe (MPFProof MPFHash)
mkFruitInclusionProof fruitKey =
    fst $ runMPFPure' $ do
        forM_ fruitsTestData $ uncurry insertByteStringM
        proofMPFM (byteStringToHexKey (proofPath fruitKey))

mkFruitExclusionProof
    :: ByteString -> Maybe (MPFExclusionProof MPFHash)
mkFruitExclusionProof fruitKey =
    fst $ runMPFPure' $ do
        forM_ fruitsTestData $ uncurry insertByteStringM
        proofExcludeMPFM (byteStringToHexKey (proofPath fruitKey))

tamperLastByte :: ByteString -> ByteString
tamperLastByte bs = case B.unsnoc bs of
    Just (prefix, w) -> prefix <> B.singleton (w `xor` 0xff)
    Nothing -> bs

spec :: Spec
spec = describe "MPF.Verify" $ do
    describe "verifyAikenInclusionProof"
        $ forM_ fruitsTestData
        $ \(fruitKey, fruitValue) ->
            it ("verifies " <> show fruitKey)
                $ case mkFruitInclusionProof fruitKey of
                    Just proof ->
                        verifyAikenInclusionProof
                            expectedFullTrieRoot
                            fruitKey
                            fruitValue
                            (renderAikenProof (mpfProofSteps proof))
                            `shouldBe` True
                    Nothing ->
                        expectationFailure
                            ("No proof for " <> show fruitKey)

    it "rejects a tampered inclusion proof"
        $ case mkFruitInclusionProof "mango[uid: 0]" of
            Just proof ->
                let mangoValue = case lookup "mango[uid: 0]" fruitsTestData of
                        Just v -> v
                        Nothing -> error "missing mango fixture"
                in  verifyAikenInclusionProof
                        expectedFullTrieRoot
                        "mango[uid: 0]"
                        mangoValue
                        (tamperLastByte (renderAikenProof (mpfProofSteps proof)))
                        `shouldBe` False
            Nothing -> expectationFailure "No proof for mango[uid: 0]"

    describe "verifyAikenExclusionProof" $ do
        it "verifies the canonical melon exclusion proof"
            $ case mkFruitExclusionProof "melon" of
                Just proof@MPFExclusionWitness{} ->
                    verifyAikenExclusionProof
                        expectedFullTrieRoot
                        "melon"
                        (renderAikenProof (mpfExclusionProofSteps proof))
                        `shouldBe` True
                Just MPFExclusionEmpty{} ->
                    expectationFailure "Unexpected empty-tree exclusion proof"
                Nothing ->
                    expectationFailure "No exclusion proof for melon"

        it "rejects an empty proof blob for the empty-tree sentinel"
            $ verifyAikenExclusionProof
                (B.replicate 32 0)
                "nothing-here"
                B.empty
            `shouldBe` False

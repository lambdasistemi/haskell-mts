{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Comprehensive proof CBOR compatibility test.
-- Verifies renderAikenProof round-trips correctly for all 30 fruits
-- and matches known JS vectors for inclusion and exclusion proofs.
module MPF.ProofCompatSpec (spec) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import MPF.Hashes (MPFHash, mkMPFHash, renderMPFHash)
import MPF.Hashes.Aiken (parseAikenProof, renderAikenProof)
import MPF.Interface (byteStringToHexKey)
import MPF.Proof.Exclusion
    ( MPFExclusionProof (..)
    , mpfExclusionProofSteps
    )
import MPF.Proof.Insertion (MPFProof (..))
import MPF.Test.Lib
    ( encodeHex
    , fruitsTestData
    , insertByteStringM
    , proofExcludeMPFM
    , proofMPFM
    , runMPFPure'
    )
import Test.Hspec

fruitProof :: ByteString -> Maybe (MPFProof MPFHash)
fruitProof fruitKey =
    fst $ runMPFPure' $ do
        forM_ fruitsTestData $ uncurry insertByteStringM
        let hexKey = byteStringToHexKey $ renderMPFHash $ mkMPFHash fruitKey
        proofMPFM hexKey

fruitExclusionProof
    :: ByteString -> Maybe (MPFExclusionProof MPFHash)
fruitExclusionProof fruitKey =
    fst $ runMPFPure' $ do
        forM_ fruitsTestData $ uncurry insertByteStringM
        let hexKey = byteStringToHexKey $ renderMPFHash $ mkMPFHash fruitKey
        proofExcludeMPFM hexKey

melonExclusionExpectedHex :: ByteString
melonExclusionExpectedHex =
    "9fd8799f005f5840c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb47238ba5d16031b6bace4aee22156f5028b0ca56dc24f7247d6435292e82c039c58403490a825d2e8deddf8679ce2f95f7e3a59d9c3e1af4a49b410266d21c9344d6d08434fd717aea47d156185d589f44a59fc2e0158eab7ff035083a2a66cd3e15bffffd8799f005f5840922f17e88cc74f89e0a135af20ae55ed0cac3c74f2b948bb9bc249bda9a759dd985c311e6afc57389e6f1e94796c920f142b867df4dd9304b3b6bbcfe5972c2958400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffff"

spec :: Spec
spec = describe "Proof CBOR compatibility" $ do
    describe "all 30 fruit proofs render and round-trip via Aiken CBOR"
        $ forM_ fruitsTestData
        $ \(fruitKey, _) ->
            it (show fruitKey) $ do
                case fruitProof fruitKey of
                    Nothing ->
                        expectationFailure $ "No proof for " ++ show fruitKey
                    Just proof -> do
                        let steps = mpfProofSteps proof
                            cbor = renderAikenProof steps
                            parsed = parseAikenProof cbor
                        -- Must round-trip
                        case parsed of
                            Nothing ->
                                expectationFailure "parseAikenProof failed on rendered CBOR"
                            Just parsedSteps ->
                                length parsedSteps `shouldBe` length steps

    describe "known JS vectors" $ do
        -- From aiken-lang/merkle-patricia-forestry off-chain/tests/trie.test.js
        it "mango proof matches JS CBOR" $ do
            case fruitProof "mango[uid: 0]" of
                Nothing -> expectationFailure "No mango proof"
                Just proof ->
                    encodeHex (renderAikenProof (mpfProofSteps proof))
                        `shouldBe` "9fd8799f005f5840c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb45fdf82687b1ab133324cebaf46d99d49f92720c5ded08d5b02f57530f2cc5a5f58401508f13471a031a21277db8817615e62a50a7427d5f8be572746aa5f0d49841758c5e4a29601399a5bd916e5f3b34c38e13253f4de2a3477114f1b2b8f9f2f4dffffd87b9f00582009d23032e6edc0522c00bc9b74edd3af226d1204a079640a367da94c84b69ecc5820c29c35ad67a5a55558084e634ab0d98f7dd1f60070b9ce2a53f9f305fd9d9795ffff"

        it "kumquat proof matches JS CBOR" $ do
            case fruitProof "kumquat[uid: 0]" of
                Nothing -> expectationFailure "No kumquat proof"
                Just proof ->
                    encodeHex (renderAikenProof (mpfProofSteps proof))
                        `shouldBe` "9fd8799f005f5840c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb47238ba5d16031b6bace4aee22156f5028b0ca56dc24f7247d6435292e82c039c58403490a825d2e8deddf8679ce2f95f7e3a59d9c3e1af4a49b410266d21c9344d6d08434fd717aea47d156185d589f44a59fc2e0158eab7ff035083a2a66cd3e15bffffd87a9f00d8799f0041075820a1ffbc0e72342b41129e2d01d289809079b002e54b123860077d2d66added281ffffff"

        it "melon exclusion proof matches JS CBOR" $ do
            case fruitExclusionProof "melon" of
                Just proof@MPFExclusionWitness{} ->
                    encodeHex
                        ( renderAikenProof
                            (mpfExclusionProofSteps proof)
                        )
                        `shouldBe` melonExclusionExpectedHex
                Just MPFExclusionEmpty{} ->
                    expectationFailure
                        "Unexpected empty-tree exclusion proof"
                Nothing ->
                    expectationFailure
                        "No exclusion proof for melon"

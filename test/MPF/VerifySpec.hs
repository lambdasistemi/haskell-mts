{-# LANGUAGE OverloadedStrings #-}

module MPF.VerifySpec (spec) where

import Control.Lens (Prism', prism')
import Control.Monad (forM_)
import Data.Bits (xor)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import MPF.Backend.Pure
    ( emptyMPFInMemoryDB
    , runMPFPure
    , runMPFPureTransaction
    )
import MPF.Backend.Standalone
    ( MPFStandalone (..)
    , MPFStandaloneCodecs (..)
    )
import MPF.Hashes
    ( MPFHash
    , fromHexKVAikenHashes
    , mkMPFHash
    , mpfHashing
    , parseMPFHash
    , renderMPFHash
    , root
    )
import MPF.Hashes.Aiken (renderAikenProof)
import MPF.Insertion (inserting)
import MPF.Interface (byteStringToHexKey)
import MPF.Proof.Exclusion
    ( MPFExclusionProof (..)
    , mpfExclusionProofSteps
    )
import MPF.Proof.Insertion
    ( MPFProof (..)
    , mkMPFInclusionProof
    )
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

bytesCodec :: Prism' ByteString ByteString
bytesCodec = prism' id Just

hashCodec :: Prism' ByteString MPFHash
hashCodec = prism' renderMPFHash parseMPFHash

byteCodecs :: MPFStandaloneCodecs ByteString ByteString MPFHash
byteCodecs =
    MPFStandaloneCodecs
        { mpfKeyCodec = bytesCodec
        , mpfValueCodec = bytesCodec
        , mpfNodeCodec = hashCodec
        }

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

    it "verifies a single-leaf proof from the raw ByteString write path" $ do
        let key = "a"
            value = "a"
            program = runMPFPureTransaction byteCodecs $ do
                inserting
                    []
                    fromHexKVAikenHashes
                    mpfHashing
                    MPFStandaloneKVCol
                    MPFStandaloneMPFCol
                    key
                    value
                builtRoot <- root MPFStandaloneMPFCol []
                builtProof <-
                    mkFruitlessProof
                        key
                pure (builtRoot, builtProof)
            ((trustedRoot, proof), _) =
                runMPFPure emptyMPFInMemoryDB program
        case (trustedRoot, proof) of
            (Just rootHash, Just proof') ->
                verifyAikenInclusionProof
                    rootHash
                    key
                    value
                    (renderAikenProof (mpfProofSteps proof'))
                    `shouldBe` True
            _ ->
                expectationFailure
                    "expected a root and inclusion proof for single-leaf trie"

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
  where
    mkFruitlessProof =
        mkMPFInclusionProof
            []
            fromHexKVAikenHashes
            mpfHashing
            MPFStandaloneMPFCol

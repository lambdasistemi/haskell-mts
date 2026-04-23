{-# LANGUAGE OverloadedStrings #-}

module MPF.Proof.ExclusionSpec (spec) where

import Data.ByteString qualified as B
import Data.List (nub)
import Data.Maybe (isJust)
import Data.Word (Word8)
import MPF.Hashes (MPFHash, mkMPFHash, mpfHashing)
import MPF.Hashes.Aiken (parseAikenProof, renderAikenProof)
import MPF.Interface (HexDigit (..), HexKey, byteStringToHexKey)
import MPF.Proof.Exclusion
    ( MPFExclusionProof (..)
    , foldMPFExclusionProof
    , mpfExclusionProofSteps
    , verifyMPFExclusionProof
    )
import MPF.Proof.Insertion (MPFProofStep (..))
import MPF.Test.Lib
    ( getRootHashM
    , insertMPFM
    , proofExcludeMPFM
    , runMPFPure'
    , verifyExcludeMPFM
    )
import Test.Hspec
import Test.QuickCheck
    ( Gen
    , Property
    , choose
    , counterexample
    , elements
    , forAll
    , property
    , vectorOf
    , (===)
    )

exclusionProofFor
    :: [(HexKey, MPFHash)]
    -> HexKey
    -> (Maybe (MPFExclusionProof MPFHash), Maybe MPFHash)
exclusionProofFor inserts targetKey =
    fst $ runMPFPure' $ do
        mapM_ (uncurry insertMPFM) inserts
        proof <- proofExcludeMPFM targetKey
        root <- getRootHashM
        pure (proof, root)

isWitness :: Maybe (MPFExclusionProof a) -> Bool
isWitness (Just MPFExclusionWitness{}) = True
isWitness _ = False

genHexDigit :: Gen HexDigit
genHexDigit =
    HexDigit . fromIntegral <$> choose (0, 15 :: Int)

genFixedKey :: Int -> Gen [HexDigit]
genFixedKey n = vectorOf n genHexDigit

genTreeAndAbsentKey :: Gen ([HexKey], HexKey)
genTreeAndAbsentKey = do
    numKeys <- choose (2, 10)
    keys <- nub <$> vectorOf numKeys (genFixedKey 16)
    absent <- genFixedKey 16
    if length keys < 2 || absent `elem` keys
        then genTreeAndAbsentKey
        else pure (keys, absent)

genValueHash :: Gen MPFHash
genValueHash =
    mkMPFHash . B.pack <$> vectorOf 8 genWord8
  where
    genWord8 :: Gen Word8
    genWord8 = fromIntegral <$> choose (0, 255 :: Int)

genInserts :: [HexKey] -> Gen [(HexKey, MPFHash)]
genInserts keys =
    zip keys <$> vectorOf (length keys) genValueHash

spec :: Spec
spec = describe "MPF.Proof.Exclusion" $ do
    it "builds an explicit empty-tree proof" $ do
        let target = byteStringToHexKey "absent"
            ((mProof, trustedRoot), _) = runMPFPure' $ do
                proof <- proofExcludeMPFM target
                root <- getRootHashM
                pure (proof, root)
        trustedRoot `shouldBe` Nothing
        case mProof of
            Just proof@MPFExclusionEmpty{} -> do
                foldMPFExclusionProof mpfHashing proof `shouldBe` Nothing
                parseAikenProof
                    (renderAikenProof (mpfExclusionProofSteps proof))
                    `shouldBe` Just []
                verifyMPFExclusionProof
                    mpfHashing
                    trustedRoot
                    proof
                    `shouldBe` True
            _ -> expectationFailure "Expected MPFExclusionEmpty"

    it "returns Nothing for a present key" $ do
        let key = byteStringToHexKey "hello"
            value = mkMPFHash "world"
            (mProof, _) = runMPFPure' $ do
                insertMPFM key value
                proofExcludeMPFM key
        mProof `shouldBe` Nothing

    it "verifies exclusion for a single-leaf divergence" $ do
        let witnessKey = byteStringToHexKey "hello"
            targetKey = byteStringToHexKey "hullo"
            value = mkMPFHash "world"
            ((mProof, trustedRoot), _) = runMPFPure' $ do
                insertMPFM witnessKey value
                proof <- proofExcludeMPFM targetKey
                root <- getRootHashM
                pure (proof, root)
        case mProof of
            Just proof@MPFExclusionWitness{} -> do
                verifyMPFExclusionProof
                    mpfHashing
                    trustedRoot
                    proof
                    `shouldBe` True
                parseAikenProof
                    (renderAikenProof (mpfExclusionProofSteps proof))
                    `shouldSatisfy` isJust
            _ -> expectationFailure "Expected populated exclusion proof"

    it "verifies exclusion for a missing child under a branch" $ do
        let keyA = [HexDigit 1, HexDigit 1]
            keyB = [HexDigit 1, HexDigit 2]
            keyC = [HexDigit 1, HexDigit 4]
            targetKey = [HexDigit 1, HexDigit 3, HexDigit 5]
            valueA = mkMPFHash "a"
            valueB = mkMPFHash "b"
            valueC = mkMPFHash "c"
            ((mProof, verified), _) = runMPFPure' $ do
                insertMPFM keyA valueA
                insertMPFM keyB valueB
                insertMPFM keyC valueC
                proof <- proofExcludeMPFM targetKey
                ok <- verifyExcludeMPFM targetKey
                pure (proof, ok)
        verified `shouldBe` True
        case mProof of
            Just MPFExclusionWitness{mpeProofSteps = [ProofStepBranch{}]} ->
                pure ()
            _ -> expectationFailure "Expected a terminal Branch exclusion step"

    it "rejects a tampered target key" $ do
        let witnessKey = byteStringToHexKey "hello"
            targetKey = byteStringToHexKey "hullo"
            tamperedKey = byteStringToHexKey "hello"
            value = mkMPFHash "world"
            ((mProof, trustedRoot), _) = runMPFPure' $ do
                insertMPFM witnessKey value
                proof <- proofExcludeMPFM targetKey
                root <- getRootHashM
                pure (proof, root)
        case mProof of
            Just proof@MPFExclusionWitness{} ->
                verifyMPFExclusionProof
                    mpfHashing
                    trustedRoot
                    proof{mpeTargetKey = tamperedKey}
                    `shouldBe` False
            _ -> expectationFailure "Expected populated exclusion proof"

    describe "property tests" $ do
        it "absent key always produces a verifiable proof"
            $ property propAbsentKeyProves

        it "present key always returns Nothing"
            $ property propPresentKeyFails

        it "tampered target key fails verification"
            $ property propTamperedTargetFails

propAbsentKeyProves :: Property
propAbsentKeyProves =
    forAll genTreeAndAbsentKey $ \(keys, absent) ->
        forAll (genInserts keys) $ \inserts ->
            let (result, trustedRoot) =
                    exclusionProofFor inserts absent
            in  counterexample
                    ( "keys="
                        ++ show keys
                        ++ " absent="
                        ++ show absent
                        ++ " result="
                        ++ show (isWitness result)
                        ++ " root="
                        ++ show trustedRoot
                    )
                    $ case result of
                        Just proof ->
                            verifyMPFExclusionProof
                                mpfHashing
                                trustedRoot
                                proof
                                === True
                        Nothing ->
                            property False

propPresentKeyFails :: Property
propPresentKeyFails =
    forAll genTreeAndAbsentKey $ \(keys, _) ->
        forAll (genInserts keys) $ \inserts ->
            forAll (elements keys) $ \present ->
                fst (exclusionProofFor inserts present)
                    === Nothing

propTamperedTargetFails :: Property
propTamperedTargetFails =
    forAll genTreeAndAbsentKey $ \(keys, absent) ->
        forAll (genInserts keys) $ \inserts ->
            forAll (elements keys) $ \present ->
                let (result, trustedRoot) =
                        exclusionProofFor inserts absent
                in  counterexample
                        ( "present="
                            ++ show present
                            ++ " absent="
                            ++ show absent
                        )
                        $ case result of
                            Just proof@MPFExclusionWitness{} ->
                                verifyMPFExclusionProof
                                    mpfHashing
                                    trustedRoot
                                    proof{mpeTargetKey = present}
                                    === False
                            _ ->
                                property False

{-# LANGUAGE OverloadedStrings #-}

module MPF.Proof.InsertionSpec (spec) where

import Control.Monad (forM_)
import Data.ByteString qualified as B
import MPF.Hashes (mkMPFHash, mpfHashing, renderMPFHash)
import MPF.Interface (HexDigit (..), byteStringToHexKey)
import MPF.Test.Lib
    ( deleteMPFM
    , foldMPFProof
    , fruitsTestData
    , insertByteStringM
    , insertMPFM
    , proofMPFM
    , runMPFPure'
    , verifyMPFM
    )
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = do
    describe "MPF.Proof.Insertion" $ do
        describe "mkMPFInclusionProof" $ do
            it "generates proof for single element" $ do
                let key = byteStringToHexKey "hello"
                    value = mkMPFHash "world"
                    (mp, _) = runMPFPure' $ do
                        insertMPFM key value
                        proofMPFM key
                mp `shouldSatisfy` \case
                    Just _ -> True
                    Nothing -> False

            it "generates proof for element with hex path" $ do
                let key = [HexDigit 1, HexDigit 2, HexDigit 3, HexDigit 4]
                    value = mkMPFHash "test-value"
                    (mp, _) = runMPFPure' $ do
                        insertMPFM key value
                        proofMPFM key
                mp `shouldSatisfy` \case
                    Just _ -> True
                    Nothing -> False

            it "returns Nothing for non-existent key" $ do
                let key1 = byteStringToHexKey "existing"
                    key2 = byteStringToHexKey "nonexistent"
                    value = mkMPFHash "value"
                    (mp, _) = runMPFPure' $ do
                        insertMPFM key1 value
                        proofMPFM key2
                mp `shouldBe` Nothing

            it "returns Nothing for empty tree" $ do
                let key = byteStringToHexKey "any-key"
                    (mp, _) = runMPFPure' $ proofMPFM key
                mp `shouldBe` Nothing

        describe "verifyMPFInclusionProof" $ do
            it "verifies proof for single element" $ do
                let key = byteStringToHexKey "hello"
                    value = mkMPFHash "world"
                    (r, _) = runMPFPure' $ do
                        insertMPFM key value
                        verifyMPFM key value
                r `shouldBe` True

            it "rejects proof with wrong value" $ do
                let key = byteStringToHexKey "hello"
                    value = mkMPFHash "world"
                    wrongValue = mkMPFHash "wrong"
                    (r, _) = runMPFPure' $ do
                        insertMPFM key value
                        verifyMPFM key wrongValue
                r `shouldBe` False

            it "verifies proof with multiple elements" $ do
                let k1 = byteStringToHexKey "key1"
                    v1 = mkMPFHash "value1"
                    k2 = byteStringToHexKey "key2"
                    v2 = mkMPFHash "value2"
                    k3 = byteStringToHexKey "key3"
                    v3 = mkMPFHash "value3"
                    (r, _) = runMPFPure' $ do
                        insertMPFM k1 v1
                        insertMPFM k2 v2
                        insertMPFM k3 v3
                        verifyMPFM k2 v2
                r `shouldBe` True

            it "verifies all elements after multiple insertions" $ do
                let k1 = byteStringToHexKey "apple"
                    v1 = mkMPFHash "red"
                    k2 = byteStringToHexKey "banana"
                    v2 = mkMPFHash "yellow"
                    k3 = byteStringToHexKey "cherry"
                    v3 = mkMPFHash "dark-red"
                    ((r1, r2, r3), _) = runMPFPure' $ do
                        insertMPFM k1 v1
                        insertMPFM k2 v2
                        insertMPFM k3 v3
                        (,,)
                            <$> verifyMPFM k1 v1
                            <*> verifyMPFM k2 v2
                            <*> verifyMPFM k3 v3
                (r1, r2, r3) `shouldBe` (True, True, True)

        describe "proof after deletion" $ do
            it "rejects proof for deleted element" $ do
                let key = byteStringToHexKey "to-delete"
                    value = mkMPFHash "value"
                    (r, _) = runMPFPure' $ do
                        insertMPFM key value
                        deleteMPFM key
                        verifyMPFM key value
                r `shouldBe` False

            it "still verifies non-deleted siblings" $ do
                let k1 = byteStringToHexKey "keep"
                    v1 = mkMPFHash "keep-value"
                    k2 = byteStringToHexKey "delete"
                    v2 = mkMPFHash "delete-value"
                    (r, _) = runMPFPure' $ do
                        insertMPFM k1 v1
                        insertMPFM k2 v2
                        deleteMPFM k2
                        verifyMPFM k1 v1
                r `shouldBe` True

        describe "foldMPFProof" $ do
            it "folds proof to compute root hash" $ do
                let key = byteStringToHexKey "test-key"
                    value = mkMPFHash "test-value"
                    ((mProof, _mRoot), _) = runMPFPure' $ do
                        insertMPFM key value
                        p <- proofMPFM key
                        return (p, value)
                case mProof of
                    Nothing -> fail "Expected proof"
                    Just proof -> do
                        let computedRoot = foldMPFProof mpfHashing value proof
                        -- The folded proof should produce a non-empty hash
                        renderMPFHash computedRoot `shouldSatisfy` (not . B.null)

        describe "fruits test vectors" $ do
            it "generates proofs for all fruits in dataset" $ do
                let (proofResults, _) = runMPFPure' $ do
                        forM_ fruitsTestData $ uncurry insertByteStringM
                        -- Try to generate proof for apple (first fruit)
                        let appleKey =
                                byteStringToHexKey
                                    $ renderMPFHash
                                    $ mkMPFHash "apple[uid: 58]"
                        proofMPFM appleKey
                proofResults `shouldSatisfy` \case
                    Just _ -> True
                    Nothing -> False

            it "verifies membership for apple with 3 fruits" $ do
                let (verified, _) = runMPFPure' $ do
                        insertByteStringM "apple[uid: 58]" "\xf0\x9f\x8d\x8e"
                        insertByteStringM "apricot[uid: 0]" "\xf0\x9f\xa4\xb7"
                        insertByteStringM "banana[uid: 218]" "\xf0\x9f\x8d\x8c"
                        let appleKey =
                                byteStringToHexKey
                                    $ renderMPFHash
                                    $ mkMPFHash "apple[uid: 58]"
                            appleValue = mkMPFHash "\xf0\x9f\x8d\x8e"
                        verifyMPFM appleKey appleValue
                verified `shouldBe` True

            it "verifies membership for apple with 28 fruits" $ do
                let first28 = take 28 fruitsTestData
                    (verified, _) = runMPFPure' $ do
                        forM_ first28 $ uncurry insertByteStringM
                        let appleKey =
                                byteStringToHexKey
                                    $ renderMPFHash
                                    $ mkMPFHash "apple[uid: 58]"
                            appleValue = mkMPFHash "\xf0\x9f\x8d\x8e"
                        verifyMPFM appleKey appleValue
                verified `shouldBe` True

            it "rejects wrong value for apple in fruits dataset" $ do
                let (verified, _) = runMPFPure' $ do
                        forM_ fruitsTestData $ uncurry insertByteStringM
                        -- Try to verify with wrong value
                        let appleKey =
                                byteStringToHexKey
                                    $ renderMPFHash
                                    $ mkMPFHash "apple[uid: 58]"
                            wrongValue = mkMPFHash "not-apple"
                        verifyMPFM appleKey wrongValue
                verified `shouldBe` False

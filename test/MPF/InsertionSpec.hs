{-# LANGUAGE OverloadedStrings #-}

module MPF.InsertionSpec (spec) where

import Control.Monad (forM_)
import Data.Map.Strict qualified as Map
import MPF.Backend.Pure (emptyMPFInMemoryDB, mpfInMemoryMPF)
import MPF.Hashes (mkMPFHash, renderMPFHash)
import MPF.Interface (HexDigit (..), byteStringToHexKey)
import MPF.Test.Lib
    ( encodeHex
    , expectedFullTrieRoot
    , fruitsTestData
    , getRootHashM
    , insertByteStringM
    , insertMPF
    , insertMPFM
    , runMPFPure'
    )
import Test.Hspec

spec :: Spec
spec = describe "MPF.Insertion" $ do
    describe "single insertion" $ do
        it "creates a non-empty database" $ do
            let key = byteStringToHexKey "hello"
                value = mkMPFHash "world"
                db = insertMPF emptyMPFInMemoryDB key value
            mpfInMemoryMPF db `shouldSatisfy` (not . Map.null)

        it "inserts at least one entry into MPF" $ do
            let key = byteStringToHexKey "test"
                value = mkMPFHash "value"
                db = insertMPF emptyMPFInMemoryDB key value
            Map.size (mpfInMemoryMPF db) `shouldSatisfy` (> 0)

    describe "multiple insertions" $ do
        it "handles two different keys" $ do
            let k1 = byteStringToHexKey "key1"
                v1 = mkMPFHash "value1"
                k2 = byteStringToHexKey "key2"
                v2 = mkMPFHash "value2"
                (_, db) = runMPFPure' $ do
                    insertMPFM k1 v1
                    insertMPFM k2 v2
            mpfInMemoryMPF db `shouldSatisfy` (not . Map.null)

        it "handles keys with common prefix" $ do
            let k1 = [HexDigit 1, HexDigit 2, HexDigit 3]
                v1 = mkMPFHash "value1"
                k2 = [HexDigit 1, HexDigit 2, HexDigit 4]
                v2 = mkMPFHash "value2"
                (_, db) = runMPFPure' $ do
                    insertMPFM k1 v1
                    insertMPFM k2 v2
            mpfInMemoryMPF db `shouldSatisfy` (not . Map.null)

    describe "test vectors" $ do
        it "produces correct key and value hashes" $ do
            -- Verify the building blocks match expected hashes
            let appleKeyHash = encodeHex $ renderMPFHash $ mkMPFHash "apple[uid: 58]"
            appleKeyHash
                `shouldBe` "5ed71f91166242e8477758810ad103aff35313b175b1762b0efe800fa9a126d2"

            let appleValueHash = encodeHex $ renderMPFHash $ mkMPFHash "\xf0\x9f\x8d\x8e"
            appleValueHash
                `shouldBe` "09d504e02c4e6fa7b66303a456bc8786da3f51e8bf2834eeb9c95ec479f3681a"

            -- Check apricot and banana key hashes (first nibble determines trie position)
            let apricotKeyHash = encodeHex $ renderMPFHash $ mkMPFHash "apricot[uid: 0]"
            -- Apricot starts with 'e'
            apricotKeyHash
                `shouldBe` "e599368b02b44b6f81db93490aad5cdc3308c5827ad9c48662c33c1c2072c9ec"

            let bananaKeyHash = encodeHex $ renderMPFHash $ mkMPFHash "banana[uid: 218]"
            -- Banana key hash starts with '3'
            bananaKeyHash
                `shouldBe` "3ee659e1fddc70f61cc65eb61478cd92a09fd7787ea4f913047469339f26b3b9"

        it "produces expected root hash for single apple" $ do
            let (mroot, _db) = runMPFPure' $ do
                    insertByteStringM "apple[uid: 58]" "\xf0\x9f\x8d\x8e" -- 🍎
                    getRootHashM
            case mroot of
                Nothing -> expectationFailure "Expected root hash, got Nothing"
                Just root ->
                    encodeHex (renderMPFHash root)
                        `shouldBe` "93c4ed2d36f2409c38b8112d70c23eaf92eeb325b5098c0195be7e5cfaf7d824"

        it "produces expected root hash for apple and apricot" $ do
            let (mroot, _db) = runMPFPure' $ do
                    insertByteStringM "apple[uid: 58]" "\xf0\x9f\x8d\x8e" -- 🍎
                    insertByteStringM "apricot[uid: 0]" "\xf0\x9f\xa4\xb7" -- 🤷
                    getRootHashM
            case mroot of
                Nothing -> expectationFailure "Expected root hash, got Nothing"
                Just root ->
                    encodeHex (renderMPFHash root)
                        `shouldBe` "d9e614a87dff7b38d59706f00085d1b23f8c3e32ab9f5c39dbfa090412012003"

        it "produces expected root hash for apple+banana" $ do
            let (mroot, _db) = runMPFPure' $ do
                    insertByteStringM "apple[uid: 58]" "\xf0\x9f\x8d\x8e" -- 🍎
                    insertByteStringM "banana[uid: 218]" "\xf0\x9f\x8d\x8c" -- 🍌
                    getRootHashM
            case mroot of
                Nothing -> expectationFailure "Expected root hash, got Nothing"
                Just root ->
                    encodeHex (renderMPFHash root)
                        `shouldBe` "6a00036a5182ad02098cc99e00ab679263571dbec847b12aa7abde525affbe39"

        it
            "produces expected root hash for 3 fruits (order: apple, apricot, banana)"
            $ do
                let (mroot, dbA) = runMPFPure' $ do
                        insertByteStringM "apple[uid: 58]" "\xf0\x9f\x8d\x8e" -- 🍎
                        insertByteStringM "apricot[uid: 0]" "\xf0\x9f\xa4\xb7" -- 🤷
                        insertByteStringM "banana[uid: 218]" "\xf0\x9f\x8d\x8c" -- 🍌
                        getRootHashM
                let (_mroot2, dbB) = runMPFPure' $ do
                        insertByteStringM "banana[uid: 218]" "\xf0\x9f\x8d\x8c" -- 🍌
                        insertByteStringM "apple[uid: 58]" "\xf0\x9f\x8d\x8e" -- 🍎
                        insertByteStringM "apricot[uid: 0]" "\xf0\x9f\xa4\xb7" -- 🤷
                        getRootHashM
                -- Verify database is identical regardless of insertion order
                mpfInMemoryMPF dbA `shouldBe` mpfInMemoryMPF dbB
                case mroot of
                    Nothing -> expectationFailure "Expected root hash, got Nothing"
                    Just root ->
                        encodeHex (renderMPFHash root)
                            `shouldBe` "3b9c8a23238aeef2bee260daec21acfdad07cb7d8f23bb5b97147323ef65ff5f"

        it
            "produces expected root hash for 3 fruits (order: banana, apple, apricot)"
            $ do
                let (mroot, _db) = runMPFPure' $ do
                        insertByteStringM "banana[uid: 218]" "\xf0\x9f\x8d\x8c" -- 🍌
                        insertByteStringM "apple[uid: 58]" "\xf0\x9f\x8d\x8e" -- 🍎
                        insertByteStringM "apricot[uid: 0]" "\xf0\x9f\xa4\xb7" -- 🤷
                        getRootHashM
                case mroot of
                    Nothing -> expectationFailure "Expected root hash, got Nothing"
                    Just root ->
                        encodeHex (renderMPFHash root)
                            `shouldBe` "3b9c8a23238aeef2bee260daec21acfdad07cb7d8f23bb5b97147323ef65ff5f"

        it "produces expected root hash for fruits dataset" $ do
            let (mroot, _db) = runMPFPure' $ do
                    forM_ fruitsTestData $ uncurry insertByteStringM
                    getRootHashM
            case mroot of
                Nothing -> expectationFailure "Expected root hash, got Nothing"
                Just root ->
                    encodeHex (renderMPFHash root)
                        `shouldBe` encodeHex expectedFullTrieRoot

        it "inserts all fruits without error" $ do
            let (mroot, _db) = runMPFPure' $ do
                    forM_ fruitsTestData $ uncurry insertByteStringM
                    getRootHashM
            mroot `shouldSatisfy` \case
                Just _ -> True
                Nothing -> False

{-# LANGUAGE OverloadedStrings #-}

module MPF.HashesSpec (spec) where

import Data.ByteString qualified as B
import MPF.Hashes
    ( computeLeafHash
    , computeMerkleRoot
    , mkMPFHash
    , nullHash
    , renderMPFHash
    )
import MPF.Interface (HexDigit (..))
import Test.Hspec

spec :: Spec
spec = describe "MPF.Hashes" $ do
    describe "nullHash" $ do
        it "is 32 bytes of zeros" $ do
            renderMPFHash nullHash `shouldSatisfy` \bs ->
                all (== 0) (B.unpack bs)

    describe "mkMPFHash" $ do
        it "produces 32-byte hashes" $ do
            let h = mkMPFHash "hello"
            B.length (renderMPFHash h) `shouldBe` 32

        it "is deterministic" $ do
            mkMPFHash "test" `shouldBe` mkMPFHash "test"

        it "produces different hashes for different inputs" $ do
            mkMPFHash "a" `shouldNotBe` mkMPFHash "b"

    describe "computeLeafHash" $ do
        it "produces different hashes for different suffixes" $ do
            let v = mkMPFHash "value"
                h1 = computeLeafHash [] v
                h2 = computeLeafHash [HexDigit 1] v
            h1 `shouldNotBe` h2

        it "handles empty suffix" $ do
            let v = mkMPFHash "value"
                h = computeLeafHash [] v
            B.length (renderMPFHash h) `shouldBe` 32

    describe "computeMerkleRoot" $ do
        it "handles all Nothing (sparse array)" $ do
            let result = computeMerkleRoot (replicate 16 Nothing)
            B.length (renderMPFHash result) `shouldBe` 32

        it "handles single element" $ do
            let h = mkMPFHash "child"
                children = Just h : replicate 15 Nothing
                result = computeMerkleRoot children
            B.length (renderMPFHash result) `shouldBe` 32

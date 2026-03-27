{-# LANGUAGE OverloadedStrings #-}

module MPF.InterfaceSpec (spec) where

import Control.Lens (preview, review)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Serialize.Extra (evalGetM, evalPutM)
import MPF.Hashes (MPFHash (..), renderMPFHash)
import MPF.Interface
    ( HexDigit (..)
    , HexIndirect (..)
    , allHexDigits
    , byteStringToHexKey
    , compareHexKeys
    , getHexIndirect
    , getHexKey
    , hexKeyPrism
    , hexKeyToByteString
    , mkBranchIndirect
    , mkHexDigit
    , mkLeafIndirect
    , prefixHex
    , putHexIndirect
    , putHexKey
    )
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

genHexDigit :: Gen HexDigit
genHexDigit = HexDigit <$> choose (0, 15)

genHexKey :: Gen [HexDigit]
genHexKey = listOf genHexDigit

genMPFHash :: Gen MPFHash
genMPFHash = MPFHash . B.pack <$> vectorOf 32 arbitrary

genHexIndirect :: Gen (HexIndirect MPFHash)
genHexIndirect =
    HexIndirect
        <$> genHexKey
        <*> genMPFHash
        <*> arbitrary

spec :: Spec
spec = describe "MPF.Interface" $ do
    describe "HexDigit" $ do
        it "mkHexDigit succeeds for valid values" $ do
            mkHexDigit 0 `shouldBe` Just (HexDigit 0)
            mkHexDigit 15 `shouldBe` Just (HexDigit 15)

        it "mkHexDigit fails for invalid values" $ do
            mkHexDigit 16 `shouldBe` Nothing
            mkHexDigit 255 `shouldBe` Nothing

        prop "mkHexDigit accepts 0-15"
            $ forAll (choose (0, 15))
            $ \n ->
                mkHexDigit n === Just (HexDigit n)

        prop "mkHexDigit rejects >= 16"
            $ forAll (choose (16, 255))
            $ \n -> mkHexDigit n === Nothing

        it "allHexDigits has 16 elements"
            $ length allHexDigits
            `shouldBe` 16

    describe "HexKey conversion" $ do
        prop "byteStringToHexKey produces 2 nibbles per byte"
            $ forAll (B.pack <$> listOf arbitrary)
            $ \bs ->
                length (byteStringToHexKey bs)
                    === B.length bs * 2

        prop "roundtrips for even-length keys"
            $ forAll (B.pack <$> listOf arbitrary)
            $ \(bs :: ByteString) ->
                hexKeyToByteString (byteStringToHexKey bs)
                    === bs

        prop "odd-length key packing"
            $ forAll genHexDigit
            $ \d ->
                B.length (hexKeyToByteString [d]) === 1

    describe "compareHexKeys" $ do
        prop "common prefix + suffixes reconstruct originals"
            $ forAll ((,) <$> genHexKey <*> genHexKey)
            $ \(k1, k2) ->
                let (common, s1, s2) = compareHexKeys k1 k2
                in  conjoin
                        [ common ++ s1 === k1
                        , common ++ s2 === k2
                        ]

        prop "identical keys have empty suffixes"
            $ forAll genHexKey
            $ \k ->
                compareHexKeys k k === (k, [], [])

    describe "HexIndirect constructors" $ do
        prop "mkLeafIndirect sets hexIsLeaf True"
            $ forAll ((,) <$> genHexKey <*> genMPFHash)
            $ \(k, v) ->
                hexIsLeaf (mkLeafIndirect k v) === True

        prop "mkBranchIndirect sets hexIsLeaf False"
            $ forAll ((,) <$> genHexKey <*> genMPFHash)
            $ \(k, v) ->
                hexIsLeaf (mkBranchIndirect k v) === False

        prop "prefixHex prepends to jump"
            $ forAll
                ((,) <$> genHexKey <*> genHexIndirect)
            $ \(prefix, hi) ->
                hexJump (prefixHex prefix hi)
                    === prefix ++ hexJump hi

    describe "HexKey serialization" $ do
        prop "putHexKey/getHexKey roundtrip"
            $ forAll genHexKey
            $ \key ->
                evalGetM getHexKey (evalPutM (putHexKey key))
                    === Just key

        prop "hexKeyPrism roundtrip"
            $ forAll genHexKey
            $ \key ->
                preview hexKeyPrism (review hexKeyPrism key)
                    === Just key

    describe "HexIndirect serialization" $ do
        prop "putHexIndirect/getHexIndirect roundtrip"
            $ forAll genHexIndirect
            $ \hi ->
                let bs = evalPutM (putHexIndirect (fmap renderMPFHash hi))
                in  evalGetM getHexIndirect bs
                        === Just (fmap renderMPFHash hi)

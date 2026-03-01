{-# LANGUAGE OverloadedStrings #-}

module MPF.InterfaceSpec (spec) where

import Data.ByteString (ByteString)
import MPF.Interface
    ( HexDigit (..)
    , byteStringToHexKey
    , compareHexKeys
    , hexKeyToByteString
    , mkHexDigit
    )
import Test.Hspec

spec :: Spec
spec = describe "MPF.Interface" $ do
    describe "HexDigit" $ do
        it "mkHexDigit succeeds for valid values" $ do
            mkHexDigit 0 `shouldBe` Just (HexDigit 0)
            mkHexDigit 15 `shouldBe` Just (HexDigit 15)
            mkHexDigit 8 `shouldBe` Just (HexDigit 8)

        it "mkHexDigit fails for invalid values" $ do
            mkHexDigit 16 `shouldBe` Nothing
            mkHexDigit 255 `shouldBe` Nothing

    describe "HexKey conversion" $ do
        it "byteStringToHexKey converts bytes to nibbles" $ do
            byteStringToHexKey "\x12" `shouldBe` [HexDigit 1, HexDigit 2]
            byteStringToHexKey "\xab" `shouldBe` [HexDigit 10, HexDigit 11]
            byteStringToHexKey "\x00" `shouldBe` [HexDigit 0, HexDigit 0]
            byteStringToHexKey "\xff" `shouldBe` [HexDigit 15, HexDigit 15]

        it "hexKeyToByteString converts nibbles back to bytes" $ do
            hexKeyToByteString [HexDigit 1, HexDigit 2] `shouldBe` "\x12"
            hexKeyToByteString [HexDigit 10, HexDigit 11] `shouldBe` "\xab"

        it "roundtrips correctly for even-length keys" $ do
            let bs = "\x12\x34\x56" :: ByteString
            hexKeyToByteString (byteStringToHexKey bs) `shouldBe` bs

    describe "compareHexKeys" $ do
        it "finds common prefix" $ do
            let k1 = [HexDigit 1, HexDigit 2, HexDigit 3]
                k2 = [HexDigit 1, HexDigit 2, HexDigit 4]
            compareHexKeys k1 k2 `shouldBe`
                ([HexDigit 1, HexDigit 2], [HexDigit 3], [HexDigit 4])

        it "handles empty common prefix" $ do
            let k1 = [HexDigit 1, HexDigit 2]
                k2 = [HexDigit 3, HexDigit 4]
            compareHexKeys k1 k2 `shouldBe`
                ([], [HexDigit 1, HexDigit 2], [HexDigit 3, HexDigit 4])

        it "handles identical keys" $ do
            let k = [HexDigit 1, HexDigit 2]
            compareHexKeys k k `shouldBe` (k, [], [])

        it "handles one key being prefix of another" $ do
            let k1 = [HexDigit 1, HexDigit 2]
                k2 = [HexDigit 1, HexDigit 2, HexDigit 3]
            compareHexKeys k1 k2 `shouldBe`
                ([HexDigit 1, HexDigit 2], [], [HexDigit 3])

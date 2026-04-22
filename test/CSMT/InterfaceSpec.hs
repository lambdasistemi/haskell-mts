{-# LANGUAGE OverloadedLists #-}

module CSMT.InterfaceSpec (spec)
where

import CSMT
    ( Direction (L, R)
    , compareKeys
    , getKey
    , putIndirect
    , putKey
    , putSizedByteString
    )
import CSMT.Interface (Indirect (..))
import CSMT.Test.Lib (genKey)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Serialize (PutM)
import Data.Serialize.Extra (evalPutM, unsafeEvalGet)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Testable (property), forAll, vectorOf)

spec :: Spec
spec = do
    describe "compareKeys" $ do
        it "handles empty keys"
            $ compareKeys [] []
            `shouldBe` ([], [], [])
        it "handles identical keys"
            $ compareKeys [L, R, L] [L, R, L]
            `shouldBe` ([L, R, L], [], [])
        it "handles common prefixes"
            $ compareKeys [L, R, R, R] [L, R, L, R]
            `shouldBe` ([L, R], [R, R], [L, R])
        it "maintains information"
            $ property
            $ forAll (vectorOf 2 genKey)
            $ \case
                [k1, k2] ->
                    let (common, suffix1, suffix2) = compareKeys k1 k2
                    in  common <> suffix1 == k1
                            && common <> suffix2 == k2
                _ -> error "vectorOf produced wrong number of keys"
        it "produces suffixes without common prefixes"
            $ property
            $ forAll (vectorOf 2 genKey)
            $ \case
                [k1, k2] ->
                    let (_, suffix1, suffix2) = compareKeys k1 k2
                    in  case (suffix1, suffix2) of
                            (d1 : _, d2 : _) -> d1 /= d2
                            _ -> True
                _ -> error "vectorOf produced wrong number of keys"
    describe "encoding bytestrings" $ do
        let put = putSizedByteString
        it "handles empty bytestrings" $ do
            hexEval (put "") `shouldBe` "00 00"
        it "handles short bytestrings" $ do
            hexEval (put "abc") `shouldBe` "00 03 61 62 63"
            hexEval (put "hello") `shouldBe` "00 05 68 65 6c 6c 6f"
    describe "encoding jumps" $ do
        it "handles empty jumps" $ do
            hexEval (putKey []) `shouldBe` "00 00"
        it "handles single direction jumps" $ do
            hexEval (putKey [L]) `shouldBe` "00 01 00"
            hexEval (putKey [R]) `shouldBe` "00 01 80"
        it "handles 2-direction jumps" $ do
            hexEval (putKey [L, L]) `shouldBe` "00 02 00"
            hexEval (putKey [L, R]) `shouldBe` "00 02 40"
            hexEval (putKey [R, L]) `shouldBe` "00 02 80"
            hexEval (putKey [R, R]) `shouldBe` "00 02 c0"

        it "handles 8-direction jumps" $ do
            hexEval (putKey [L, L, L, L, L, L, L, R])
                `shouldBe` "00 08 01"
            hexEval (putKey [R, R, R, R, R, R, R, L])
                `shouldBe` "00 08 fe"
        it "handles 9-direction longer jumps" $ do
            hexEval (putKey [L, L, L, L, L, L, L, L, L])
                `shouldBe` "00 09 00 00"
            hexEval (putKey [R, R, R, R, R, R, R, R, R])
                `shouldBe` "00 09 ff 80"
            hexEval (putKey [R, R, R, R, R, R, R, R, L])
                `shouldBe` "00 09 ff 00"
    describe "encoding indirects" $ do
        it "handles empty jumps and empty values" $ do
            let ind = Indirect{jump = [], value = "" :: ByteString}
                bs = evalPutM $ putIndirect ind
            hex bs `shouldBe` "00 00 00 00"
        it "handles non-empty jumps and non-empty values" $ do
            let ind = Indirect{jump = [L, R, L], value = "data" :: ByteString}
                bs = evalPutM $ putIndirect ind
            hex bs `shouldBe` "00 03 40 00 04 64 61 74 61"
    describe "key convert to bytes and back" $ do
        it "is identity"
            $ property
            $ forAll genKey
            $ \key -> do
                let bs = evalPutM $ putKey key
                    key' = unsafeEvalGet getKey bs
                key' `shouldBe` key

hexEval :: PutM a -> String
hexEval = hex . evalPutM

hex :: ByteString -> String
hex = insertSpaces . BL.unpack . toLazyByteString . byteStringHex
  where
    insertSpaces :: String -> String
    insertSpaces = concat . go
      where
        go [] = []
        go [a, b] = [a, b] : go []
        go (a : b : rest) = [a, b, ' '] : go rest
        go rest = error $ "insertSpaces: unexpected input " ++ show rest

{-# LANGUAGE OverloadedStrings #-}

module Data.Serialize.ExtraSpec (spec) where

import Control.Lens (preview, review)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Serialize (putByteString)
import Data.Serialize qualified as S
import Data.Serialize.Extra
    ( evalGetM
    , evalPutM
    , intCodec
    , unsafeEvalGet
    )
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

spec :: Spec
spec = describe "Data.Serialize.Extra" $ do
    describe "evalPutM" $ do
        prop "serializes bytes correctly"
            $ \ws ->
                let bs = B.pack ws
                in  evalPutM (putByteString bs) === bs

    describe "unsafeEvalGet" $ do
        prop "roundtrips with evalPutM for Int"
            $ \(n :: Int) ->
                unsafeEvalGet S.get (evalPutM (S.put n))
                    === n

    describe "evalGetM" $ do
        prop "returns Just on valid input"
            $ \(n :: Int) ->
                evalGetM S.get (evalPutM (S.put n))
                    === Just n

        it "returns Nothing on invalid input"
            $ evalGetM (S.get :: S.Get Int) ""
            `shouldBe` Nothing

        it "returns Nothing on garbage"
            $ evalGetM (S.get :: S.Get Int) "x"
            `shouldBe` Nothing

    describe "intCodec" $ do
        prop "roundtrips via prism"
            $ \(n :: Int) ->
                preview intCodec (review intCodec n)
                    === Just n

        it "rejects empty bytestring"
            $ preview intCodec ("" :: ByteString)
            `shouldBe` (Nothing :: Maybe Int)

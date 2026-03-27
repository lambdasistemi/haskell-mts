{-# LANGUAGE OverloadedStrings #-}

module MPF.HashesSpec (spec) where

import Control.Lens (preview, review)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import MPF.Hashes
    ( MPFHash (..)
    , computeLeafHash
    , computeMerkleRoot
    , isoMPFHash
    , merkleProof
    , mkMPFHash
    , nibbleBytes
    , nullHash
    , packHexKey
    , parseMPFHash
    , renderMPFHash
    )
import MPF.Interface (HexDigit (..))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

-- | Generate a HexDigit (0-15)
genHexDigit :: Gen HexDigit
genHexDigit = HexDigit <$> choose (0, 15)

-- | Generate a HexKey of given length
genHexKey :: Gen [HexDigit]
genHexKey = listOf genHexDigit

-- | Generate an arbitrary 32-byte MPFHash
genMPFHash :: Gen MPFHash
genMPFHash = MPFHash . B.pack <$> vectorOf 32 arbitrary

-- | Generate an arbitrary ByteString
genBS :: Gen ByteString
genBS = B.pack <$> listOf arbitrary

spec :: Spec
spec = describe "MPF.Hashes" $ do
    describe "nullHash" $ do
        it "is 32 bytes of zeros"
            $ renderMPFHash nullHash
            `shouldSatisfy` \bs ->
                all (== 0) (B.unpack bs)

    describe "mkMPFHash" $ do
        prop "produces 32-byte hashes"
            $ forAll genBS
            $ \bs ->
                B.length (renderMPFHash (mkMPFHash bs))
                    === 32

        prop "is deterministic"
            $ forAll genBS
            $ \bs -> mkMPFHash bs === mkMPFHash bs

    describe "parseMPFHash" $ do
        prop "accepts 32-byte input"
            $ forAll (B.pack <$> vectorOf 32 arbitrary)
            $ \bs -> parseMPFHash bs === Just (MPFHash bs)

        prop "rejects non-32-byte input"
            $ forAll genBS
            $ \bs ->
                B.length bs /= 32 ==>
                    parseMPFHash bs === Nothing

        prop "roundtrips with renderMPFHash"
            $ forAll genMPFHash
            $ \h ->
                parseMPFHash (renderMPFHash h) === Just h

    describe "isoMPFHash" $ do
        prop "review then preview roundtrips"
            $ forAll genMPFHash
            $ \h ->
                preview isoMPFHash (review isoMPFHash h)
                    === Just h

    describe "packHexKey" $ do
        prop "packs pairs of nibbles"
            $ forAll (vectorOf 4 genHexDigit)
            $ \key ->
                B.length (packHexKey key) === 2

        prop "single nibble produces one byte"
            $ forAll genHexDigit
            $ \d -> B.length (packHexKey [d]) === 1

        it "empty key packs to empty"
            $ packHexKey []
            `shouldBe` B.empty

    describe "nibbleBytes" $ do
        prop "one byte per nibble"
            $ forAll genHexKey
            $ \key ->
                B.length (nibbleBytes key)
                    === length key

        prop "each byte equals nibble value"
            $ forAll genHexKey
            $ \key ->
                B.unpack (nibbleBytes key)
                    === map (\(HexDigit d) -> d) key

    describe "computeLeafHash" $ do
        prop "produces 32-byte output"
            $ forAll ((,) <$> genHexKey <*> genMPFHash)
            $ \(suffix, v) ->
                B.length
                    (renderMPFHash (computeLeafHash suffix v))
                    === 32

        prop "even-length suffix uses 0xff head"
            $ forAll
                ( (,)
                    <$> ( do
                            n <- choose (1, 10)
                            vectorOf (n * 2) genHexDigit
                        )
                    <*> genMPFHash
                )
            $ \(suffix, v) ->
                computeLeafHash suffix v
                    =/= computeLeafHash [] v

        prop "odd-length suffix uses 0x00 head"
            $ forAll
                ( (,)
                    <$> ( do
                            n <- choose (0, 9)
                            vectorOf (n * 2 + 1) genHexDigit
                        )
                    <*> genMPFHash
                )
            $ \(suffix, v) ->
                computeLeafHash suffix v
                    =/= computeLeafHash [] v

    describe "computeMerkleRoot" $ do
        prop "produces 32-byte output"
            $ forAll (vectorOf 16 (oneof [pure Nothing, Just <$> genMPFHash]))
            $ \children ->
                B.length
                    (renderMPFHash (computeMerkleRoot children))
                    === 32

        prop "is deterministic"
            $ forAll (vectorOf 16 (oneof [pure Nothing, Just <$> genMPFHash]))
            $ \children ->
                computeMerkleRoot children
                    === computeMerkleRoot children

        it "handles all Nothing"
            $ B.length
                ( renderMPFHash
                    $ computeMerkleRoot (replicate 16 Nothing)
                )
            `shouldBe` 32

    describe "merkleProof" $ do
        prop "produces exactly 4 hashes"
            $ forAll
                ( (,)
                    <$> vectorOf
                        16
                        (oneof [pure Nothing, Just <$> genMPFHash])
                    <*> choose (0, 15)
                )
            $ \(children, pos) ->
                length (merkleProof children pos) === 4

        prop "each proof hash is 32 bytes"
            $ forAll
                ( (,)
                    <$> vectorOf
                        16
                        (oneof [pure Nothing, Just <$> genMPFHash])
                    <*> choose (0, 15)
                )
            $ \(children, pos) ->
                all
                    (\h -> B.length (renderMPFHash h) == 32)
                    (merkleProof children pos)
                    === True

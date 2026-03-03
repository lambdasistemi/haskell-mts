module MPF.NamespaceSpec (spec) where

import MPF.Hashes (mkMPFHash)
import MPF.Interface (HexDigit (..), byteStringToHexKey)
import MPF.Test.Lib
    ( deleteMPFMAt
    , deleteSubtreeMAt
    , evalMPFPure'
    , getRootHashM
    , getRootHashMAt
    , insertMPFM
    , insertMPFMAt
    , verifyMPFMAt
    )
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

spec :: Spec
spec = describe "MPF.Namespace" $ do
    let pfxA = [HexDigit 0, HexDigit 1]
        pfxB = [HexDigit 0, HexDigit 2]
        key1 = byteStringToHexKey "hello"
        val1 = mkMPFHash "world"
        key2 = byteStringToHexKey "foo"
        val2 = mkMPFHash "bar"
        key3 = byteStringToHexKey "baz"
        val3 = mkMPFHash "qux"

    it "namespaces are independent" $ do
        let (rootA, rootB) = evalMPFPure' $ do
                insertMPFMAt pfxA key1 val1
                insertMPFMAt pfxB key2 val2
                (,)
                    <$> getRootHashMAt pfxA
                    <*> getRootHashMAt pfxB
        rootA `shouldNotBe` rootB

    it "prefix root matches non-prefixed root" $ do
        let rootPrefixed = evalMPFPure' $ do
                insertMPFMAt pfxA key1 val1
                getRootHashMAt pfxA
            rootPlain = evalMPFPure' $ do
                insertMPFM key1 val1
                getRootHashM
        rootPrefixed `shouldBe` rootPlain

    it "delete under one prefix doesn't affect another" $ do
        let rootB = evalMPFPure' $ do
                insertMPFMAt pfxA key1 val1
                insertMPFMAt pfxB key2 val2
                deleteMPFMAt pfxA key1
                getRootHashMAt pfxB
            rootBAlone = evalMPFPure' $ do
                insertMPFMAt pfxB key2 val2
                getRootHashMAt pfxB
        rootB `shouldBe` rootBAlone

    it "nsDelete wipes entire namespace" $ do
        let (rootA, rootB) = evalMPFPure' $ do
                insertMPFMAt pfxA key1 val1
                insertMPFMAt pfxA key2 val2
                insertMPFMAt pfxB key3 val3
                deleteSubtreeMAt pfxA
                (,)
                    <$> getRootHashMAt pfxA
                    <*> getRootHashMAt pfxB
        rootA `shouldBe` Nothing
        rootB `shouldNotBe` Nothing

    it "proofs work within a namespace" $ do
        let verified = evalMPFPure' $ do
                insertMPFMAt pfxA key1 val1
                verifyMPFMAt pfxA key1 val1
        verified `shouldBe` True

    it "multiple inserts in same namespace" $ do
        let rootPrefixed = evalMPFPure' $ do
                insertMPFMAt pfxA key1 val1
                insertMPFMAt pfxA key2 val2
                getRootHashMAt pfxA
            rootPlain = evalMPFPure' $ do
                insertMPFM key1 val1
                insertMPFM key2 val2
                getRootHashM
        rootPrefixed `shouldBe` rootPlain

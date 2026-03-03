module CSMT.NamespaceSpec (spec) where

import CSMT.Hashes (mkHash)
import CSMT.Interface (Direction (..))
import CSMT.Test.Lib
    ( deleteHashMAt
    , deleteSubtreeHashMAt
    , evalPureFromEmptyDB
    , getRootHashM
    , getRootHashMAt
    , insertHashM
    , insertHashMAt
    , verifyHashMAt
    )
import Data.String (fromString)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

spec :: Spec
spec = describe "CSMT.Namespace" $ do
    let pfxA = [L, L]
        pfxB = [L, R]
        key1 = [R, L, R]
        val1 = mkHash $ fromString "world"
        key2 = [R, R, L]
        val2 = mkHash $ fromString "bar"
        key3 = [L, R, L]
        val3 = mkHash $ fromString "qux"

    it "namespaces are independent" $ do
        let (rootA, rootB) = evalPureFromEmptyDB $ do
                insertHashMAt pfxA key1 val1
                insertHashMAt pfxB key2 val2
                (,)
                    <$> getRootHashMAt pfxA
                    <*> getRootHashMAt pfxB
        rootA `shouldNotBe` rootB

    it "prefix root matches non-prefixed root" $ do
        let rootPrefixed = evalPureFromEmptyDB $ do
                insertHashMAt pfxA key1 val1
                getRootHashMAt pfxA
            rootPlain = evalPureFromEmptyDB $ do
                insertHashM key1 val1
                getRootHashM
        rootPrefixed `shouldBe` rootPlain

    it "delete under one prefix doesn't affect another" $ do
        let rootB = evalPureFromEmptyDB $ do
                insertHashMAt pfxA key1 val1
                insertHashMAt pfxB key2 val2
                deleteHashMAt pfxA key1
                getRootHashMAt pfxB
            rootBAlone = evalPureFromEmptyDB $ do
                insertHashMAt pfxB key2 val2
                getRootHashMAt pfxB
        rootB `shouldBe` rootBAlone

    it "nsDelete wipes entire namespace" $ do
        let (rootA, rootB) = evalPureFromEmptyDB $ do
                insertHashMAt pfxA key1 val1
                insertHashMAt pfxA key2 val2
                insertHashMAt pfxB key3 val3
                deleteSubtreeHashMAt pfxA
                (,)
                    <$> getRootHashMAt pfxA
                    <*> getRootHashMAt pfxB
        rootA `shouldBe` Nothing
        rootB `shouldNotBe` Nothing

    it "proofs work within a namespace" $ do
        let verified = evalPureFromEmptyDB $ do
                insertHashMAt pfxA key1 val1
                verifyHashMAt pfxA key1 val1
        verified `shouldBe` True

    it "multiple inserts in same namespace" $ do
        let rootPrefixed = evalPureFromEmptyDB $ do
                insertHashMAt pfxA key1 val1
                insertHashMAt pfxA key2 val2
                getRootHashMAt pfxA
            rootPlain = evalPureFromEmptyDB $ do
                insertHashM key1 val1
                insertHashM key2 val2
                getRootHashM
        rootPrefixed `shouldBe` rootPlain

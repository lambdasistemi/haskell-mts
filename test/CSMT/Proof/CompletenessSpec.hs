{-# LANGUAGE OverloadedLists #-}

module CSMT.Proof.CompletenessSpec (spec)
where

import CSMT
    ( Direction (..)
    , Standalone (StandaloneCSMTCol)
    )
import CSMT.Backend.Pure
    ( runPureTransaction
    )
import CSMT.Hashes (hashHashing)
import CSMT.Interface (Hashing (..))
import CSMT.Proof.Completeness
    ( CompletenessProof (..)
    , collectValues
    , foldCompletenessProof
    , generateProof
    )
import CSMT.Test.Lib
    ( evalPureFromEmptyDB
    , hashCodecs
    , indirect
    , insertHashes
    , insertWord64s
    , intHash
    , manyRandomPaths
    , word64Codecs
    , word64Hashing
    )
import Data.List (sort)
import Database.KV.Transaction (query)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (forAll, property)

spec :: Spec
spec = do
    describe "collectValues" $ do
        it "collects all values for a simple tree of ints"
            $ property
            $ forAll manyRandomPaths
            $ \ks -> do
                let values = zipWith indirect ks [1 ..]
                    collected = evalPureFromEmptyDB $ do
                        insertWord64s values
                        runPureTransaction word64Codecs
                            $ collectValues StandaloneCSMTCol [] []
                collected
                    `shouldBe` sort values
        it "collects all values for a simple tree of hashes"
            $ property
            $ forAll manyRandomPaths
            $ \ks -> do
                let values = zipWith indirect ks (intHash <$> [1 ..])
                    collected = evalPureFromEmptyDB $ do
                        insertHashes values
                        runPureTransaction hashCodecs
                            $ collectValues StandaloneCSMTCol [] []
                collected
                    `shouldBe` sort values
    describe "generateProof" $ do
        it "returns Nothing for empty tree (no sentinel root)"
            $ let mp =
                    evalPureFromEmptyDB
                        $ runPureTransaction hashCodecs
                        $ generateProof StandaloneCSMTCol [] []
              in  mp `shouldBe` Nothing
        it "can generate proof for simple tree"
            $ let
                mp = evalPureFromEmptyDB $ do
                    insertWord64s [indirect [L] 1]
                    runPureTransaction word64Codecs
                        $ generateProof StandaloneCSMTCol [] []
              in
                fmap cpMergeOps mp `shouldBe` Just []
        it "can generate proof for larger tree"
            $ let
                mp = evalPureFromEmptyDB $ do
                    insertWord64s
                        [ indirect [L] 1
                        , indirect [R] 2
                        ]
                    runPureTransaction word64Codecs
                        $ generateProof StandaloneCSMTCol [] []
              in
                fmap cpMergeOps mp `shouldBe` Just [(0, 1)]
        it "can generate proof for even larger tree"
            $ let
                mp = evalPureFromEmptyDB $ do
                    insertWord64s
                        [ indirect [L, L, L, L] 1
                        , indirect [L, R, L, L] 5
                        , indirect [L, R, L, R] 6
                        , indirect [R, R, R, R] 16
                        ]
                    runPureTransaction word64Codecs
                        $ generateProof StandaloneCSMTCol [] []
              in
                fmap cpMergeOps mp
                    `shouldBe` Just [(1, 2), (0, 1), (0, 3)]
    describe "verifyProof" $ do
        it "can verify completeness proof for larger tree"
            $ let
                values =
                    [ indirect [L, L, L, L] 1
                    , indirect [L, R, L, L] 5
                    , indirect [L, R, L, R] 6
                    , indirect [R, R, R, R] 16
                    ]
                (mp, r) = evalPureFromEmptyDB $ do
                    insertWord64s values
                    mp' <-
                        runPureTransaction word64Codecs
                            $ generateProof StandaloneCSMTCol [] []
                    r' <-
                        runPureTransaction word64Codecs
                            $ query StandaloneCSMTCol []
                    return (mp', r')
              in
                case (mp, r) of
                    (Just proof, Just rootIndirect) ->
                        let tr = rootHash word64Hashing rootIndirect
                        in  foldCompletenessProof
                                word64Hashing
                                tr
                                []
                                values
                                proof
                                `shouldBe` Just tr
                    _ -> error "expected a proof"
        it "can verify completeness proof for random trees"
            $ property
            $ forAll manyRandomPaths
            $ \ks -> do
                let values = zipWith indirect ks [1 ..]
                    (mp, r) = evalPureFromEmptyDB $ do
                        insertWord64s values
                        mp' <-
                            runPureTransaction word64Codecs
                                $ generateProof StandaloneCSMTCol [] []
                        r' <-
                            runPureTransaction word64Codecs
                                $ query StandaloneCSMTCol []
                        return (mp', r')
                case (mp, r) of
                    (Just proof, Just rootIndirect) ->
                        let tr = rootHash word64Hashing rootIndirect
                        in  foldCompletenessProof
                                word64Hashing
                                tr
                                []
                                (sort values)
                                proof
                                `shouldBe` Just tr
                    _ -> error "expected a proof"
        it "can verify completeness proof for random trees of hashes"
            $ property
            $ forAll manyRandomPaths
            $ \ks -> do
                let values = zipWith indirect ks (intHash <$> [1 ..])
                    (mp, r) = evalPureFromEmptyDB $ do
                        insertHashes values
                        mp' <-
                            runPureTransaction hashCodecs
                                $ generateProof StandaloneCSMTCol [] []
                        r' <-
                            runPureTransaction hashCodecs
                                $ query StandaloneCSMTCol []
                        return (mp', r')
                case (mp, r) of
                    (Just proof, Just rootIndirect) ->
                        let tr = rootHash hashHashing rootIndirect
                        in  foldCompletenessProof
                                hashHashing
                                tr
                                []
                                (sort values)
                                proof
                                `shouldBe` Just tr
                    _ -> error "expected a proof"

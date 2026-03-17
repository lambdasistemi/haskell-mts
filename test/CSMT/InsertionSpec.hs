{-# LANGUAGE OverloadedLists #-}

module CSMT.InsertionSpec (spec)
where

import CSMT
    ( Direction (L, R)
    , Indirect
    , Key
    )
import CSMT.Backend.Pure
    ( Pure
    , emptyInMemoryDB
    , inMemoryCSMTParsed
    , runPure
    )
import CSMT.Test.Lib
    ( ListOf
    , element
    , genSomePaths
    , insertBatchWord64M
    , insertBucketedWord64M
    , insertMWord64
    , list
    , node
    , word64Codecs
    )
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (forAll, (===))
import Prelude

hasExpectedDB
    :: Pure b -> ListOf (Key, Indirect Word64) () -> Expectation
hasExpectedDB program expectedDB =
    let (_, r) = runPure emptyInMemoryDB program
    in  inMemoryCSMTParsed word64Codecs r `shouldBe` mkDb expectedDB

record :: Key -> Key -> Word64 -> ListOf (Key, Indirect Word64) ()
record p k v = element (p, node k v)

i :: Key -> Word64 -> Pure ()
i = insertMWord64

mkDb :: ListOf (Key, Indirect a) () -> Map Key (Indirect a)
mkDb = Map.fromList . list

spec :: Spec
spec = do
    describe "insertion with ints" $ do
        it "inserts 1 key L" $ do
            let p = i [L] 1
                db = record [] [L] 1
            p `hasExpectedDB` db
        it "inserts 1 key R" $ do
            let p = i [R] 1
                db = record [] [R] 1
            p `hasExpectedDB` db
        it "inserts 1 key LL" $ do
            let p = i [L, L] 1
                db = record [] [L, L] 1
            p `hasExpectedDB` db
        it "inserts 1 key LR" $ do
            let p = i [L, R] 1
                db = record [] [L, R] 1
            p `hasExpectedDB` db
        it "inserts 2 keys R and L" $ do
            let p = do
                    i [R] 2
                    i [L] 1
                db = do
                    record [] [] 3
                    record [L] [] 1
                    record [R] [] 2
            p `hasExpectedDB` db
        it "inserts 2 keys L and R" $ do
            let p = do
                    i [L] 1
                    i [R] 2
                db = do
                    record [] [] 3
                    record [L] [] 1
                    record [R] [] 2
             in p `hasExpectedDB` db
        it "inserts 2 keys LL and LR"
            $ let
                p = do
                    i [L, L] 1
                    i [L, R] 2
                db = do
                    record [] [L] 3
                    record [L, L] [] 1
                    record [L, R] [] 2
              in
                p `hasExpectedDB` db

        it "inserts 2 keys RR and LL" $ do
            let p = do
                    i [L, L] 1
                    i [R, R] 2
                db = do
                    record [] [] 4
                    record [L] [L] 1
                    record [R] [R] 2
            p `hasExpectedDB` db
        it "inserts 2 keys LR and RL" $ do
            let p = do
                    i [L, R] 1
                    i [R, L] 2
                db = do
                    record [] [] 4
                    record [L] [R] 1
                    record [R] [L] 2
            p `hasExpectedDB` db
        it "inserts 2 keys RR and RL and LL" $ do
            let
                p = do
                    i [R, L] 1
                    i [R, R] 2
                    i [L, L] 3
                db = do
                    record [] [] 6
                    record [R] [] 3
                    record [R, L] [] 1
                    record [R, R] [] 2
                    record [L] [L] 3
            p `hasExpectedDB` db
        it "inserts 3 keys LL, RL, LR" $ do
            let p = do
                    i [L, L] 1
                    i [R, L] 2
                    i [L, R] 3
                db = do
                    record [] [] 6
                    record [L] [] 4
                    record [L, L] [] 1
                    record [L, R] [] 3
                    record [R] [L] 2
            p `hasExpectedDB` db
        it "inserts 3 keys LL, LR, RL" $ do
            let p = do
                    i [L, L] 1
                    i [L, R] 2
                    i [R, L] 3
                db = do
                    record [] [] 6
                    record [L] [] 3
                    record [R] [L] 3
                    record [L, L] [] 1
                    record [L, R] [] 2
            p `hasExpectedDB` db

        it "cover the docs example" $ do
            let p = do
                    i [L, L, R, R] 13
                    i [L, R, R, L] 5
                    i [L, R, R, R] 19
                    i [R, L, R, L] 23
                db = do
                    record [] [] 66
                    record [L] [] 41
                    record [L, R] [R] 24
                    record [L, L] [R, R] 13
                    record [L, R, R, L] [] 5
                    record [L, R, R, R] [] 19
                    record [R] [L, R, L] 23
            p `hasExpectedDB` db

    describe "batch insertion" $ do
        it "batch insert produces same result as sequential for 3 items" $ do
            let kvs = [([L, L], 1), ([L, R], 2), ([R, L], 3)]
                seqResult = snd $ runPure emptyInMemoryDB $ mapM_ (uncurry i) kvs
                batchResult =
                    snd $ runPure emptyInMemoryDB $ insertBatchWord64M kvs
            inMemoryCSMTParsed word64Codecs batchResult
                `shouldBe` inMemoryCSMTParsed word64Codecs seqResult

        it "batch insert produces same result for docs example" $ do
            let kvs =
                    [ ([L, L, R, R], 13)
                    , ([L, R, R, L], 5)
                    , ([L, R, R, R], 19)
                    , ([R, L, R, L], 23)
                    ]
                seqResult = snd $ runPure emptyInMemoryDB $ mapM_ (uncurry i) kvs
                batchResult =
                    snd $ runPure emptyInMemoryDB $ insertBatchWord64M kvs
            inMemoryCSMTParsed word64Codecs batchResult
                `shouldBe` inMemoryCSMTParsed word64Codecs seqResult

        prop "batch insert equals sequential insert"
            $ forAll (genSomePaths 8)
            $ \keys ->
                let kvs = zip keys [1 ..]
                    seqResult = snd $ runPure emptyInMemoryDB $ mapM_ (uncurry i) kvs
                    batchResult =
                        snd $ runPure emptyInMemoryDB $ insertBatchWord64M kvs
                in  inMemoryCSMTParsed word64Codecs batchResult
                        === inMemoryCSMTParsed word64Codecs seqResult

    describe "bucketed insertion" $ do
        it "bucketed insert (1 bit) produces same result for 3 items" $ do
            let kvs = [([L, L], 1), ([L, R], 2), ([R, L], 3)]
                seqResult = snd $ runPure emptyInMemoryDB $ mapM_ (uncurry i) kvs
                bucketedResult =
                    snd $ runPure emptyInMemoryDB $ insertBucketedWord64M 1 kvs
            inMemoryCSMTParsed word64Codecs bucketedResult
                `shouldBe` inMemoryCSMTParsed word64Codecs seqResult

        it "bucketed insert (2 bits) produces same result for docs example" $ do
            let kvs =
                    [ ([L, L, R, R], 13)
                    , ([L, R, R, L], 5)
                    , ([L, R, R, R], 19)
                    , ([R, L, R, L], 23)
                    ]
                seqResult = snd $ runPure emptyInMemoryDB $ mapM_ (uncurry i) kvs
                bucketedResult =
                    snd $ runPure emptyInMemoryDB $ insertBucketedWord64M 2 kvs
            inMemoryCSMTParsed word64Codecs bucketedResult
                `shouldBe` inMemoryCSMTParsed word64Codecs seqResult

        prop "bucketed insert (1 bit) equals sequential insert"
            $ forAll (genSomePaths 8)
            $ \keys ->
                let kvs = zip keys [1 ..]
                    seqResult = snd $ runPure emptyInMemoryDB $ mapM_ (uncurry i) kvs
                    bucketedResult =
                        snd $ runPure emptyInMemoryDB $ insertBucketedWord64M 1 kvs
                in  inMemoryCSMTParsed word64Codecs bucketedResult
                        === inMemoryCSMTParsed word64Codecs seqResult

        prop "bucketed insert (2 bits) equals sequential insert"
            $ forAll (genSomePaths 8)
            $ \keys ->
                let kvs = zip keys [1 ..]
                    seqResult = snd $ runPure emptyInMemoryDB $ mapM_ (uncurry i) kvs
                    bucketedResult =
                        snd $ runPure emptyInMemoryDB $ insertBucketedWord64M 2 kvs
                in  inMemoryCSMTParsed word64Codecs bucketedResult
                        === inMemoryCSMTParsed word64Codecs seqResult

        prop "bucketed insert (4 bits) equals sequential insert"
            $ forAll (genSomePaths 8)
            $ \keys ->
                let kvs = zip keys [1 ..]
                    seqResult = snd $ runPure emptyInMemoryDB $ mapM_ (uncurry i) kvs
                    bucketedResult =
                        snd $ runPure emptyInMemoryDB $ insertBucketedWord64M 4 kvs
                in  inMemoryCSMTParsed word64Codecs bucketedResult
                        === inMemoryCSMTParsed word64Codecs seqResult

{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module CSMT.DeletionSpec (spec)
where

import CSMT
    ( Direction (L, R)
    , Standalone (StandaloneCSMTCol)
    )
import CSMT.Backend.Pure
    ( emptyInMemoryDB
    , inMemoryCSMTParsed
    , runPure
    , runPureTransaction
    )
import CSMT.Deletion
    ( DeletionPath (..)
    , deletionPathToOps
    , newDeletionPath
    )
import CSMT.Interface (Key)
import CSMT.Test.Lib
    ( deleteWord64
    , genPaths
    , identityFromKV
    , insertWord64
    , inserted
    , mkDeletionPath
    , node
    , word64Codecs
    , word64Hashing
    )
import Data.Word (Word64)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck
    ( Gen
    , elements
    , forAll
    , getSize
    , shuffle
    )

spec :: Spec
spec = do
    describe "deletion" $ do
        it "constructs a deletion path for a singleton tree"
            $ let
                rs0 = insertWord64 emptyInMemoryDB [] (1 :: Word64)
                (mp, _) = do
                    runPure rs0
                        $ runPureTransaction word64Codecs
                        $ newDeletionPath [] StandaloneCSMTCol []
              in
                mp `shouldBe` Just (Value [] 1)
        it "constructs a deletion path for a tree with siblings"
            $ let
                rs0 = insertWord64 emptyInMemoryDB [L] (1 :: Word64)
                rs1 = insertWord64 rs0 [R] (2 :: Word64)
                mp = mkDeletionPath word64Codecs rs1 [L]
              in
                mp
                    `shouldBe` Just
                        (Branch [] L (Value [] 1) (node [] 2))
        it "constructs a deletion path for a tree with jumps"
            $ let
                rs0 = insertWord64 emptyInMemoryDB [L, L] (1 :: Word64)
                rs1 = insertWord64 rs0 [L, R] (2 :: Word64)
                mp = mkDeletionPath word64Codecs rs1 [L, L]
              in
                mp `shouldBe` Just (Branch [L] L (Value [] 1) (node [] 2))
        it "constructs a deletion path for a deeper tree with jumps"
            $ let
                rs0 = insertWord64 emptyInMemoryDB [L, L, R] (1 :: Word64)
                rs1 = insertWord64 rs0 [L, R, L] (2 :: Word64)
                rs2 = insertWord64 rs1 [R, L, R] (3 :: Word64)
                mp0 = mkDeletionPath word64Codecs rs2 [L, L, R]
                mp1 = mkDeletionPath word64Codecs rs2 [L, R, L]
                mp2 = mkDeletionPath word64Codecs rs2 [R, L, R]
                rs3 = deleteWord64 rs2 [R, L, R]
              in
                do
                    mp0
                        `shouldBe` Just
                            ( Branch
                                []
                                L
                                ( Branch
                                    []
                                    L
                                    (Value [R] 1)
                                    (node [L] 2)
                                )
                                (node [L, R] 3)
                            )
                    mp1
                        `shouldBe` Just
                            ( Branch
                                []
                                L
                                ( Branch
                                    []
                                    R
                                    (Value [L] 2)
                                    (node [R] 1)
                                )
                                (node [L, R] 3)
                            )
                    mp2
                        `shouldBe` Just
                            ( Branch
                                []
                                R
                                (Value [L, R] 3)
                                (node [] 4)
                            )
                    rs3 `shouldBe` rs1

        it "constructs a deletion path for a deeper tree with jumps"
            $ let
                rs0 = insertWord64 emptyInMemoryDB [L, L, L] (1 :: Word64)
                rs1 = insertWord64 rs0 [L, L, R] (2 :: Word64)
                rs2 = insertWord64 rs1 [R, R, R] (3 :: Word64)
                mp0 = mkDeletionPath word64Codecs rs2 [L, L, L]
                mp1 = mkDeletionPath word64Codecs rs2 [L, L, R]
                mp2 = mkDeletionPath word64Codecs rs2 [R, R, R]
              in
                do
                    mp0
                        `shouldBe` Just
                            ( Branch
                                []
                                L
                                ( Branch
                                    [L]
                                    L
                                    (Value [] 1)
                                    (node [] 2)
                                )
                                (node [R, R] 3)
                            )
                    mp1
                        `shouldBe` Just
                            ( Branch
                                []
                                L
                                ( Branch
                                    [L]
                                    R
                                    (Value [] 2)
                                    (node [] 1)
                                )
                                (node [R, R] 3)
                            )
                    mp2
                        `shouldBe` Just
                            ( Branch
                                []
                                R
                                (Value [R, R] 3)
                                (node [L] 3)
                            )

        it "deletes the singleton tree"
            $ let
                rs0 = insertWord64 emptyInMemoryDB [] (1 :: Word64)
                rs1 = deleteWord64 rs0 []
              in
                rs1 `shouldBe` emptyInMemoryDB
        it "deletes a single key"
            $ let
                rs0 = insertWord64 emptyInMemoryDB [L] (1 :: Word64)
                rs1 = deleteWord64 rs0 [L]
              in
                rs1 `shouldBe` emptyInMemoryDB
        it "deletes one of two sibling keys"
            $ let
                rs0 = insertWord64 emptyInMemoryDB [L] (1 :: Word64)
                rs1 = insertWord64 rs0 [R] (2 :: Word64)
                rs2 = deleteWord64 rs1 [R]
              in
                rs2 `shouldBe` rs0
        it "deletes one of four cousins keys"
            $ let
                rs0 = insertWord64 emptyInMemoryDB [L, L] (1 :: Word64)
                rs1 = insertWord64 rs0 [L, R] (2 :: Word64)
                rs2 = insertWord64 rs1 [R, L] (3 :: Word64)
                rs3 = insertWord64 rs2 [R, R] (4 :: Word64)
                dll = deleteWord64 rs3 [L, L]
                dlr = deleteWord64 rs3 [L, R]
                drl = deleteWord64 rs3 [R, L]
                drr = deleteWord64 rs3 [R, R]
              in
                do
                    inMemoryCSMTParsed word64Codecs dll
                        `shouldBe` [ ([], node [] 10)
                                   , ([R], node [] 7)
                                   , ([L], node [R] 2)
                                   , ([R, L], node [] 3)
                                   , ([R, R], node [] 4)
                                   ]
                    inMemoryCSMTParsed word64Codecs dlr
                        `shouldBe` [ ([], node [] 8)
                                   , ([R], node [] 7)
                                   , ([L], node [L] 1)
                                   , ([R, L], node [] 3)
                                   , ([R, R], node [] 4)
                                   ]
                    inMemoryCSMTParsed word64Codecs drl
                        `shouldBe` [ ([], node [] 8)
                                   , ([L], node [] 3)
                                   , ([R], node [R] 4)
                                   , ([L, L], node [] 1)
                                   , ([L, R], node [] 2)
                                   ]
                    inMemoryCSMTParsed word64Codecs drr
                        `shouldBe` [ ([], node [] 6)
                                   , ([L], node [] 3)
                                   , ([R], node [L] 3)
                                   , ([L, L], node [] 1)
                                   , ([L, R], node [] 2)
                                   ]
        it "deletes 2 of four cousin keys"
            $ let
                rs0 = insertWord64 emptyInMemoryDB [L, L] (1 :: Word64)
                rs1 = insertWord64 rs0 [L, R] (2 :: Word64)
                rs2 = insertWord64 rs1 [R, L] (3 :: Word64)
                rs3 = insertWord64 rs2 [R, R] (4 :: Word64)
                dlrl = deleteWord64 (deleteWord64 rs3 [L, L]) [R, R]
              in
                do
                    inMemoryCSMTParsed word64Codecs dlrl
                        `shouldBe` [ ([], node [] 6)
                                   , ([L], node [R] 2)
                                   , ([R], node [L] 3)
                                   ]
        it "deletes 3 of four cousin keys"
            $ let
                rs0 = insertWord64 emptyInMemoryDB [L, L] (1 :: Word64)
                rs1 = insertWord64 rs0 [L, R] (2 :: Word64)
                rs2 = insertWord64 rs1 [R, L] (3 :: Word64)
                rs3 = insertWord64 rs2 [R, R] (4 :: Word64)
                dll = deleteWord64 rs3 [L, L]
                drr = deleteWord64 dll [R, R]
                dlr = deleteWord64 drr [L, R]
              in
                do
                    inMemoryCSMTParsed word64Codecs dlr
                        `shouldBe` [ ([], node [R, L] 3)
                                   ]
        it "computes the right deletion path for [[L, R], [R, L]]"
            $ let
                rs0 = emptyInMemoryDB
                rs1 = insertWord64 rs0 [L, R] (2 :: Word64)
                rs2 = insertWord64 rs1 [R, L] (3 :: Word64)
                mp = mkDeletionPath word64Codecs rs2 [L, R]
              in
                mp
                    `shouldBe` Just
                        ( Branch
                            []
                            L
                            (Value [R] 2)
                            (node [L] 3)
                        )
        it "computes the right ops to delete [L,R] from [[L, R], [R, L]]"
            $ let
                rs0 = emptyInMemoryDB
                rs1 = insertWord64 rs0 [L, R] (2 :: Word64)
                rs2 = insertWord64 rs1 [R, L] (3 :: Word64)
                Just mp = mkDeletionPath word64Codecs rs2 [L, R]
                ops = deletionPathToOps [] word64Hashing mp
              in
                ops
                    `shouldBe` [ ([], Just $ node [R, L] 3)
                               , ([R], Nothing)
                               , ([L], Nothing)
                               ]
        it
            "inserting some facts and deleting them in a random order results in an empty tree"
            $ forAll (elements [1 .. 19])
            $ \n -> forAll (randomPaths n) $ \(inserting, deleting) -> do
                let
                    kvs = zip inserting [1 :: Word64 .. 2 ^ n]
                    full = inserted word64Codecs identityFromKV word64Hashing kvs
                    emptied = foldl deleteWord64 full deleting
                emptied `shouldBe` emptyInMemoryDB

randomPaths :: Word64 -> Gen ([Key], [Key])
randomPaths k = do
    xs <- genPaths k
    ys <- shuffle xs
    n <- getSize
    let ys' = take n ys
    zs <- shuffle ys'
    return (ys', zs)

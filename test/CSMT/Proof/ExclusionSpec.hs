{-# LANGUAGE OverloadedLists #-}

module CSMT.Proof.ExclusionSpec (spec) where

import CSMT (Direction (L, R), Key)
import CSMT.Backend.Pure
    ( emptyInMemoryDB
    , pureDatabase
    , runPure
    )
import CSMT.Backend.Standalone
    ( Standalone (StandaloneCSMTCol)
    )
import CSMT.Interface (root)
import CSMT.Proof.Exclusion
    ( ExclusionProof (..)
    , buildExclusionProof
    , verifyExclusionProof
    )
import CSMT.Proof.Insertion (verifyInclusionProof)
import CSMT.Test.Lib
    ( insertMWord64
    , word64Codecs
    , word64Hashing
    )
import Data.List (nub)
import Data.Word (Word64)
import Database.KV.Transaction (runTransactionUnguarded)
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldSatisfy
    )
import Test.QuickCheck
    ( Gen
    , Property
    , choose
    , counterexample
    , elements
    , forAll
    , property
    , vectorOf
    , (===)
    )

-- | Generate an exclusion proof and root hash in the pure backend.
exclusionProofFor
    :: [(Key, Word64)]
    -> Key
    -> (Maybe (ExclusionProof Word64), Maybe Word64)
exclusionProofFor inserts targetKey =
    fst
        $ runPure emptyInMemoryDB
        $ do
            mapM_ (uncurry insertMWord64) inserts
            runTransactionUnguarded
                (pureDatabase word64Codecs)
                $ do
                    ep <-
                        buildExclusionProof
                            []
                            StandaloneCSMTCol
                            targetKey
                    mr <- root word64Hashing StandaloneCSMTCol []
                    pure (ep, mr)

isWitness :: Maybe (ExclusionProof a) -> Bool
isWitness (Just ExclusionWitness{}) = True
isWitness _ = False

-- | Generate a fixed-length key.
genFixedKey :: Int -> Gen Key
genFixedKey n = vectorOf n (elements [L, R])

-- | Generate a set of distinct keys and a key NOT in the set.
-- Uses long keys (16 bits) so collisions are extremely unlikely.
genTreeAndAbsentKey :: Gen ([Key], Key)
genTreeAndAbsentKey = do
    numKeys <- choose (2, 10)
    keys <- nub <$> vectorOf numKeys (genFixedKey 16)
    absent <- genFixedKey 16
    if absent `elem` keys
        then genTreeAndAbsentKey
        else pure (keys, absent)

-- | Generate a set of keys with values.
genInserts :: [Key] -> Gen [(Key, Word64)]
genInserts keys =
    zip keys <$> vectorOf (length keys) (choose (1, 10000))

spec :: Spec
spec = describe "CSMT.Proof.Exclusion" $ do
    describe "empty tree" $ do
        it "generates ExclusionEmpty"
            $ fst (exclusionProofFor [] [L, R, L])
            `shouldBe` Just ExclusionEmpty

        it "ExclusionEmpty verifies"
            $ verifyExclusionProof
                word64Hashing
                0
                ExclusionEmpty
            `shouldBe` True

    describe "single-element tree" $ do
        it "excludes absent key"
            $ let (result, mr) =
                    exclusionProofFor [([L], 1)] [R]
              in  case (result, mr) of
                    (Just proof, Just r) ->
                        verifyExclusionProof
                            word64Hashing
                            r
                            proof
                            `shouldBe` True
                    _ ->
                        result `shouldSatisfy` isWitness

        it "returns Nothing for present key"
            $ fst (exclusionProofFor [([L], 1)] [L])
            `shouldBe` Nothing

    describe "multi-element tree" $ do
        it "excludes key with jump divergence"
            $ let inserts =
                    [ ([L, L, L], 1)
                    , ([L, L, R], 2)
                    , ([R, R, R], 3)
                    ]
                  (result, mr) =
                    exclusionProofFor inserts [L, R, L]
              in  case (result, mr) of
                    (Just proof, Just r) ->
                        verifyExclusionProof
                            word64Hashing
                            r
                            proof
                            `shouldBe` True
                    _ ->
                        result `shouldSatisfy` isWitness

        it "excludes deeply absent key"
            $ let inserts =
                    [ ([L, L, L, L], 1)
                    , ([L, L, L, R], 2)
                    , ([L, L, R, L], 3)
                    , ([R, R, R, R], 4)
                    ]
                  (result, mr) =
                    exclusionProofFor inserts [L, L, R, R]
              in  case (result, mr) of
                    (Just proof, Just r) ->
                        verifyExclusionProof
                            word64Hashing
                            r
                            proof
                            `shouldBe` True
                    _ ->
                        result `shouldSatisfy` isWitness

        it "returns Nothing for present key"
            $ let inserts =
                    [ ([L, L], 1)
                    , ([L, R], 2)
                    , ([R, L], 3)
                    ]
              in  fst (exclusionProofFor inserts [L, R])
                    `shouldBe` Nothing

    describe "property tests" $ do
        it "absent key always produces a verifiable proof"
            $ property propAbsentKeyProves

        it "present key always returns Nothing"
            $ property propPresentKeyFails

        it "tampered target key fails verification"
            $ property propTamperedTargetFails

propAbsentKeyProves :: Property
propAbsentKeyProves =
    forAll genTreeAndAbsentKey $ \(keys, absent) ->
        forAll (genInserts keys) $ \inserts ->
            let (result, mr) =
                    exclusionProofFor inserts absent
            in  counterexample
                    ( "keys="
                        ++ show keys
                        ++ " absent="
                        ++ show absent
                        ++ " result="
                        ++ show
                            (isWitness result)
                    )
                    $ case (result, mr) of
                        (Just (ExclusionWitness _ wp), Just r) ->
                            counterexample
                                ( "inclusionValid="
                                    ++ show
                                        ( verifyInclusionProof
                                            word64Hashing
                                            r
                                            wp
                                        )
                                )
                                $ verifyExclusionProof
                                    word64Hashing
                                    r
                                    (ExclusionWitness absent wp)
                                    === True
                        (Just ExclusionEmpty, _) ->
                            property True
                        _ ->
                            property False

propPresentKeyFails :: Property
propPresentKeyFails =
    forAll genTreeAndAbsentKey $ \(keys, _) ->
        forAll (genInserts keys) $ \inserts ->
            forAll (elements keys) $ \present ->
                fst (exclusionProofFor inserts present)
                    === Nothing

propTamperedTargetFails :: Property
propTamperedTargetFails =
    forAll genTreeAndAbsentKey $ \(keys, absent) ->
        forAll (genInserts keys) $ \inserts ->
            forAll (elements keys) $ \present ->
                let (result, mr) =
                        exclusionProofFor inserts absent
                in  case (result, mr) of
                        ( Just
                                ( ExclusionWitness
                                        { epWitnessProof = wp
                                        }
                                    )
                            , Just r
                            ) ->
                                -- Swap target with a key that EXISTS
                                let tampered =
                                        ExclusionWitness
                                            { epTargetKey = present
                                            , epWitnessProof = wp
                                            }
                                in  counterexample
                                        ( "present="
                                            ++ show present
                                        )
                                        $ verifyExclusionProof
                                            word64Hashing
                                            r
                                            tampered
                                            === False
                        _ -> property True

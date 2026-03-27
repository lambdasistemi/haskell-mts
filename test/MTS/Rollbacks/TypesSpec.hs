{-# LANGUAGE ScopedTypeVariables #-}

module MTS.Rollbacks.TypesSpec (spec) where

import MTS.Rollbacks.Types
    ( Operation (..)
    , RollbackPoint (..)
    , inverseOf
    )
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

-- | Type alias for conciseness
type Op = Operation Int Int

-- | Generate an arbitrary Operation
genOp :: Gen Op
genOp =
    oneof
        [ Insert <$> arbitrary <*> arbitrary
        , Delete <$> arbitrary
        ]

spec :: Spec
spec = describe "MTS.Rollbacks.Types" $ do
    describe "inverseOf" $ do
        prop "insert into empty yields Delete"
            $ forAll
                ( (,)
                    <$> (arbitrary :: Gen Int)
                    <*> (arbitrary :: Gen Int)
                )
            $ \(k, v) ->
                inverseOf (Insert k v) Nothing
                    === Just (Delete k :: Op)

        prop "insert over existing yields Insert old"
            $ forAll
                ( (,,)
                    <$> (arbitrary :: Gen Int)
                    <*> (arbitrary :: Gen Int)
                    <*> (arbitrary :: Gen Int)
                )
            $ \(k, v, old) ->
                inverseOf (Insert k v) (Just old)
                    === Just (Insert k old :: Op)

        prop "delete from empty yields Nothing"
            $ forAll (arbitrary :: Gen Int)
            $ \k ->
                inverseOf (Delete k :: Op) Nothing
                    === (Nothing :: Maybe Op)

        prop "delete from existing yields Insert"
            $ forAll
                ( (,)
                    <$> (arbitrary :: Gen Int)
                    <*> (arbitrary :: Gen Int)
                )
            $ \(k, v) ->
                inverseOf (Delete k :: Op) (Just v)
                    === Just (Insert k v)

        prop "double inverse recovers Insert"
            $ forAll
                ( (,,)
                    <$> (arbitrary :: Gen Int)
                    <*> (arbitrary :: Gen Int)
                    <*> (arbitrary :: Gen (Maybe Int))
                )
            $ \(k, v, current) ->
                let op = Insert k v :: Op
                    inv = inverseOf op current
                in  case inv of
                        Nothing -> property True
                        Just invOp ->
                            inverseOf invOp (Just v)
                                === Just op

        prop "double inverse recovers Delete"
            $ forAll
                ( (,)
                    <$> (arbitrary :: Gen Int)
                    <*> (arbitrary :: Gen Int)
                )
            $ \(k, v) ->
                let op = Delete k :: Op
                in  case inverseOf op (Just v) of
                        Just invOp ->
                            inverseOf invOp Nothing
                                === Just op
                        Nothing -> property Discard

    describe "RollbackPoint" $ do
        prop "Eq is reflexive"
            $ forAll
                ( RollbackPoint
                    <$> listOf genOp
                    <*> (arbitrary :: Gen (Maybe Int))
                )
            $ \rp -> rp === rp

        prop "Show does not crash"
            $ forAll
                ( RollbackPoint
                    <$> listOf genOp
                    <*> (arbitrary :: Gen (Maybe Int))
                )
            $ \rp -> not (null (show rp))

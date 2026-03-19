{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module MTS.RollbacksSpec (spec) where

import Control.Monad (forM_, when)
import Control.Monad.Catch.Pure (Catch, runCatch)
import Control.Monad.Trans.State.Strict
    ( StateT (runStateT)
    , gets
    , modify'
    )
import Data.ByteString (ByteString)
import Data.Function (fix)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Type.Equality ((:~:) (..))
import Database.KV.Database
    ( Database (..)
    , Pos (..)
    , QueryIterator (..)
    )
import Database.KV.Transaction
    ( Codecs (..)
    , Column (..)
    , DMap
    , DSum (..)
    , GCompare (..)
    , GEq (..)
    , GOrdering (..)
    , KV
    , Transaction
    , fromPairList
    , runTransactionUnguarded
    )
import MTS.Rollbacks.Store
    ( RollbackCounter (..)
    , RollbackResult (..)
    , armageddonSetup
    , countPoints
    , countedArmageddonCleanup
    , countedArmageddonSetup
    , countedPruneBelow
    , countedRollbackTo
    , countedStore
    , readCount
    , storeRollbackPoint
    )
import MTS.Rollbacks.Types
    ( RollbackPoint (..)
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import Test.QuickCheck
    ( Gen
    , chooseInt
    , forAll
    , listOf1
    , property
    )

import Control.Lens (Prism', iso, prism')
import Data.IORef
    ( newIORef
    , readIORef
    , writeIORef
    )
import Data.Serialize
    ( getWord64be
    , putWord64be
    )
import Data.Serialize.Extra (evalGetM, evalPutM)
import Data.Word (Word64)

-- ----------------------------------------------------------
-- Minimal pure backend for rollback + metrics columns
-- ----------------------------------------------------------

data TestCF = RollbackCF | MetricsCF
    deriving (Eq, Ord)

data TestCol c where
    RollbackCol
        :: TestCol (KV Int (RollbackPoint () ()))
    MetricsCol
        :: TestCol (KV ByteString Int)

instance GEq TestCol where
    geq RollbackCol RollbackCol = Just Refl
    geq MetricsCol MetricsCol = Just Refl
    geq _ _ = Nothing

instance GCompare TestCol where
    gcompare RollbackCol RollbackCol = GEQ
    gcompare RollbackCol MetricsCol = GLT
    gcompare MetricsCol RollbackCol = GGT
    gcompare MetricsCol MetricsCol = GEQ

type TestOp = (TestCF, ByteString, Maybe ByteString)

data TestDB = TestDB
    { dbRollbacks :: Map ByteString ByteString
    , dbMetrics :: Map ByteString ByteString
    , dbIterators :: Map Int TestCursor
    }

data TestCursor = TestCursor
    { curPos :: Maybe ByteString
    , curSnap :: Map ByteString ByteString
    }

type TestM = StateT TestDB Catch

runTestM :: TestDB -> TestM a -> (a, TestDB)
runTestM db m = case runCatch (runStateT m db) of
    Left e -> error $ "runTestM: " ++ show e
    Right r -> r

emptyTestDB :: TestDB
emptyTestDB = TestDB Map.empty Map.empty Map.empty

intPrism :: Prism' ByteString Int
intPrism = prism' encode decode
  where
    encode =
        evalPutM . putWord64be . fromIntegral
    decode =
        fmap fromIntegral . evalGetM getWord64be

rpPrism :: Prism' ByteString (RollbackPoint () ())
rpPrism = prism' encode decode
  where
    encode RollbackPoint{rpInverses, rpMeta} =
        evalPutM $ do
            putWord64be
                $ fromIntegral
                $ length rpInverses
            putWord64be
                $ case rpMeta of
                    Nothing -> 0
                    Just () -> 1
    decode = evalGetM $ do
        n <- fromIntegral <$> getWord64be
        meta <- getWord64be
        pure
            $ RollbackPoint
                { rpInverses =
                    replicate n ()
                , rpMeta =
                    if (meta :: Word64) == 0
                        then Nothing
                        else Just ()
                }

testCols
    :: DMap
        TestCol
        (Column TestCF)
testCols =
    fromPairList
        [ RollbackCol
            :=> Column
                { family = RollbackCF
                , codecs =
                    Codecs intPrism rpPrism
                }
        , MetricsCol
            :=> Column
                { family = MetricsCF
                , codecs =
                    Codecs (iso id id) intPrism
                }
        ]

pureValueAt
    :: TestCF
    -> ByteString
    -> TestM (Maybe ByteString)
pureValueAt RollbackCF k =
    gets (Map.lookup k . dbRollbacks)
pureValueAt MetricsCF k =
    gets (Map.lookup k . dbMetrics)

pureApplyOps :: [TestOp] -> TestM ()
pureApplyOps ops = forM_ ops $ \(cf, k, mv) ->
    case (cf, mv) of
        (RollbackCF, Nothing) ->
            modify'
                $ \db ->
                    db
                        { dbRollbacks =
                            Map.delete
                                k
                                (dbRollbacks db)
                        }
        (RollbackCF, Just v) ->
            modify'
                $ \db ->
                    db
                        { dbRollbacks =
                            Map.insert
                                k
                                v
                                (dbRollbacks db)
                        }
        (MetricsCF, Nothing) ->
            modify'
                $ \db ->
                    db
                        { dbMetrics =
                            Map.delete
                                k
                                (dbMetrics db)
                        }
        (MetricsCF, Just v) ->
            modify'
                $ \db ->
                    db
                        { dbMetrics =
                            Map.insert
                                k
                                v
                                (dbMetrics db)
                        }

mkTestOp
    :: TestCF
    -> ByteString
    -> Maybe ByteString
    -> TestOp
mkTestOp = (,,)

testIterator :: TestCF -> TestM (QueryIterator TestM)
testIterator cf = do
    snap <- gets $ case cf of
        RollbackCF -> dbRollbacks
        MetricsCF -> dbMetrics
    nextId <-
        gets
            $ \db -> case Map.lookupMax (dbIterators db) of
                Just (i, _) -> i + 1
                Nothing -> 0
    let cursor =
            TestCursor
                { curPos = Nothing
                , curSnap = snap
                }
    modify'
        $ \db ->
            db
                { dbIterators =
                    Map.insert
                        nextId
                        cursor
                        (dbIterators db)
                }
    pure
        $ QueryIterator
            { step = stepIt nextId
            , isValid = validIt nextId
            , entry = entryIt nextId
            }

stepIt :: Int -> Pos -> TestM ()
stepIt itId PosDestroy =
    modify'
        $ \db ->
            db
                { dbIterators =
                    Map.delete
                        itId
                        (dbIterators db)
                }
stepIt itId pos = do
    iters <- gets dbIterators
    case Map.lookup itId iters of
        Nothing ->
            error "stepIt: invalid iterator"
        Just c -> do
            let c' = case pos of
                    PosFirst ->
                        c
                            { curPos =
                                fst
                                    <$> Map.lookupMin
                                        (curSnap c)
                            }
                    PosLast ->
                        c
                            { curPos =
                                fst
                                    <$> Map.lookupMax
                                        (curSnap c)
                            }
                    PosNext ->
                        case curPos c of
                            Nothing -> c
                            Just k ->
                                let (_, after) =
                                        Map.split
                                            k
                                            (curSnap c)
                                in  c
                                        { curPos =
                                            fst
                                                <$> Map.lookupMin
                                                    after
                                        }
                    PosPrev ->
                        case curPos c of
                            Nothing -> c
                            Just k ->
                                let (before, _) =
                                        Map.split
                                            k
                                            (curSnap c)
                                in  c
                                        { curPos =
                                            fst
                                                <$> Map.lookupMax
                                                    before
                                        }
                    PosAny k ->
                        let (_, after) =
                                Map.split k (curSnap c)
                        in  c
                                { curPos =
                                    fst
                                        <$> Map.lookupMin
                                            after
                                }
            modify'
                $ \db ->
                    db
                        { dbIterators =
                            Map.insert
                                itId
                                c'
                                (dbIterators db)
                        }

validIt :: Int -> TestM Bool
validIt itId = do
    iters <- gets dbIterators
    case Map.lookup itId iters of
        Nothing -> error "validIt: invalid"
        Just c -> pure $ case curPos c of
            Just _ -> True
            Nothing -> False

entryIt
    :: Int
    -> TestM (Maybe (ByteString, ByteString))
entryIt itId = do
    iters <- gets dbIterators
    case Map.lookup itId iters of
        Nothing -> error "entryIt: invalid"
        Just c -> case curPos c of
            Nothing -> pure Nothing
            Just k ->
                pure
                    $ Map.lookup k (curSnap c)
                        >>= \v -> Just (k, v)

testDatabase
    :: Database TestM TestCF TestCol TestOp
testDatabase =
    let db =
            Database
                { valueAt = pureValueAt
                , applyOps = pureApplyOps
                , columns = testCols
                , mkOperation = mkTestOp
                , newIterator = testIterator
                , withSnapshot = \f -> f db
                }
    in  db

runTx
    :: Transaction TestM TestCF TestCol TestOp a
    -> TestM a
runTx = runTransactionUnguarded testDatabase

-- ----------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------

mkRP :: RollbackPoint () ()
mkRP =
    RollbackPoint
        { rpInverses = [()]
        , rpMeta = Nothing
        }

emptyRP :: RollbackPoint () ()
emptyRP =
    RollbackPoint
        { rpInverses = []
        , rpMeta = Nothing
        }

rc :: RollbackCounter TestCol
rc =
    RollbackCounter
        { rcSelector = MetricsCol
        , rcKey = "rollback-count"
        }

-- ----------------------------------------------------------
-- Generators
-- ----------------------------------------------------------

genSlots :: Gen [Int]
genSlots = do
    n <- chooseInt (1, 20)
    pure [1 .. n]

-- ----------------------------------------------------------
-- Spec
-- ----------------------------------------------------------

spec :: Spec
spec = describe "Rollbacks.Store counted" $ do
    it "count matches countPoints after stores"
        $ property
        $ forAll genSlots
        $ \slots -> do
            ref <- newIORef emptyTestDB
            let run :: forall a. TestM a -> IO a
                run action = do
                    s <- readIORef ref
                    let (a, s') = runTestM s action
                    writeIORef ref s'
                    pure a
            -- Setup sentinel at key 0
            run
                $ runTx
                $ countedArmageddonSetup
                    RollbackCol
                    rc
                    (0 :: Int)
                    Nothing
            -- Store rollback points
            forM_ slots $ \slot ->
                run
                    $ runTx
                    $ countedStore
                        RollbackCol
                        rc
                        slot
                        mkRP
            -- Compare
            cached <- run $ runTx $ readCount rc
            actual <-
                run $ runTx $ countPoints RollbackCol
            cached `shouldBe` actual

    it "count matches after rollback"
        $ property
        $ forAll genSlots
        $ \slots -> do
            ref <- newIORef emptyTestDB
            let run :: forall a. TestM a -> IO a
                run action = do
                    s <- readIORef ref
                    let (a, s') = runTestM s action
                    writeIORef ref s'
                    pure a
            run
                $ runTx
                $ countedArmageddonSetup
                    RollbackCol
                    rc
                    (0 :: Int)
                    Nothing
            forM_ slots $ \slot ->
                run
                    $ runTx
                    $ countedStore
                        RollbackCol
                        rc
                        slot
                        mkRP
            -- Rollback to midpoint
            let mid = length slots `div` 2
            run
                $ runTx
                $ countedRollbackTo
                    RollbackCol
                    rc
                    (const $ pure ())
                    mid
            cached <- run $ runTx $ readCount rc
            actual <-
                run $ runTx $ countPoints RollbackCol
            cached `shouldBe` actual

    it "count matches after prune"
        $ property
        $ forAll genSlots
        $ \slots -> do
            ref <- newIORef emptyTestDB
            let run :: forall a. TestM a -> IO a
                run action = do
                    s <- readIORef ref
                    let (a, s') = runTestM s action
                    writeIORef ref s'
                    pure a
            run
                $ runTx
                $ countedArmageddonSetup
                    RollbackCol
                    rc
                    (0 :: Int)
                    Nothing
            forM_ slots $ \slot ->
                run
                    $ runTx
                    $ countedStore
                        RollbackCol
                        rc
                        slot
                        mkRP
            -- Prune below midpoint
            let mid = length slots `div` 2
            _ <-
                run
                    $ runTx
                    $ countedPruneBelow
                        RollbackCol
                        rc
                        mid
            cached <- run $ runTx $ readCount rc
            actual <-
                run $ runTx $ countPoints RollbackCol
            cached `shouldBe` actual

    it "count is 1 after armageddon setup"
        $ do
            ref <- newIORef emptyTestDB
            let run :: forall a. TestM a -> IO a
                run action = do
                    s <- readIORef ref
                    let (a, s') = runTestM s action
                    writeIORef ref s'
                    pure a
            -- Store some points first
            run
                $ runTx
                $ storeRollbackPoint
                    RollbackCol
                    (1 :: Int)
                    mkRP
            run
                $ runTx
                $ storeRollbackPoint
                    RollbackCol
                    2
                    mkRP
            -- Armageddon
            run
                $ ($ ())
                $ fix
                $ \go _ -> do
                    more <-
                        runTx
                            $ countedArmageddonCleanup
                                RollbackCol
                                rc
                                100
                    when more $ go ()
            run
                $ runTx
                $ countedArmageddonSetup
                    RollbackCol
                    rc
                    (0 :: Int)
                    Nothing
            cached <- run $ runTx $ readCount rc
            cached `shouldBe` 1

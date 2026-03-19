module MTS.Rollbacks.StoreSpec (spec) where

import Control.Lens (prism')
import Control.Monad (forM_)
import Control.Monad.Catch.Pure (Catch, runCatch)
import Control.Monad.Trans.State.Strict
    ( StateT (runStateT)
    , gets
    , modify'
    )
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Serialize (decode, encode)
import Database.KV.Database
    ( Database (..)
    , Pos (..)
    , QueryIterator (..)
    )
import Database.KV.Transaction
    ( Codecs (..)
    , Column (..)
    , DSum (..)
    , Transaction
    , fromPairList
    , runTransactionUnguarded
    )
import MTS.Rollbacks.Column
    ( RollbackColumn (..)
    , RollbackKV
    )
import MTS.Rollbacks.Store
    ( countPoints
    , pruneExcess
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

-- ---------------------------------------------------------
-- Minimal pure in-memory backend for rollback column only
-- ---------------------------------------------------------

-- | Column family tag (only one).
data CF = RollbackCF

-- | Database operation.
type Op = (CF, ByteString, Maybe ByteString)

mkOp :: CF -> ByteString -> Maybe ByteString -> Op
mkOp = (,,)

-- | In-memory state: one map + cursor bookkeeping.
data Mem = Mem
    { store :: Map ByteString ByteString
    , iterators :: Map Int Cursor
    }

data Cursor = Cursor
    { position :: Maybe ByteString
    , snapshot :: Map ByteString ByteString
    }

type M = StateT Mem Catch

emptyMem :: Mem
emptyMem = Mem Map.empty Map.empty

runM :: Mem -> M a -> (a, Mem)
runM m action = case runCatch (runStateT action m) of
    Left err -> error $ "runM: " ++ show err
    Right r -> r

-- Codec for the rollback column
rbCodecs :: Codecs (RollbackKV Int () ())
rbCodecs =
    Codecs
        { keyCodec =
            prism' encode (either (const Nothing) Just . decode)
        , valueCodec =
            prism'
                (const mempty)
                (const $ Just $ RollbackPoint [] Nothing)
        }

rbDatabase :: Database M CF (RollbackColumn Int () ()) Op
rbDatabase =
    let db =
            Database
                { valueAt = \_cf k -> do
                    s <- gets store
                    pure $ Map.lookup k s
                , applyOps = \ops -> forM_ ops $ \(_cf, k, mv) ->
                    case mv of
                        Nothing -> modify' $ \m ->
                            m{store = Map.delete k (store m)}
                        Just v -> modify' $ \m ->
                            m{store = Map.insert k v (store m)}
                , columns =
                    fromPairList
                        [ RollbackPoints
                            :=> Column
                                { family = RollbackCF
                                , codecs = rbCodecs
                                }
                        ]
                , mkOperation = mkOp
                , newIterator = \_cf -> do
                    s <- gets store
                    nextId <- gets $ \m ->
                        case Map.lookupMax (iterators m) of
                            Just (i, _) -> i + 1
                            Nothing -> 0
                    modify' $ \m ->
                        m
                            { iterators =
                                Map.insert
                                    nextId
                                    ( Cursor
                                        { position = Nothing
                                        , snapshot = s
                                        }
                                    )
                                    (iterators m)
                            }
                    pure
                        QueryIterator
                            { step = stepIt nextId
                            , isValid = validIt nextId
                            , entry = entryIt nextId
                            }
                , withSnapshot = \f -> f db
                }
    in  db

stepIt :: Int -> Pos -> M ()
stepIt itId pos = do
    iters <- gets iterators
    case pos of
        PosDestroy -> modify' $ \m ->
            m{iterators = Map.delete itId (iterators m)}
        _ -> case Map.lookup itId iters of
            Nothing ->
                error "stepIt: invalid iterator"
            Just c -> do
                let c' = case pos of
                        PosFirst ->
                            c
                                { position =
                                    fst <$> Map.lookupMin (snapshot c)
                                }
                        PosLast ->
                            c
                                { position =
                                    fst <$> Map.lookupMax (snapshot c)
                                }
                        PosNext -> case position c of
                            Nothing -> c
                            Just k ->
                                let (_, after) = Map.split k (snapshot c)
                                in  c
                                        { position =
                                            fst <$> Map.lookupMin after
                                        }
                        PosPrev -> case position c of
                            Nothing -> c
                            Just k ->
                                let (before, _) =
                                        Map.split k (snapshot c)
                                in  c
                                        { position =
                                            fst <$> Map.lookupMax before
                                        }
                        PosAny k ->
                            let (_, after) = Map.split k (snapshot c)
                            in  c
                                    { position =
                                        fst <$> Map.lookupMin after
                                    }
                modify' $ \m ->
                    m
                        { iterators =
                            Map.insert itId c' (iterators m)
                        }

validIt :: Int -> M Bool
validIt itId = do
    iters <- gets iterators
    case Map.lookup itId iters of
        Nothing -> error "validIt: invalid iterator"
        Just c -> pure $ case position c of
            Just _ -> True
            Nothing -> False

entryIt
    :: Int -> M (Maybe (ByteString, ByteString))
entryIt itId = do
    iters <- gets iterators
    case Map.lookup itId iters of
        Nothing ->
            error "entryIt: invalid iterator"
        Just c -> case position c of
            Nothing -> pure Nothing
            Just k ->
                pure $ (k,) <$> Map.lookup k (snapshot c)

-- ---------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------

emptyRp :: RollbackPoint () ()
emptyRp = RollbackPoint [] Nothing

run :: M a -> (a, Mem)
run = runM emptyMem

-- ---------------------------------------------------------
-- Specs
-- ---------------------------------------------------------

-- | Run a transaction against the pure database.
runTx
    :: Transaction
        M
        CF
        (RollbackColumn Int () ())
        Op
        a
    -> M a
runTx = runTransactionUnguarded rbDatabase

-- | Store N rollback points with keys 1..n.
storeN :: Int -> M ()
storeN n = runTx
    $ forM_ [1 .. n]
    $ \k ->
        storeRollbackPoint
            RollbackPoints
            k
            emptyRp

spec :: Spec
spec = describe "MTS.Rollbacks.Store" $ do
    describe "pruneExcess" $ do
        it "does nothing on empty column" $ do
            let (pruned, _) = run $ do
                    runTx
                        $ pruneExcess RollbackPoints 5
            pruned `shouldBe` 0

        it "does nothing when count <= maxToKeep" $ do
            let (pruned, _) = run $ do
                    storeN 3
                    runTx
                        $ pruneExcess RollbackPoints 5
            pruned `shouldBe` 0

        it "prunes oldest when count > maxToKeep" $ do
            let ((pruned, remaining), _) = run $ do
                    storeN 5
                    p <-
                        runTx
                            $ pruneExcess RollbackPoints 2
                    c <-
                        runTx
                            $ countPoints RollbackPoints
                    pure (p, c)
            pruned `shouldBe` 3
            remaining `shouldBe` 2

        it "prunes all when maxToKeep is 0" $ do
            let ((pruned, remaining), _) = run $ do
                    storeN 2
                    p <-
                        runTx
                            $ pruneExcess RollbackPoints 0
                    c <-
                        runTx
                            $ countPoints RollbackPoints
                    pure (p, c)
            pruned `shouldBe` 2
            remaining `shouldBe` 0

        it "handles maxToKeep equal to count" $ do
            let (pruned, _) = run $ do
                    storeN 2
                    runTx
                        $ pruneExcess RollbackPoints 2
            pruned `shouldBe` 0

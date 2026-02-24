{-# LANGUAGE StrictData #-}

-- |
-- Module      : CSMT.Backend.Pure
-- Description : Pure in-memory backend for CSMT
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- A pure in-memory backend for CSMT, useful for testing and development.
-- This backend stores all data in memory using 'Map' and provides a simple
-- interface for running CSMT operations without external dependencies.
--
-- Not intended for production use - see "CSMT.Backend.RocksDB" for a
-- persistent storage backend.
module CSMT.Backend.Pure
    ( InMemoryDB (..)
    , Pure
    , standalonePureCols
    , runPure
    , emptyInMemoryDB
    , pureDatabase
    , inMemoryCSMTParsed
    , runPureTransaction
    )
where

import CSMT.Backend.Standalone
    ( Standalone (..)
    , StandaloneCF (..)
    , StandaloneCodecs (..)
    , StandaloneOp
    , mkStandaloneOp
    )
import CSMT.Interface
    ( Indirect (..)
    , Key
    , csmtCodecs
    )
import Control.Lens (preview)
import Control.Monad.Catch.Pure (Catch, runCatch)
import Control.Monad.Trans.State.Strict
    ( StateT (runStateT)
    , gets
    , modify'
    )
import Data.ByteString (ByteString)
import Data.Foldable (forM_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
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
    , Transaction
    , fromPairList
    , runTransactionUnguarded
    )

data Cursor = Cursor
    { position :: Maybe ByteString
    , snapshot :: Map ByteString ByteString
    }
    deriving (Show, Eq)

isValidCursor :: Cursor -> Bool
isValidCursor Cursor{position} = case position of
    Just _ -> True
    Nothing -> False

seekCursor :: ByteString -> Cursor -> Cursor
seekCursor key c@Cursor{snapshot} =
    let (_, k') = Map.split key snapshot
    in  case Map.lookupMin k' of
            Just (k, _) -> c{position = Just k}
            Nothing -> c{position = Nothing}

entryCursor
    :: Map ByteString ByteString
    -> Cursor
    -> Maybe (ByteString, ByteString)
entryCursor _ Cursor{position = Nothing} = Nothing
entryCursor db Cursor{position = Just k} =
    case Map.lookup k db of
        Just v -> Just (k, v)
        Nothing -> Nothing

firstCursor :: Cursor -> Cursor
firstCursor c@Cursor{snapshot} =
    case Map.lookupMin snapshot of
        Just (k, _) -> c{position = Just k}
        Nothing -> c{position = Nothing}

lastCursor :: Cursor -> Cursor
lastCursor c@Cursor{snapshot} =
    case Map.lookupMax snapshot of
        Just (k, _) -> c{position = Just k}
        Nothing -> c{position = Nothing}

nextCursor :: Cursor -> Cursor
nextCursor c@Cursor{position = Nothing} = c
nextCursor c@Cursor{position = Just k, snapshot} =
    let (_, k') = Map.split k snapshot
    in  case Map.lookupMin k' of
            Just (knext, _) -> c{position = Just knext}
            Nothing -> c{position = Nothing}

prevCursor :: Cursor -> Cursor
prevCursor c@Cursor{position = Nothing} = c
prevCursor c@Cursor{position = Just k, snapshot} =
    let (kprev, _) = Map.split k snapshot
    in  case Map.lookupMax kprev of
            Just (kprev', _) -> c{position = Just kprev'}
            Nothing -> c{position = Nothing}

-- |
-- In-memory database storing CSMT nodes and key-value pairs.
--
-- Contains three maps:
-- * 'inMemoryCSMT' - The CSMT node storage
-- * 'inMemoryKV' - The key-value pair storage
-- * 'inMemoryIterators' - Active iterator cursors
data InMemoryDB = InMemoryDB
    { inMemoryCSMT :: Map ByteString ByteString
    , inMemoryKV :: Map ByteString ByteString
    , inMemoryIterators :: Map Int Cursor
    }
    deriving (Show, Eq)

-- | Parse the in-memory CSMT storage into typed Key/Indirect pairs.
inMemoryCSMTParsed
    :: StandaloneCodecs k v a
    -> InMemoryDB
    -> Map Key (Indirect a)
inMemoryCSMTParsed StandaloneCodecs{nodeCodec = pa} m =
    let Codecs{keyCodec, valueCodec} = csmtCodecs pa
    in  Map.fromList
            [ (key, aux)
            | (kbs, vbs) <- Map.toList (inMemoryCSMT m)
            , Just key <- [preview keyCodec kbs]
            , Just aux <- [preview valueCodec vbs]
            ]

-- | An empty in-memory database with no data.
emptyInMemoryDB :: InMemoryDB
emptyInMemoryDB = InMemoryDB Map.empty Map.empty Map.empty

onCSMT
    :: (Map ByteString ByteString -> Map ByteString ByteString)
    -> InMemoryDB
    -> InMemoryDB
onCSMT f m = m{inMemoryCSMT = f (inMemoryCSMT m)}

onKV
    :: (Map ByteString ByteString -> Map ByteString ByteString)
    -> InMemoryDB
    -> InMemoryDB
onKV f m = m{inMemoryKV = f (inMemoryKV m)}

-- | Pure monad for CSMT operations
type Pure = StateT InMemoryDB Catch

-- | Run a pure CSMT computation against an in-memory database and throw monadfail
-- failures as errors.
runPure
    :: InMemoryDB
    -> Pure b
    -> (b, InMemoryDB)
runPure p s = case runCatch (runStateT s p) of
    Left err -> error $ "runPure: unexpected error: " ++ show err
    Right res -> res

pureValueAt :: StandaloneCF -> ByteString -> Pure (Maybe ByteString)
pureValueAt StandaloneKV k = do
    kv <- gets inMemoryKV
    pure $ Map.lookup k kv
pureValueAt StandaloneCSMT k = do
    csmt <- gets inMemoryCSMT
    pure $ Map.lookup k csmt

pureApplyOps :: [StandaloneOp] -> Pure ()
pureApplyOps ops = forM_ ops $ \(cf, k, mv) -> case (cf, mv) of
    (StandaloneKV, Nothing) -> modify' $ onKV $ Map.delete k
    (StandaloneKV, Just v) -> modify' $ onKV $ Map.insert k v
    (StandaloneCSMT, Nothing) -> modify' $ onCSMT $ Map.delete k
    (StandaloneCSMT, Just v) -> modify' $ onCSMT $ Map.insert k v

-- | Build column definitions for the pure in-memory backend.
standalonePureCols
    :: StandaloneCodecs k v a
    -> DMap (Standalone k v a) (Column StandaloneCF)
standalonePureCols StandaloneCodecs{keyCodec = pk, valueCodec = pv, nodeCodec = pa} =
    fromPairList
        [ StandaloneKVCol
            :=> Column
                { family = StandaloneKV
                , codecs = Codecs pk pv
                }
        , StandaloneCSMTCol
            :=> Column
                { family = StandaloneCSMT
                , codecs = csmtCodecs pa
                }
        ]

pureIterator :: StandaloneCF -> Pure (QueryIterator Pure)
pureIterator cf = do
    db <- gets $ case cf of
        StandaloneKV -> inMemoryKV
        StandaloneCSMT -> inMemoryCSMT
    nextId <- gets $ \m -> case Map.lookupMax (inMemoryIterators m) of
        Just (i, _) -> i + 1
        Nothing -> 0
    modify' $ \m ->
        m
            { inMemoryIterators =
                Map.insert
                    nextId
                    (Cursor{position = Nothing, snapshot = db})
                    (inMemoryIterators m)
            }
    pure
        $ QueryIterator
            { step = pureStepIterator nextId
            , isValid = pureIsValidIterator nextId
            , entry = pureEntryIterator nextId
            }

pureStepIterator :: Int -> Pos -> StateT InMemoryDB Catch ()
pureStepIterator itId pos = do
    iterators <- gets inMemoryIterators
    case pos of
        PosDestroy -> modify' $ \m ->
            m
                { inMemoryIterators =
                    Map.delete itId (inMemoryIterators m)
                }
        _ -> case Map.lookup itId iterators of
            Just cursor -> do
                let cursor' = case pos of
                        PosFirst -> firstCursor cursor
                        PosLast -> lastCursor cursor
                        PosNext -> nextCursor cursor
                        PosPrev -> prevCursor cursor
                        PosAny k -> seekCursor k cursor
                modify' $ \m ->
                    m
                        { inMemoryIterators =
                            Map.insert itId cursor' (inMemoryIterators m)
                        }
            Nothing -> error "pureStepIterator: invalid iterator id"

pureEntryIterator
    :: Int -> StateT InMemoryDB Catch (Maybe (ByteString, ByteString))
pureEntryIterator itId = do
    iterators <- gets inMemoryIterators
    case Map.lookup itId iterators of
        Just cursor -> pure $ entryCursor (snapshot cursor) cursor
        Nothing -> error "pureEntryIterator: invalid iterator id"

pureIsValidIterator :: Int -> StateT InMemoryDB Catch Bool
pureIsValidIterator itId = do
    iterators <- gets inMemoryIterators
    case Map.lookup itId iterators of
        Just cursor -> pure $ isValidCursor cursor
        Nothing -> error "pureIsValidIterator: invalid iterator id"

-- | Create a pure in-memory database instance.
pureDatabase
    :: StandaloneCodecs k v a
    -> Database Pure StandaloneCF (Standalone k v a) StandaloneOp
pureDatabase codecs =
    let db =
            Database
                { valueAt = pureValueAt
                , applyOps = pureApplyOps
                , columns = standalonePureCols codecs
                , mkOperation = mkStandaloneOp
                , newIterator = pureIterator
                , withSnapshot = \f -> f db
                }
    in  db

-- | Run a transaction in the pure in-memory backend.
runPureTransaction
    :: StandaloneCodecs k v a
    -> Transaction Pure StandaloneCF (Standalone k v a) StandaloneOp b
    -> Pure b
runPureTransaction codecs = runTransactionUnguarded (pureDatabase codecs)

{-# LANGUAGE StrictData #-}

module MPF.Backend.Pure
    ( MPFInMemoryDB (..)
    , MPFPure
    , standaloneMPFPureCols
    , runMPFPure
    , emptyMPFInMemoryDB
    , mpfPureDatabase
    , runMPFPureTransaction
    )
where

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
import MPF.Backend.Standalone
    ( MPFStandalone (..)
    , MPFStandaloneCF (..)
    , MPFStandaloneCodecs (..)
    , MPFStandaloneOp
    , mkMPFStandaloneOp
    )
import MPF.Interface (mpfCodecs)

-- | Cursor for iterating over in-memory database
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

-- | In-memory database for MPF
data MPFInMemoryDB = MPFInMemoryDB
    { mpfInMemoryMPF :: Map ByteString ByteString
    , mpfInMemoryKV :: Map ByteString ByteString
    , mpfInMemoryIterators :: Map Int Cursor
    }
    deriving (Show, Eq)

-- | Empty in-memory database
emptyMPFInMemoryDB :: MPFInMemoryDB
emptyMPFInMemoryDB = MPFInMemoryDB Map.empty Map.empty Map.empty

onMPF
    :: (Map ByteString ByteString -> Map ByteString ByteString)
    -> MPFInMemoryDB
    -> MPFInMemoryDB
onMPF f m = m{mpfInMemoryMPF = f (mpfInMemoryMPF m)}

onKV
    :: (Map ByteString ByteString -> Map ByteString ByteString)
    -> MPFInMemoryDB
    -> MPFInMemoryDB
onKV f m = m{mpfInMemoryKV = f (mpfInMemoryKV m)}

-- | Pure monad for MPF operations
type MPFPure = StateT MPFInMemoryDB Catch

-- | Run a pure MPF computation against an in-memory database
runMPFPure
    :: MPFInMemoryDB
    -> MPFPure b
    -> (b, MPFInMemoryDB)
runMPFPure p s = case runCatch (runStateT s p) of
    Left err -> error $ "runMPFPure: unexpected error: " ++ show err
    Right res -> res

pureValueAt
    :: MPFStandaloneCF -> ByteString -> MPFPure (Maybe ByteString)
pureValueAt MPFStandaloneKV k = do
    kv <- gets mpfInMemoryKV
    pure $ Map.lookup k kv
pureValueAt MPFStandaloneMPF k = do
    mpf <- gets mpfInMemoryMPF
    pure $ Map.lookup k mpf

pureApplyOps :: [MPFStandaloneOp] -> MPFPure ()
pureApplyOps ops = forM_ ops $ \(cf, k, mv) -> case (cf, mv) of
    (MPFStandaloneKV, Nothing) -> modify' $ onKV $ Map.delete k
    (MPFStandaloneKV, Just v) -> modify' $ onKV $ Map.insert k v
    (MPFStandaloneMPF, Nothing) -> modify' $ onMPF $ Map.delete k
    (MPFStandaloneMPF, Just v) -> modify' $ onMPF $ Map.insert k v

standaloneMPFPureCols
    :: MPFStandaloneCodecs k v a
    -> DMap (MPFStandalone k v a) (Column MPFStandaloneCF)
standaloneMPFPureCols
    MPFStandaloneCodecs
        { mpfKeyCodec = pk
        , mpfValueCodec = pv
        , mpfNodeCodec = pa
        } =
        fromPairList
            [ MPFStandaloneKVCol
                :=> Column
                    { family = MPFStandaloneKV
                    , codecs = Codecs pk pv
                    }
            , MPFStandaloneMPFCol
                :=> Column
                    { family = MPFStandaloneMPF
                    , codecs = mpfCodecs pa
                    }
            ]

pureIterator :: MPFStandaloneCF -> MPFPure (QueryIterator MPFPure)
pureIterator cf = do
    db <- gets $ case cf of
        MPFStandaloneKV -> mpfInMemoryKV
        MPFStandaloneMPF -> mpfInMemoryMPF
    nextId <- gets $ \m -> case Map.lookupMax (mpfInMemoryIterators m) of
        Just (i, _) -> i + 1
        Nothing -> 0
    modify' $ \m ->
        m
            { mpfInMemoryIterators =
                Map.insert
                    nextId
                    (Cursor{position = Nothing, snapshot = db})
                    (mpfInMemoryIterators m)
            }
    pure
        $ QueryIterator
            { step = pureStepIterator nextId
            , isValid = pureIsValidIterator nextId
            , entry = pureEntryIterator nextId
            }

pureStepIterator :: Int -> Pos -> StateT MPFInMemoryDB Catch ()
pureStepIterator itId pos = do
    iterators <- gets mpfInMemoryIterators
    case pos of
        PosDestroy -> modify' $ \m ->
            m
                { mpfInMemoryIterators =
                    Map.delete itId (mpfInMemoryIterators m)
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
                        { mpfInMemoryIterators =
                            Map.insert itId cursor' (mpfInMemoryIterators m)
                        }
            Nothing -> error "pureStepIterator: invalid iterator id"

pureEntryIterator
    :: Int -> StateT MPFInMemoryDB Catch (Maybe (ByteString, ByteString))
pureEntryIterator itId = do
    iterators <- gets mpfInMemoryIterators
    case Map.lookup itId iterators of
        Just cursor -> pure $ entryCursor (snapshot cursor) cursor
        Nothing -> error "pureEntryIterator: invalid iterator id"

pureIsValidIterator :: Int -> StateT MPFInMemoryDB Catch Bool
pureIsValidIterator itId = do
    iterators <- gets mpfInMemoryIterators
    case Map.lookup itId iterators of
        Just cursor -> pure $ isValidCursor cursor
        Nothing -> error "pureIsValidIterator: invalid iterator id"

mpfPureDatabase
    :: MPFStandaloneCodecs k v a
    -> Database MPFPure MPFStandaloneCF (MPFStandalone k v a) MPFStandaloneOp
mpfPureDatabase codecs =
    let db =
            Database
                { valueAt = pureValueAt
                , applyOps = pureApplyOps
                , columns = standaloneMPFPureCols codecs
                , mkOperation = mkMPFStandaloneOp
                , newIterator = pureIterator
                , withSnapshot = \f -> f db
                }
    in  db

runMPFPureTransaction
    :: MPFStandaloneCodecs k v a
    -> Transaction
        MPFPure
        MPFStandaloneCF
        (MPFStandalone k v a)
        MPFStandaloneOp
        b
    -> MPFPure b
runMPFPureTransaction codecs = runTransactionUnguarded (mpfPureDatabase codecs)

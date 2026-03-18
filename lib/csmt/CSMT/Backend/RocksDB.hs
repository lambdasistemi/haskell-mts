-- |
-- Module      : CSMT.Backend.RocksDB
-- Description : RocksDB persistent storage backend for CSMT
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- A persistent storage backend for CSMT using RocksDB. This backend stores
-- all data on disk, making it suitable for production use with large datasets.
--
-- The database uses four column families:
-- * \"kv\" - For key-value pair storage
-- * \"csmt\" - For CSMT node storage
-- * \"journal\" - For KVOnly mode journal entries
-- * \"metrics\" - For persistent counters
module CSMT.Backend.RocksDB
    ( withRocksDB
    , RocksDB
    , RunRocksDB (..)
    , unsafeWithRocksDB
    , standaloneRocksDBDatabase
    )
where

import CSMT.Backend.Standalone
    ( Standalone (..)
    , StandaloneCodecs (..)
    )
import CSMT.Interface (csmtCodecs)
import Control.Concurrent
    ( newEmptyMVar
    , putMVar
    , readMVar
    )
import Control.Concurrent.Async (async, link)
import Control.Lens (iso)
import Control.Monad ((<=<))
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Data.Serialize.Extra (intCodec)
import Database.KV.Database
    ( Database (..)
    )
import Database.KV.RocksDB (mkRocksDBDatabase)
import Database.KV.Transaction
    ( Codecs (..)
    , Column (..)
    , DMap
    , DSum (..)
    , fromPairList
    )
import Database.RocksDB
    ( BatchOp
    , ColumnFamily
    , Config (..)
    , DB (..)
    , withDBCF
    )
import UnliftIO (MonadUnliftIO)

-- | The RocksDB monad, providing access to a RocksDB database.
type RocksDB = ReaderT DB IO

-- | Build column definitions for RocksDB from column families.
standaloneRocksDBCols
    :: StandaloneCodecs k v a
    -> [ColumnFamily]
    -> DMap
        (Standalone k v a)
        (Column ColumnFamily)
standaloneRocksDBCols
    StandaloneCodecs
        { keyCodec
        , valueCodec
        , nodeCodec = pa
        }
    [kvcf, csmtcf, journalcf, metricscf] =
        fromPairList
            [ StandaloneKVCol
                :=> Column
                    { family = kvcf
                    , codecs =
                        Codecs{keyCodec, valueCodec}
                    }
            , StandaloneCSMTCol
                :=> Column
                    { family = csmtcf
                    , codecs = csmtCodecs pa
                    }
            , StandaloneJournalCol
                :=> Column
                    { family = journalcf
                    , codecs =
                        Codecs
                            (iso id id)
                            (iso id id)
                    }
            , StandaloneMetricsCol
                :=> Column
                    { family = metricscf
                    , codecs =
                        Codecs
                            (iso id id)
                            intCodec
                    }
            ]
standaloneRocksDBCols _ _ =
    error
        "standaloneRocksDBCols: expected exactly four column families"

-- | Create a RocksDB-backed database instance.
standaloneRocksDBDatabase
    :: MonadUnliftIO m
    => StandaloneCodecs k v a
    -> RocksDB
        ( Database
            m
            ColumnFamily
            (Standalone k v a)
            BatchOp
        )
standaloneRocksDBDatabase codecs = do
    db@DB{columnFamilies} <- ask
    pure
        $ mkRocksDBDatabase
            db
            (standaloneRocksDBCols codecs columnFamilies)

-- | A wrapper for running RocksDB operations in IO.
newtype RunRocksDB
    = RunRocksDB (forall a. RocksDB a -> IO a)

-- |
-- Open a RocksDB database and run an action with access to it.
-- The database is automatically closed when the action completes.
--
-- Parameters:
-- * 'path' - Directory path for the database files
-- * 'csmtMaxFiles' - Maximum open files for CSMT column family
-- * 'kvMaxFiles' - Maximum open files for KV column family
withRocksDB
    :: FilePath
    -> Int
    -> Int
    -> (RunRocksDB -> IO b)
    -> IO b
withRocksDB path csmtMaxFiles kvMaxFiles action = do
    withDBCF
        path
        def
        [ ("kv", configKV kvMaxFiles)
        , ("csmt", configCSMT csmtMaxFiles)
        , ("journal", configJournal)
        , ("metrics", configMetrics)
        ]
        $ \db -> do
            action $ RunRocksDB $ flip runReaderT db

-- |
-- Open a RocksDB database without a scoped cleanup. Returns the database
-- handle and a close action. The caller is responsible for closing the database.
--
-- This is \"unsafe\" because forgetting to call the close action will leak resources.
unsafeWithRocksDB
    :: FilePath
    -> Int
    -> Int
    -> IO (RunRocksDB, IO ())
unsafeWithRocksDB path csmtMaxFiles kvMaxFiles = do
    wait <- newEmptyMVar
    dbv <- newEmptyMVar
    done <- newEmptyMVar
    link <=< async $ do
        withDBCF
            path
            def
            [ ("kv", configKV kvMaxFiles)
            , ("csmt", configCSMT csmtMaxFiles)
            , ("journal", configJournal)
            , ("metrics", configMetrics)
            ]
            $ \db -> do
                putMVar
                    dbv
                    (RunRocksDB $ flip runReaderT db)
                readMVar wait
        putMVar done ()
    rdb <- readMVar dbv
    let close = putMVar wait ()
    pure (rdb, close >> readMVar done)

-- | Default RocksDB configuration.
def :: Config
def =
    Config
        { createIfMissing = True
        , errorIfExists = False
        , paranoidChecks = False
        , maxFiles = Nothing
        , prefixLength = Nothing
        , bloomFilter = False
        }

-- | Configuration for the CSMT column family.
configCSMT :: Int -> Config
configCSMT n =
    Config
        { createIfMissing = True
        , errorIfExists = False
        , paranoidChecks = False
        , maxFiles = Just n
        , prefixLength = Nothing
        , bloomFilter = False
        }

-- | Configuration for the KV column family.
configKV :: Int -> Config
configKV n =
    Config
        { createIfMissing = True
        , errorIfExists = False
        , paranoidChecks = False
        , maxFiles = Just n
        , prefixLength = Nothing
        , bloomFilter = False
        }

-- | Configuration for the journal column family.
configJournal :: Config
configJournal =
    Config
        { createIfMissing = True
        , errorIfExists = False
        , paranoidChecks = False
        , maxFiles = Nothing
        , prefixLength = Nothing
        , bloomFilter = False
        }

-- | Configuration for the metrics column family.
configMetrics :: Config
configMetrics =
    Config
        { createIfMissing = True
        , errorIfExists = False
        , paranoidChecks = False
        , maxFiles = Nothing
        , prefixLength = Nothing
        , bloomFilter = False
        }

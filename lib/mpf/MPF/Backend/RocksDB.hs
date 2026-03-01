module MPF.Backend.RocksDB
    ( withMPFRocksDB
    , MPFRocksDB
    , RunMPFRocksDB (..)
    , unsafeWithMPFRocksDB
    , mpfStandaloneRocksDBDatabase
    )
where

import Control.Concurrent (newEmptyMVar, putMVar, readMVar)
import Control.Concurrent.Async (async, link)
import Control.Monad ((<=<))
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Database.KV.Database (Database (..))
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
import MPF.Backend.Standalone
    ( MPFStandalone (..)
    , MPFStandaloneCodecs (..)
    )
import MPF.Interface (mpfCodecs)
import UnliftIO (MonadUnliftIO)

-- | The RocksDB monad for MPF
type MPFRocksDB = ReaderT DB IO

-- | Create column family mappings for RocksDB
mpfStandaloneRocksDBCols
    :: MPFStandaloneCodecs k v a
    -> [ColumnFamily]
    -> DMap (MPFStandalone k v a) (Column ColumnFamily)
mpfStandaloneRocksDBCols
    MPFStandaloneCodecs{mpfKeyCodec, mpfValueCodec, mpfNodeCodec}
    [kvcf, mpfcf] =
        fromPairList
            [ MPFStandaloneKVCol
                :=> Column
                    { family = kvcf
                    , codecs = Codecs{keyCodec = mpfKeyCodec, valueCodec = mpfValueCodec}
                    }
            , MPFStandaloneMPFCol
                :=> Column
                    { family = mpfcf
                    , codecs = mpfCodecs mpfNodeCodec
                    }
            ]
mpfStandaloneRocksDBCols _ _ =
    error "mpfStandaloneRocksDBCols: expected exactly two column families"

-- | Create a RocksDB database for MPF
mpfStandaloneRocksDBDatabase
    :: MonadUnliftIO m
    => MPFStandaloneCodecs k v a
    -> MPFRocksDB (Database m ColumnFamily (MPFStandalone k v a) BatchOp)
mpfStandaloneRocksDBDatabase codecs = do
    db@DB{columnFamilies} <- ask
    pure
        $ mkRocksDBDatabase db (mpfStandaloneRocksDBCols codecs columnFamilies)

-- | Runner for RocksDB operations
newtype RunMPFRocksDB = RunMPFRocksDB (forall a. MPFRocksDB a -> IO a)

-- | Open a RocksDB database for MPF with the given configuration
withMPFRocksDB
    :: FilePath
    -> Int
    -- ^ Max files for MPF column family
    -> Int
    -- ^ Max files for KV column family
    -> (RunMPFRocksDB -> IO b)
    -> IO b
withMPFRocksDB path mpfMaxFiles kvMaxFiles action = do
    withDBCF
        path
        def
        [("kv", configKV kvMaxFiles), ("mpf", configMPF mpfMaxFiles)]
        $ \db -> do
            action $ RunMPFRocksDB $ flip runReaderT db

-- | Open a RocksDB database unsafely (returns close action)
unsafeWithMPFRocksDB
    :: FilePath -> Int -> Int -> IO (RunMPFRocksDB, IO ())
unsafeWithMPFRocksDB path mpfMaxFiles kvMaxFiles = do
    wait <- newEmptyMVar
    dbv <- newEmptyMVar
    done <- newEmptyMVar
    link <=< async $ do
        withDBCF
            path
            def
            [("kv", configKV kvMaxFiles), ("mpf", configMPF mpfMaxFiles)]
            $ \db -> do
                putMVar dbv (RunMPFRocksDB $ flip runReaderT db)
                readMVar wait
        putMVar done ()
    rdb <- readMVar dbv
    let close = putMVar wait ()
    pure (rdb, close >> readMVar done)

-- | Default RocksDB configuration
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

-- | Configuration for MPF column family
configMPF :: Int -> Config
configMPF n =
    Config
        { createIfMissing = True
        , errorIfExists = False
        , paranoidChecks = False
        , maxFiles = Just n
        , prefixLength = Nothing
        , bloomFilter = False
        }

-- | Configuration for KV column family
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

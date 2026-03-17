module CSMT.Backend.RocksDBSpec
    ( spec
    )
where

import CSMT
    ( InclusionProof
    , Standalone (StandaloneCSMTCol, StandaloneKVCol)
    , StandaloneCodecs
    , buildInclusionProof
    , inserting
    , verifyInclusionProof
    )
import CSMT.Backend.RocksDB
    ( RunRocksDB (..)
    , withRocksDB
    )
import CSMT.Backend.RocksDB qualified as RocksDB
import CSMT.Backend.Standalone (StandaloneCodecs (..))
import CSMT.Deletion (deleting)
import CSMT.Hashes
    ( Hash
    , fromKVHashes
    , hashHashing
    )
import CSMT.Hashes qualified as Hashes
import CSMT.Interface (FromKV (..), root)
import CSMT.Populate (PatchOp (..), patchParallel)
import Control.Lens (view)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.Foldable (traverse_)
import Data.List (nub)
import Database.KV.Transaction
    ( RunTransaction (..)
    , newRunTransaction
    , runTransactionUnguarded
    )
import Database.KV.Transaction qualified as Transaction
import Database.RocksDB (BatchOp, ColumnFamily)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, around, describe, it, shouldBe)
import Test.QuickCheck
    ( Gen
    , Property
    , Testable (property)
    , choose
    , elements
    , forAll
    , listOf
    , listOf1
    , scale
    , (==>)
    )

type T a =
    Transaction.Transaction
        IO
        ColumnFamily
        (Standalone ByteString ByteString Hash)
        BatchOp
        a

type RunT =
    RunTransaction
        IO
        ColumnFamily
        (Standalone ByteString ByteString Hash)
        BatchOp

tempDB :: (RunT -> IO a) -> IO a
tempDB action = withSystemTempDirectory "rocksdb-test"
    $ \dir -> do
        let path = dir </> "testdb"
        withRocksDB path 1 1 $ \(RunRocksDB run) -> do
            database <- run $ RocksDB.standaloneRocksDBDatabase rocksDBCodecs
            newRunTransaction database >>= action

rocksDBCodecs :: StandaloneCodecs ByteString ByteString Hash
rocksDBCodecs =
    StandaloneCodecs
        { keyCodec = id
        , valueCodec = id
        , nodeCodec = Hashes.isoHash
        }

iM :: ByteString -> ByteString -> T ()
iM =
    inserting
        []
        fromKVHashes
        hashHashing
        StandaloneKVCol
        StandaloneCSMTCol

dM :: ByteString -> T ()
dM =
    deleting
        []
        fromKVHashes
        hashHashing
        StandaloneKVCol
        StandaloneCSMTCol

pfM :: ByteString -> T (Maybe (ByteString, InclusionProof Hash))
pfM =
    buildInclusionProof
        []
        fromKVHashes
        StandaloneKVCol
        StandaloneCSMTCol
        hashHashing

vpfM :: ByteString -> ByteString -> T Bool
vpfM k expectedV = do
    mp <- pfM k
    pure $ case mp of
        Nothing -> False
        Just (v, p) -> v == expectedV && verifyInclusionProof hashHashing p

testRandomFactsInASparseTree
    :: RunT
    -> Property
testRandomFactsInASparseTree (RunTransaction run) =
    forAll (elements [128 .. 256])
        $ \n -> forAll (genSomePaths n)
            $ \keys -> forAll (listOf $ elements [0 .. length keys - 1])
                $ \ks -> forM_ ks
                    $ \m -> do
                        let kvs =
                                zip keys
                                    $ BC.pack . show <$> [1 :: Int ..]
                            (testKey, testValue) = kvs !! m
                        run $ do
                            traverse_ (uncurry iM) kvs
                        r <- run (vpfM testKey testValue)
                        r `shouldBe` True

genSomePaths :: Int -> Gen [ByteString]
genSomePaths n = fmap nub <$> listOf1 $ do
    let go 0 = return []
        go c = do
            d <- elements [0 .. 255]
            ds <- go (c - 1)
            return (d : ds)
    B.pack <$> go (n `div` 8)

spec :: Spec
spec = around tempDB $ do
    describe "RocksDB CSMT backend" $ do
        it "can initialize and close a db"
            $ \_run -> pure @IO ()
        it "verifies a fact" $ \(RunTransaction run) -> run $ do
            iM "key1" "value1"
            r <- vpfM "key1" "value1"
            liftIO $ r `shouldBe` True
        it "rejects an incorrect fact" $ \(RunTransaction run) -> run $ do
            iM "key2" "value2"
            r <- vpfM "key2" "wrongvalue"
            liftIO $ r `shouldBe` False
        it "rejects a deleted fact" $ \(RunTransaction run) -> run $ do
            iM "key3" "value3"
            dM "key3"
            r <- vpfM "key3" "value3"
            liftIO $ r `shouldBe` False
        it "verifies random facts in a sparse tree"
            $ property . testRandomFactsInASparseTree

    describe "parallel population" $ do
        it "patchParallel on empty tree matches sequential"
            $ \_run -> property
                $ forAll (scale (* 10) $ genSomePaths 32)
                $ \keys ->
                    forAll (choose (1, 4))
                        $ \bucketBits ->
                            forAll (choose (1, 50))
                                $ \batchSize -> do
                                    let kvs = zip keys $ BC.pack . show <$> [1 :: Int ..]
                                        fkv = fromKVHashes
                                    -- Sequential insert
                                    seqRoot <- withSystemTempDirectory "seq"
                                        $ \dir -> do
                                            let path = dir </> "seqdb"
                                            withRocksDB path 1 1 $ \(RunRocksDB run) -> do
                                                database <- run $ RocksDB.standaloneRocksDBDatabase rocksDBCodecs
                                                runTransactionUnguarded database $ do
                                                    traverse_ (uncurry iM) kvs
                                                    root hashHashing StandaloneCSMTCol []
                                    -- Parallel populate
                                    popRoot <- withSystemTempDirectory "pop"
                                        $ \dir -> do
                                            let path = dir </> "popdb"
                                            withRocksDB path 1 1 $ \(RunRocksDB run) -> do
                                                database <- run $ RocksDB.standaloneRocksDBDatabase rocksDBCodecs
                                                patchParallel
                                                    bucketBits
                                                    batchSize
                                                    100
                                                    []
                                                    hashHashing
                                                    StandaloneCSMTCol
                                                    (runTransactionUnguarded database)
                                                    $ \feed ->
                                                        forM_ kvs $ \(k, v) ->
                                                            feed $ PatchInsert (view (isoK fkv) k) (fromV fkv v)
                                                runTransactionUnguarded database
                                                    $ root hashHashing StandaloneCSMTCol []
                                    popRoot `shouldBe` seqRoot

        it "patchParallel with mixed inserts and deletes matches sequential"
            $ \_run -> property
                $ forAll (scale (* 10) $ genSomePaths 32)
                $ \allKeys ->
                    forAll (choose (1, 4))
                        $ \bucketBits ->
                            forAll (choose (1, 50))
                                $ \batchSize ->
                                    length allKeys > 5 ==> do
                                        let fkv = fromKVHashes
                                            n = length allKeys
                                            -- First third: pre-populate the tree
                                            preKeys = take (n `div` 3) allKeys
                                            preKvs = zip preKeys $ BC.pack . ("pre" <>) . show <$> [1 :: Int ..]
                                            -- Rest: new inserts via patchParallel
                                            popKeys = drop (n `div` 3) allKeys
                                            popKvs = zip popKeys $ BC.pack . ("pop" <>) . show <$> [1 :: Int ..]
                                            -- Delete half of pre-inserted keys via patchParallel
                                            delKeys = take (length preKeys `div` 2) preKeys
                                            -- Build the operation stream: inserts then deletes
                                            ops =
                                                [PatchInsert (view (isoK fkv) k) (fromV fkv v) | (k, v) <- popKvs]
                                                    <> [PatchDelete (view (isoK fkv) k) | k <- delKeys]
                                        -- Sequential: pre-insert, then apply same ops
                                        seqRoot <- withSystemTempDirectory "seq"
                                            $ \dir -> do
                                                let path = dir </> "seqdb"
                                                withRocksDB path 1 1 $ \(RunRocksDB run) -> do
                                                    database <- run $ RocksDB.standaloneRocksDBDatabase rocksDBCodecs
                                                    runTransactionUnguarded database $ do
                                                        traverse_ (uncurry iM) preKvs
                                                        traverse_ (uncurry iM) popKvs
                                                        traverse_ dM delKeys
                                                    runTransactionUnguarded database
                                                        $ root hashHashing StandaloneCSMTCol []
                                        -- Parallel: pre-insert, then patchParallel with mixed ops
                                        popRoot <- withSystemTempDirectory "pop"
                                            $ \dir -> do
                                                let path = dir </> "popdb"
                                                withRocksDB path 1 1 $ \(RunRocksDB run) -> do
                                                    database <- run $ RocksDB.standaloneRocksDBDatabase rocksDBCodecs
                                                    runTransactionUnguarded database
                                                        $ traverse_ (uncurry iM) preKvs
                                                    patchParallel
                                                        bucketBits
                                                        batchSize
                                                        100
                                                        []
                                                        hashHashing
                                                        StandaloneCSMTCol
                                                        (runTransactionUnguarded database)
                                                        $ \feed -> forM_ ops feed
                                                    runTransactionUnguarded database
                                                        $ root hashHashing StandaloneCSMTCol []
                                        popRoot `shouldBe` seqRoot

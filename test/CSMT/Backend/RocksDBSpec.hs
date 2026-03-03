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
    , elements
    , forAll
    , listOf
    , listOf1
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

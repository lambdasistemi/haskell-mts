{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- | MPF Benchmark with RocksDB backend for large datasets
module Main where

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B8
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Database.KV.Transaction (query, runTransactionUnguarded)
import MPF.Backend.RocksDB
    ( MPFRocksDB
    , RunMPFRocksDB (..)
    , mpfStandaloneRocksDBDatabase
    , withMPFRocksDB
    )
import MPF.Backend.Standalone (MPFStandalone (..), MPFStandaloneCodecs (..))
import MPF.Hashes (MPFHash, mkMPFHash, mpfHashing, renderMPFHash)
import MPF.Insertion (insertingStream)
import MPF.Proof.Insertion (mkMPFInclusionProof, verifyMPFInclusionProof)
import MPF.Interface
    ( FromHexKV (..)
    , HexIndirect (..)
    , HexKey
    , byteStringToHexKey
    , hexKeyPrism
    )
import MPF.Hashes (MPFHashing(..))
import System.Directory (removeDirectoryRecursive, doesDirectoryExist)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import Control.Lens (Iso', iso)
import MPF.Hashes (parseMPFHash)

-- | Generate deterministic test data as a lazy list
generateTestData :: Int -> [(ByteString, ByteString)]
generateTestData count =
    [ (B8.pack $ "key-" <> padLeft 8 '0' (show i), B8.pack $ "value-" <> show i)
    | i <- [0 .. count - 1]
    ]
  where
    padLeft n c s = replicate (n - length s) c <> s

-- | Codecs for RocksDB
mpfHashCodecs :: MPFStandaloneCodecs HexKey MPFHash MPFHash
mpfHashCodecs =
    MPFStandaloneCodecs
        { mpfKeyCodec = hexKeyPrism
        , mpfValueCodec = isoMPFHash
        , mpfNodeCodec = isoMPFHash
        }

isoMPFHash :: Iso' ByteString MPFHash
isoMPFHash = iso parseMPFHashUnsafe renderMPFHash
  where
    parseMPFHashUnsafe bs = case parseMPFHash bs of
        Just h -> h
        Nothing -> mkMPFHash bs

fromHexKVIdentity :: FromHexKV HexKey MPFHash MPFHash
fromHexKVIdentity = FromHexKV{fromHexK = id, fromHexV = id}

-- | Insert using streaming with RocksDB
insertAllRocksDB :: Int -> [(ByteString, ByteString)] -> FilePath -> IO (Maybe ByteString)
insertAllRocksDB chunkSize testData dbPath = do
    withMPFRocksDB dbPath 1000 1000 $ \(RunMPFRocksDB run) -> run $ do
        db <- mpfStandaloneRocksDBDatabase mpfHashCodecs

        -- Process in chunks to show progress and allow GC
        let chunks = chunksOf chunkSize testData
            totalChunks = length chunks

        forM_ (zip [1..] chunks) $ \(n, chunk) -> do
            let kvs = [(byteStringToHexKey k, mkMPFHash v) | (k, v) <- chunk]
            runTransactionUnguarded db $
                insertingStream fromHexKVIdentity mpfHashing MPFStandaloneKVCol MPFStandaloneMPFCol kvs
            liftIO $ do
                printf "  Chunk %d/%d (%d items)\n" (n :: Int) totalChunks (length chunk)
                hFlush stdout

        -- Get root hash
        runTransactionUnguarded db $ do
            mi <- query MPFStandaloneMPFCol []
            pure $ case mi of
                Nothing -> Nothing
                Just i -> Just $ renderMPFHash $
                    if hexIsLeaf i
                        then leafHash mpfHashing (hexJump i) (hexValue i)
                        else hexValue i

-- | Split a list into chunks
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
    let (chunk, rest) = splitAt n xs
    in  chunk : chunksOf n rest

main :: IO ()
main = do
    args <- getArgs
    let (chunkSize, counts) = parseArgs args

    putStrLn "MPF Benchmark - RocksDB Backend"
    putStrLn "================================"
    printf "Chunk size: %d\n" chunkSize

    forM_ counts $ \count -> do
        putStrLn $ "\n=== Benchmark (n=" ++ show count ++ ") ===\n"

        -- Clean up any existing DB
        let dbPath = "/tmp/mpf-bench-rocksdb"
        exists <- doesDirectoryExist dbPath
        when exists $ removeDirectoryRecursive dbPath

        -- Generate test data
        putStr $ "Generating " ++ show count ++ " test items... "
        hFlush stdout
        start <- getCurrentTime
        let testData = generateTestData count
        _ <- pure $! length testData
        end <- getCurrentTime
        printf "%.2fs\n" (realToFrac (diffUTCTime end start) :: Double)

        -- Insert
        putStrLn $ "Inserting " ++ show count ++ " items..."
        startInsert <- getCurrentTime
        mRootHash <- insertAllRocksDB chunkSize testData dbPath
        endInsert <- getCurrentTime
        let insertTime = realToFrac (diffUTCTime endInsert startInsert) :: Double

        case mRootHash of
            Just h -> printf "Root hash: %s\n" (B8.unpack h)
            Nothing -> putStrLn "Root hash: (empty)"

        -- Verify some random keys
        putStrLn "\nVerifying 100 random keys..."
        let sampleKeys = [testData !! i | i <- [0, count `div` 100 .. min (count - 1) (99 * count `div` 100)]]
        verified <- verifyKeys dbPath (take 100 sampleKeys)
        printf "Verified: %d/100\n" verified

        -- Summary
        putStrLn "\n--- Summary ---"
        printf "Insert time: %.2fs\n" insertTime
        printf "Insert rate: %.0f ops/sec\n" (fromIntegral count / insertTime)

        -- Cleanup
        removeDirectoryRecursive dbPath

-- | Verify keys exist and have correct proofs
verifyKeys :: FilePath -> [(ByteString, ByteString)] -> IO Int
verifyKeys dbPath kvs = do
    withMPFRocksDB dbPath 1000 1000 $ \(RunMPFRocksDB run) -> run $ do
        db <- mpfStandaloneRocksDBDatabase mpfHashCodecs
        results <- mapM (verifyOne db) kvs
        pure $ length (filter id results)
  where
    verifyOne db (k, v) = do
        let key = byteStringToHexKey k
            value = mkMPFHash v
        runTransactionUnguarded db $ do
            mProof <- mkMPFInclusionProof fromHexKVIdentity mpfHashing MPFStandaloneMPFCol key
            case mProof of
                Nothing -> pure False
                Just proof -> verifyMPFInclusionProof fromHexKVIdentity MPFStandaloneMPFCol mpfHashing value proof

parseArgs :: [String] -> (Int, [Int])
parseArgs args =
    let chunkSize = case [read (drop 8 x) | x <- args, take 8 x == "--chunk="] of
            (n:_) -> n
            [] -> 100000  -- Default chunk size
        nums = [read x | x <- args, all (`elem` ['0'..'9']) x, not (null x)]
        counts = if null nums then [100000] else nums
    in (chunkSize, counts)

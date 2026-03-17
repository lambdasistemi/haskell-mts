{-# LANGUAGE BangPatterns #-}

-- | Benchmark: patchParallel vs sequential insertion
--
-- Compares wall-clock time for building a CSMT from N entries
-- using sequential insertion vs parallel population with
-- varying bucket bits.
module Main where

import CSMT.Backend.RocksDB
    ( RunRocksDB (..)
    , standaloneRocksDBDatabase
    , withRocksDB
    )
import CSMT.Backend.Standalone
    ( Standalone (StandaloneCSMTCol, StandaloneKVCol)
    , StandaloneCodecs (..)
    )
import CSMT.Hashes
    ( Hash
    , fromKVHashes
    , hashHashing
    , insert
    , isoHash
    , mkHash
    , renderHash
    )
import CSMT.Insertion
    ( expandToBucketDepth
    , mergeSubtreeRoots
    )
import CSMT.Interface (FromKV (..), root)
import CSMT.Populate (PatchOp (..), patchParallel)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Lens (view)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.String (IsString (..))
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Database.KV.Transaction (runTransactionUnguarded)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import System.IO.Temp (withSystemTempDirectory)
import Text.Printf (printf)

codecs :: StandaloneCodecs ByteString ByteString Hash
codecs =
    StandaloneCodecs
        { keyCodec = id
        , valueCodec = id
        , nodeCodec = isoHash
        }

v :: ByteString
v = "value"

mkKey :: Int -> ByteString
mkKey = renderHash . mkHash . fromString . show

fkv :: FromKV ByteString ByteString Hash
fkv = fromKVHashes

-- | Generate test data: list of (key, value) pairs
generateData :: Int -> [(ByteString, ByteString)]
generateData n = [(mkKey i, v) | i <- [1 .. n]]

-- | Benchmark sequential insertion
benchSequential :: FilePath -> [(ByteString, ByteString)] -> IO Double
benchSequential tmpDir kvs = do
    let path = tmpDir </> "seq"
    withRocksDB path 256 256 $ \(RunRocksDB run) -> do
        database <- run $ standaloneRocksDBDatabase codecs
        start <- getCurrentTime
        runTransactionUnguarded database
            $ forM_ kvs
            $ uncurry (insert fkv StandaloneKVCol StandaloneCSMTCol)
        end <- getCurrentTime
        pure $ realToFrac (diffUTCTime end start)

-- | Benchmark parallel population
benchPopulate
    :: FilePath
    -> Int
    -> Int
    -> [(ByteString, ByteString)]
    -> IO Double
benchPopulate tmpDir bucketBits batchSize kvs = do
    let path = tmpDir </> "pop-" <> show bucketBits
    withRocksDB path 256 256 $ \(RunRocksDB run) -> do
        database <- run $ standaloneRocksDBDatabase codecs
        let runTx = runTransactionUnguarded database
            ops =
                [ (k, PatchInsert (view (isoK fkv) k) (fromV fkv val))
                | (k, val) <- kvs
                ]
            chunks = chunksOf batchSize ops
        start <- getCurrentTime
        runTx
            $ expandToBucketDepth [] bucketBits StandaloneCSMTCol
        forM_ chunks $ \chunk -> do
            let txns =
                    patchParallel
                        bucketBits
                        []
                        hashHashing
                        StandaloneCSMTCol
                        StandaloneKVCol
                        chunk
            mapConcurrently_ runTx txns
        runTx
            $ mergeSubtreeRoots [] hashHashing StandaloneCSMTCol bucketBits
        end <- getCurrentTime
        pure $ realToFrac (diffUTCTime end start)

-- | Split a list into chunks of at most @n@ elements.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
    let (chunk, rest) = splitAt n xs
    in  chunk : chunksOf n rest

-- | Run one benchmark round
runBench :: Int -> Int -> IO ()
runBench n batchSize = do
    putStrLn
        $ "\n=== N="
            <> show n
            <> ", batchSize="
            <> show batchSize
            <> " ==="
    let kvs = generateData n
    -- Force data generation
    let !_ = length kvs

    withSystemTempDirectory "csmt-populate-bench"
        $ \tmpDir -> do
            -- Sequential
            putStr "  sequential:     "
            hFlush stdout
            seqTime <- benchSequential tmpDir kvs
            printf
                "%.3fs  (%.0f inserts/sec)\n"
                seqTime
                (fromIntegral n / seqTime :: Double)

            -- Populate with varying bucket bits
            forM_ [1, 2, 3, 4, 5, 6, 7, 8] $ \bits -> do
                putStr $ "  populate " <> show bits <> " bits: "
                hFlush stdout
                popTime <- benchPopulate tmpDir bits batchSize kvs
                let speedup = seqTime / popTime
                printf
                    "%.3fs  (%.0f inserts/sec, %.1fx)\n"
                    popTime
                    (fromIntegral n / popTime :: Double)
                    speedup

main :: IO ()
main = do
    args <- getArgs
    let (sizes, batchSize) = parseArgs args
    putStrLn "CSMT Populate Benchmark"
    putStrLn "======================="
    putStrLn $ "Batch size: " <> show batchSize
    forM_ sizes $ \n -> runBench n batchSize

parseArgs :: [String] -> ([Int], Int)
parseArgs args =
    let nums =
            [ read x
            | x <- args
            , all (`elem` ['0' .. '9']) x
            , not (null x)
            ]
        batch = case dropWhile (/= "--batch") args of
            (_ : b : _) -> read b
            _ -> 1000
    in  case nums of
            [] -> ([1000, 5000, 10000, 50000], batch)
            ns -> (ns, batch)

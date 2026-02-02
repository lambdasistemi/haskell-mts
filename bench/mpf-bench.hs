{-# LANGUAGE OverloadedStrings #-}

-- | MPF Benchmark - Haskell Implementation
-- Compares against the TypeScript/Aiken implementation
module Main where

import Control.Monad (forM)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B8
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import MPF.Hashes (mkMPFHash, renderMPFHash)
import MPF.Interface (byteStringToHexKey)
import MPF.Test.Lib
    ( MPFInMemoryDB
    , MPFPure
    , emptyMPFInMemoryDB
    , getRootHashM
    , insertBatchMPFM
    , proofMPFM
    , runMPFPure
    , verifyMPFM
    )
import System.Environment (getArgs)
import Text.Printf (printf)

-- | Generate deterministic test data
generateTestData :: Int -> [(ByteString, ByteString)]
generateTestData count =
    [ (B8.pack $ "key-" <> padLeft 6 '0' (show i), B8.pack $ "value-" <> show i)
    | i <- [0 .. count - 1]
    ]
  where
    padLeft n c s = replicate (n - length s) c <> s

-- | Benchmark helper
benchmark :: String -> IO a -> IO (a, Double)
benchmark name action = do
    start <- getCurrentTime
    result <- action
    end <- getCurrentTime
    let durationMs = realToFrac (diffUTCTime end start) * 1000 :: Double
    printf "%s: %.2fms\n" name durationMs
    pure (result, durationMs)

-- | Run all insertions and return the final database state
-- Uses batch insert for O(n log n) performance
insertAll :: [(ByteString, ByteString)] -> (Maybe ByteString, MPFInMemoryDB)
insertAll testData =
    let action :: MPFPure (Maybe ByteString)
        action = do
            let kvs = [(byteStringToHexKey k, mkMPFHash v) | (k, v) <- testData]
            insertBatchMPFM kvs
            mRoot <- getRootHashM
            pure $ renderMPFHash <$> mRoot
    in  runMPFPure emptyMPFInMemoryDB action

-- | Generate proofs for all keys using an existing database
generateProofs :: [(ByteString, ByteString)] -> MPFInMemoryDB -> Int
generateProofs testData db =
    let action :: MPFPure Int
        action = do
            results <- forM testData $ \(k, _) -> do
                let key = byteStringToHexKey k
                mProof <- proofMPFM key
                pure $ maybe 0 (const 1) mProof
            pure $ sum results
    in  fst $ runMPFPure db action

-- | Verify all key-value pairs using an existing database
verifyAll :: [(ByteString, ByteString)] -> MPFInMemoryDB -> Int
verifyAll testData db =
    let action :: MPFPure Int
        action = do
            results <- forM testData $ \(k, v) -> do
                let key = byteStringToHexKey k
                    value = mkMPFHash v
                verified <- verifyMPFM key value
                pure $ if verified then 1 else 0
            pure $ sum results
    in  fst $ runMPFPure db action

-- | Run benchmark for a given count
runBenchmark :: Int -> IO ()
runBenchmark count = do
    printf "\n=== Haskell MPF Benchmark (n=%d) ===\n\n" count

    let testData = generateTestData count

    -- Benchmark: Insert all items
    ((mRootHash, db), insertTime) <- benchmark (printf "Insert %d items" count) $ do
        pure $! insertAll testData

    case mRootHash of
        Just h -> printf "Root hash: %s\n" (B8.unpack h)
        Nothing -> putStrLn "Root hash: (empty)"

    -- Benchmark: Generate proofs for all items
    (proofsGenerated, proofGenTime) <- benchmark (printf "Generate %d proofs" count) $ do
        pure $! generateProofs testData db

    printf "Proofs generated: %d/%d\n" proofsGenerated count

    -- Benchmark: Verify all proofs
    (verified, verifyTime) <- benchmark (printf "Verify %d proofs" count) $ do
        pure $! verifyAll testData db

    printf "Verified: %d/%d\n" verified count

    -- Summary
    putStrLn "\n--- Summary ---"
    printf "Insert rate: %.0f ops/sec\n" (fromIntegral count / insertTime * 1000 :: Double)
    printf "Proof gen rate: %.0f ops/sec\n" (fromIntegral count / proofGenTime * 1000 :: Double)
    printf "Verify rate: %.0f ops/sec\n" (fromIntegral count / verifyTime * 1000 :: Double)

main :: IO ()
main = do
    args <- getArgs
    let counts = case args of
            [] -> [100, 1000]
            xs -> map read xs

    putStrLn "MPF Benchmark - Haskell Implementation"
    putStrLn "======================================"

    mapM_ runBenchmark counts

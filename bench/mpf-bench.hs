{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- | MPF Benchmark - Haskell Implementation
module Main where

import Control.Monad (forM, when)
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
    , insertMPFM
    , insertStreamMPFM
    , proofMPFM
    , runMPFPure
    , verifyMPFM
    )
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

-- | Generate deterministic test data
generateTestData :: Int -> [(ByteString, ByteString)]
generateTestData count =
    [ (B8.pack $ "key-" <> padLeft 8 '0' (show i), B8.pack $ "value-" <> show i)
    | i <- [0 .. count - 1]
    ]
  where
    padLeft n c s = replicate (n - length s) c <> s

-- | Benchmark helper
benchmark :: String -> IO a -> IO (a, Double)
benchmark name action = do
    putStr $ name ++ "... "
    hFlush stdout
    start <- getCurrentTime
    result <- action
    end <- getCurrentTime
    let durationMs = realToFrac (diffUTCTime end start) * 1000 :: Double
    printf "%.2fms\n" durationMs
    pure (result, durationMs)

-- | Run all insertions using batch insert (divide-and-conquer)
insertAllBatch :: [(ByteString, ByteString)] -> (Maybe ByteString, MPFInMemoryDB)
insertAllBatch testData =
    let action :: MPFPure (Maybe ByteString)
        action = do
            let kvs = [(byteStringToHexKey k, mkMPFHash v) | (k, v) <- testData]
            insertBatchMPFM kvs
            mRoot <- getRootHashM
            pure $ renderMPFHash <$> mRoot
    in  runMPFPure emptyMPFInMemoryDB action

-- | Run all insertions using streaming insert (groups by first hex digit)
insertAllStream :: [(ByteString, ByteString)] -> (Maybe ByteString, MPFInMemoryDB)
insertAllStream testData =
    let action :: MPFPure (Maybe ByteString)
        action = do
            let kvs = [(byteStringToHexKey k, mkMPFHash v) | (k, v) <- testData]
            insertStreamMPFM kvs
            mRoot <- getRootHashM
            pure $ renderMPFHash <$> mRoot
    in  runMPFPure emptyMPFInMemoryDB action

-- | Run insertions using chunked insert with progress
insertAllChunked :: Int -> [(ByteString, ByteString)] -> IO (Maybe ByteString, MPFInMemoryDB)
insertAllChunked chunkSize testData = do
    let totalChunks = (length testData + chunkSize - 1) `div` chunkSize
    putStrLn $ "Inserting " ++ show (length testData) ++ " items in " ++ show totalChunks ++ " chunks..."

    -- Process chunks with progress
    let go :: MPFInMemoryDB -> [[(ByteString, ByteString)]] -> Int -> IO (MPFInMemoryDB, Int)
        go db [] !n = pure (db, n)
        go db (chunk:rest) !n = do
            let (_, db') = runMPFPure db $ do
                    let kvs = [(byteStringToHexKey k, mkMPFHash v) | (k, v) <- chunk]
                    mapM_ (uncurry insertMPFM) kvs
            putStrLn $ "  Chunk " ++ show (n+1) ++ "/" ++ show totalChunks ++ " (" ++ show (length chunk) ++ " items)"
            hFlush stdout
            go db' rest (n+1)

    let chunks = chunksOf chunkSize testData
    (finalDb, _) <- go emptyMPFInMemoryDB chunks 0

    -- Get root hash
    let (mRoot, _) = runMPFPure finalDb $ do
            mh <- getRootHashM
            pure $ renderMPFHash <$> mh

    pure (mRoot, finalDb)

-- | Split a list into chunks
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
    let (chunk, rest) = splitAt n xs
    in  chunk : chunksOf n rest

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

data InsertMethod = Batch | Stream | Chunked Int
    deriving (Eq)

methodName :: InsertMethod -> String
methodName Batch = "batch"
methodName Stream = "stream"
methodName (Chunked n) = "chunked-" ++ show n

-- | Run benchmark for a given count
runBenchmark :: InsertMethod -> Bool -> Int -> IO ()
runBenchmark method skipProofs count = do
    putStrLn $ "\n=== Haskell MPF Benchmark (n=" ++ show count ++ ", method=" ++ methodName method ++ ") ===\n"

    -- Generate test data
    putStr $ "Generating " ++ show count ++ " test items... "
    hFlush stdout
    start <- getCurrentTime
    let testData = generateTestData count
    _ <- pure $! length testData
    end <- getCurrentTime
    printf "%.2fs\n" (realToFrac (diffUTCTime end start) :: Double)

    -- Benchmark: Insert all items
    (mRootHash, db, insertTime) <- case method of
        Chunked chunkSize -> do
            start' <- getCurrentTime
            (mRoot, db') <- insertAllChunked chunkSize testData
            end' <- getCurrentTime
            let durationMs = realToFrac (diffUTCTime end' start') * 1000
            pure (mRoot, db', durationMs)
        Stream -> do
            ((mRoot, db'), t) <- benchmark ("Insert " ++ show count ++ " items (stream)") $ do
                pure $! insertAllStream testData
            pure (mRoot, db', t)
        Batch -> do
            ((mRoot, db'), t) <- benchmark ("Insert " ++ show count ++ " items (batch)") $ do
                pure $! insertAllBatch testData
            pure (mRoot, db', t)

    case mRootHash of
        Just h -> printf "Root hash: %s\n" (B8.unpack h)
        Nothing -> putStrLn "Root hash: (empty)"

    -- For very large datasets, skip proof generation/verification
    when (not skipProofs) $ do
        (proofsGenerated, proofGenTime) <- benchmark ("Generate " ++ show count ++ " proofs") $ do
            pure $! generateProofs testData db

        printf "Proofs generated: %d/%d\n" proofsGenerated count

        (verified, verifyTime) <- benchmark ("Verify " ++ show count ++ " proofs") $ do
            pure $! verifyAll testData db

        printf "Verified: %d/%d\n" verified count

        printf "Proof gen rate: %.0f ops/sec\n" (fromIntegral count / proofGenTime * 1000 :: Double)
        printf "Verify rate: %.0f ops/sec\n" (fromIntegral count / verifyTime * 1000 :: Double)

    -- Summary
    putStrLn "\n--- Summary ---"
    printf "Insert rate: %.0f ops/sec\n" (fromIntegral count / insertTime * 1000 :: Double)
    printf "Total insert time: %.2fs\n" (insertTime / 1000)

main :: IO ()
main = do
    args <- getArgs
    let (method, skipProofs, counts) = parseArgs args

    putStrLn "MPF Benchmark - Haskell Implementation"
    putStrLn "======================================"
    putStrLn $ "(Using " ++ methodName method ++ " insert)"
    when skipProofs $ putStrLn "(Skipping proof generation/verification)"

    mapM_ (runBenchmark method skipProofs) counts

parseArgs :: [String] -> (InsertMethod, Bool, [Int])
parseArgs args =
    let useStream = "--stream" `elem` args
        useChunked = "--chunked" `elem` args
        skipProofs = "--skip-proofs" `elem` args
        chunkSize = case [read (drop 8 x) | x <- args, take 8 x == "--chunk="] of
            (n:_) -> n
            [] -> 50000  -- Default chunk size
        method
            | useStream = Stream
            | useChunked = Chunked chunkSize
            | otherwise = Batch
        nums = [read x | x <- args, all (`elem` ['0'..'9']) x, not (null x)]
        counts = if null nums then [100, 1000] else nums
    in (method, skipProofs, counts)

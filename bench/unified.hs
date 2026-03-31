{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Unified benchmark: CSMT vs MPF
--
-- Fair head-to-head comparison on RocksDB with identical conditions:
-- same datasets, same operations, same measurement method.
--
-- Operations: sequential insert, delete, proof generation, proof CBOR size
module Main where

import CSMT.Backend.RocksDB qualified as CSMT
    ( RunRocksDB (..)
    , standaloneRocksDBDatabase
    , withRocksDB
    )
import CSMT.Backend.Standalone
    ( Standalone (StandaloneCSMTCol, StandaloneKVCol)
    , StandaloneCodecs (..)
    )
import CSMT.Hashes qualified as CSMT
    ( delete
    , fromKVHashes
    , hashHashing
    , insert
    , isoHash
    , mkHash
    , renderHash
    )
import CSMT.Hashes.CBOR qualified as CSMTCBOR (renderProof)
import CSMT.Hashes.Compact (renderCompactProof)
import CSMT.Hashes.Types (Hash)
import CSMT.Proof.Insertion qualified as CSMTProof
    ( buildInclusionProof
    )
import Control.Lens (Iso', iso)
import Control.Monad (forM_, mapM, when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Database.KV.Transaction (runTransactionUnguarded)
import MPF.Backend.RocksDB qualified as MPF
    ( RunMPFRocksDB (..)
    , mpfStandaloneRocksDBDatabase
    , withMPFRocksDB
    )
import MPF.Backend.Standalone
    ( MPFStandalone (MPFStandaloneKVCol, MPFStandaloneMPFCol)
    , MPFStandaloneCodecs (..)
    )
import MPF.Deletion qualified as MPF (deleting)
import MPF.Hashes
    ( MPFHash
    , mkMPFHash
    , mpfHashing
    , renderMPFHash
    )
import MPF.Hashes.Aiken (renderAikenProof)
import MPF.Hashes.Types (MPFHash (..))
import MPF.Insertion (inserting)
import MPF.Insertion.Direct (insertingDirect)
import MPF.Insertion.Faithful (insertingFaithful)
import MPF.Interface
    ( FromHexKV (..)
    , HexKey
    , byteStringToHexKey
    , hexKeyPrism
    )
import MPF.Proof.Insertion (MPFProof (..), mkMPFInclusionProof)
import System.Directory
    ( doesDirectoryExist
    , getFileSize
    , listDirectory
    , removeDirectoryRecursive
    )
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import System.IO.Temp (withSystemTempDirectory)
import Text.Printf (printf)

-- ---------------------------------------------------------------------------
-- Shared test data generation
-- ---------------------------------------------------------------------------

generateTestData :: Int -> [(ByteString, ByteString)]
generateTestData count =
    [ ( BS.pack
            $ map (fromIntegral . fromEnum)
            $ "key-" <> padLeft 8 '0' (show i)
      , BS.pack $ map (fromIntegral . fromEnum) $ "value-" <> show i
      )
    | i <- [0 .. count - 1]
    ]
  where
    padLeft n c s = replicate (n - length s) c <> s

-- ---------------------------------------------------------------------------
-- CSMT operations on RocksDB
-- ---------------------------------------------------------------------------

csmtCodecs :: StandaloneCodecs ByteString ByteString Hash
csmtCodecs =
    StandaloneCodecs
        { keyCodec = id
        , valueCodec = id
        , nodeCodec = CSMT.isoHash
        }

-- | Pre-hash keys for CSMT: blake2b(key) as the stored key
csmtHashData
    :: [(ByteString, ByteString)] -> [(ByteString, ByteString)]
csmtHashData = map (\(k, v) -> (CSMT.renderHash (CSMT.mkHash k), v))

runCSMTBench
    :: FilePath -> [(ByteString, ByteString)] -> IO BenchResult
runCSMTBench tmpDir testData = do
    let dbPath = tmpDir </> "csmt"
        hashed = csmtHashData testData
    cleanDir dbPath
    CSMT.withRocksDB dbPath 256 256 $ \(CSMT.RunRocksDB run) -> do
        database <- run $ CSMT.standaloneRocksDBDatabase csmtCodecs

        insertTime <- timeAction
            $ forM_ hashed
            $ \(k, v) ->
                runTransactionUnguarded database
                    $ CSMT.insert CSMT.fromKVHashes StandaloneKVCol StandaloneCSMTCol k v

        proofSizeRef <- newIORef (0 :: Int)
        compactSizeRef <- newIORef (0 :: Int)
        proofCountRef <- newIORef (0 :: Int)
        proofTime <- timeAction
            $ forM_ hashed
            $ \(k, _) -> do
                mp <-
                    runTransactionUnguarded database
                        $ CSMTProof.buildInclusionProof
                            []
                            CSMT.fromKVHashes
                            StandaloneKVCol
                            StandaloneCSMTCol
                            CSMT.hashHashing
                            k
                case mp of
                    Nothing -> pure ()
                    Just (_, proof) -> do
                        modifyIORef' proofSizeRef (+ BS.length (CSMTCBOR.renderProof proof))
                        modifyIORef' compactSizeRef (+ BS.length (renderCompactProof proof))
                        modifyIORef' proofCountRef (+ 1)

        totalProofBytes <- readIORef compactSizeRef
        proofCount <- readIORef proofCountRef
        oldProofBytes <- readIORef proofSizeRef

        dbSize <- dirSize dbPath

        let deleteHashed = take (length hashed `div` 2) hashed
        deleteTime <- timeAction
            $ forM_ deleteHashed
            $ \(k, _) ->
                runTransactionUnguarded database
                    $ CSMT.delete CSMT.fromKVHashes StandaloneKVCol StandaloneCSMTCol k

        -- Print old vs compact proof sizes
        let avgOld = if proofCount > 0 then oldProofBytes `div` proofCount else 0
            avgCompact = if proofCount > 0 then totalProofBytes `div` proofCount else 0
        putStrLn
            $ "    (CSMT old proof: "
                ++ show avgOld
                ++ " bytes, compact: "
                ++ show avgCompact
                ++ " bytes)"

        pure
            BenchResult
                { brInsertTime = insertTime
                , brProofTime = proofTime
                , brDeleteTime = deleteTime
                , brTotalProofBytes = totalProofBytes
                , brProofCount = proofCount
                , brDeleteCount = length deleteHashed
                , brDbSizeBytes = dbSize
                }

-- ---------------------------------------------------------------------------
-- MPF operations on RocksDB
-- ---------------------------------------------------------------------------

mpfHashCodecs :: MPFStandaloneCodecs HexKey MPFHash MPFHash
mpfHashCodecs =
    MPFStandaloneCodecs
        { mpfKeyCodec = hexKeyPrism
        , mpfValueCodec = isoMPFHashUnsafe
        , mpfNodeCodec = isoMPFHashUnsafe
        }

isoMPFHashUnsafe :: Iso' ByteString MPFHash
isoMPFHashUnsafe = iso parse renderMPFHash
  where
    parse bs
        | BS.length bs == 32 = MPFHash bs
        | otherwise = mkMPFHash bs

mpfFromHex :: FromHexKV HexKey MPFHash MPFHash
mpfFromHex = FromHexKV{fromHexK = id, fromHexV = id, hexTreePrefix = const []}

-- | Pre-hash test data like the JS/Aiken implementation:
-- trie path = hex nibbles of blake2b(key), value = blake2b(value)
mpfHashData :: [(ByteString, ByteString)] -> [(HexKey, MPFHash)]
mpfHashData =
    map
        ( \(k, v) -> (byteStringToHexKey (renderMPFHash (mkMPFHash k)), mkMPFHash v)
        )

runMPFBench
    :: FilePath -> [(ByteString, ByteString)] -> IO BenchResult
runMPFBench tmpDir testData = do
    let dbPath = tmpDir </> "mpf"
        hashed = mpfHashData testData
    cleanDir dbPath
    MPF.withMPFRocksDB dbPath 256 256 $ \(MPF.RunMPFRocksDB run) -> run $ do
        database <- MPF.mpfStandaloneRocksDBDatabase mpfHashCodecs

        insertTime <- liftIO
            $ timeAction
            $ forM_ hashed
            $ \(hk, hv) ->
                runTransactionUnguarded database
                    $ inserting
                        []
                        mpfFromHex
                        mpfHashing
                        MPFStandaloneKVCol
                        MPFStandaloneMPFCol
                        hk
                        hv

        -- Proof generation + CBOR size
        proofSizeRef <- liftIO $ newIORef (0 :: Int)
        proofCountRef <- liftIO $ newIORef (0 :: Int)
        proofTime <- liftIO
            $ timeAction
            $ forM_ hashed
            $ \(hk, _) -> do
                mp <-
                    runTransactionUnguarded database
                        $ mkMPFInclusionProof [] mpfFromHex mpfHashing MPFStandaloneMPFCol hk
                case mp of
                    Nothing -> pure ()
                    Just proof -> do
                        let proofBytes = renderAikenProof (mpfProofSteps proof)
                        modifyIORef' proofSizeRef (+ BS.length proofBytes)
                        modifyIORef' proofCountRef (+ 1)

        totalProofBytes <- liftIO $ readIORef proofSizeRef
        proofCount <- liftIO $ readIORef proofCountRef

        dbSize <- liftIO $ dirSize dbPath

        let deleteHashed = take (length hashed `div` 2) hashed
        deleteTime <- liftIO
            $ timeAction
            $ forM_ deleteHashed
            $ \(hk, _) ->
                runTransactionUnguarded database
                    $ MPF.deleting
                        []
                        mpfFromHex
                        mpfHashing
                        MPFStandaloneKVCol
                        MPFStandaloneMPFCol
                        hk

        pure
            BenchResult
                { brInsertTime = insertTime
                , brProofTime = proofTime
                , brDeleteTime = deleteTime
                , brTotalProofBytes = totalProofBytes
                , brProofCount = proofCount
                , brDeleteCount = length deleteHashed
                , brDbSizeBytes = dbSize
                }

runMPFDirectBench
    :: FilePath -> [(ByteString, ByteString)] -> IO BenchResult
runMPFDirectBench tmpDir testData = do
    let dbPath = tmpDir </> "mpf-direct"
        hashed = mpfHashData testData
    cleanDir dbPath
    MPF.withMPFRocksDB dbPath 256 256 $ \(MPF.RunMPFRocksDB run) -> run $ do
        database <- MPF.mpfStandaloneRocksDBDatabase mpfHashCodecs

        insertTime <- liftIO
            $ timeAction
            $ forM_ hashed
            $ \(hk, hv) ->
                runTransactionUnguarded database
                    $ insertingDirect
                        []
                        mpfFromHex
                        mpfHashing
                        MPFStandaloneKVCol
                        MPFStandaloneMPFCol
                        hk
                        hv

        dbSize <- liftIO $ dirSize dbPath

        proofSizeRef <- liftIO $ newIORef (0 :: Int)
        proofCountRef <- liftIO $ newIORef (0 :: Int)
        proofTime <- liftIO
            $ timeAction
            $ forM_ hashed
            $ \(hk, _) -> do
                mp <-
                    runTransactionUnguarded database
                        $ mkMPFInclusionProof [] mpfFromHex mpfHashing MPFStandaloneMPFCol hk
                case mp of
                    Nothing -> pure ()
                    Just proof -> do
                        let proofBytes = renderAikenProof (mpfProofSteps proof)
                        modifyIORef' proofSizeRef (+ BS.length proofBytes)
                        modifyIORef' proofCountRef (+ 1)

        totalProofBytes <- liftIO $ readIORef proofSizeRef
        proofCount <- liftIO $ readIORef proofCountRef

        let deleteHashed = take (length hashed `div` 2) hashed
        deleteTime <- liftIO
            $ timeAction
            $ forM_ deleteHashed
            $ \(hk, _) ->
                runTransactionUnguarded database
                    $ MPF.deleting
                        []
                        mpfFromHex
                        mpfHashing
                        MPFStandaloneKVCol
                        MPFStandaloneMPFCol
                        hk

        pure
            BenchResult
                { brInsertTime = insertTime
                , brProofTime = proofTime
                , brDeleteTime = deleteTime
                , brTotalProofBytes = totalProofBytes
                , brProofCount = proofCount
                , brDeleteCount = length deleteHashed
                , brDbSizeBytes = dbSize
                }

runMPFFaithfulBench
    :: FilePath -> [(ByteString, ByteString)] -> IO BenchResult
runMPFFaithfulBench tmpDir testData = do
    let dbPath = tmpDir </> "mpf-faithful"
        hashed = mpfHashData testData
    cleanDir dbPath
    MPF.withMPFRocksDB dbPath 256 256 $ \(MPF.RunMPFRocksDB run) -> run $ do
        database <- MPF.mpfStandaloneRocksDBDatabase mpfHashCodecs

        insertTime <- liftIO
            $ timeAction
            $ forM_ hashed
            $ \(hk, hv) ->
                runTransactionUnguarded database
                    $ insertingFaithful
                        []
                        mpfFromHex
                        mpfHashing
                        MPFStandaloneKVCol
                        MPFStandaloneMPFCol
                        hk
                        hv

        dbSize <- liftIO $ dirSize dbPath

        proofSizeRef <- liftIO $ newIORef (0 :: Int)
        proofCountRef <- liftIO $ newIORef (0 :: Int)
        proofTime <- liftIO
            $ timeAction
            $ forM_ hashed
            $ \(hk, _) -> do
                mp <-
                    runTransactionUnguarded database
                        $ mkMPFInclusionProof [] mpfFromHex mpfHashing MPFStandaloneMPFCol hk
                case mp of
                    Nothing -> pure ()
                    Just proof -> do
                        let proofBytes = renderAikenProof (mpfProofSteps proof)
                        modifyIORef' proofSizeRef (+ BS.length proofBytes)
                        modifyIORef' proofCountRef (+ 1)

        totalProofBytes <- liftIO $ readIORef proofSizeRef
        proofCount <- liftIO $ readIORef proofCountRef

        let deleteHashed = take (length hashed `div` 2) hashed
        deleteTime <- liftIO
            $ timeAction
            $ forM_ deleteHashed
            $ \(hk, _) ->
                runTransactionUnguarded database
                    $ MPF.deleting
                        []
                        mpfFromHex
                        mpfHashing
                        MPFStandaloneKVCol
                        MPFStandaloneMPFCol
                        hk

        pure
            BenchResult
                { brInsertTime = insertTime
                , brProofTime = proofTime
                , brDeleteTime = deleteTime
                , brTotalProofBytes = totalProofBytes
                , brProofCount = proofCount
                , brDeleteCount = length deleteHashed
                , brDbSizeBytes = dbSize
                }

-- ---------------------------------------------------------------------------
-- Result type and reporting
-- ---------------------------------------------------------------------------

data BenchResult = BenchResult
    { brInsertTime :: !Double
    , brProofTime :: !Double
    , brDeleteTime :: !Double
    , brTotalProofBytes :: !Int
    , brProofCount :: !Int
    , brDeleteCount :: !Int
    , brDbSizeBytes :: !Integer
    }

printResults :: String -> Int -> BenchResult -> IO ()
printResults label n br = do
    let insertRate = fromIntegral n / brInsertTime br
        proofRate = fromIntegral (brProofCount br) / brProofTime br
        deleteRate = fromIntegral (brDeleteCount br) / brDeleteTime br
        avgProofBytes
            | brProofCount br > 0 =
                fromIntegral (brTotalProofBytes br)
                    / fromIntegral (brProofCount br)
                    :: Double
            | otherwise = 0
        dbSizeKB = fromIntegral (brDbSizeBytes br) / 1024 :: Double
    printf
        "  %-14s | %8.0f ins/s | %8.0f proof/s | %8.0f del/s | %6.0f bytes | %8.0f KB\n"
        label
        insertRate
        proofRate
        deleteRate
        avgProofBytes
        dbSizeKB

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

timeAction :: IO a -> IO Double
timeAction action = do
    start <- getCurrentTime
    !_ <- action
    end <- getCurrentTime
    pure $ realToFrac (diffUTCTime end start)

cleanDir :: FilePath -> IO ()
cleanDir path = do
    exists <- doesDirectoryExist path
    when exists $ removeDirectoryRecursive path

dirSize :: FilePath -> IO Integer
dirSize dir = do
    exists <- doesDirectoryExist dir
    if not exists
        then pure 0
        else do
            entries <- listDirectory dir
            sum
                <$> mapM
                    ( \e -> do
                        let full = dir </> e
                        isDir <- doesDirectoryExist full
                        if isDir then dirSize full else getFileSize full
                    )
                    entries

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    let sizes = case [read x | x <- args, all (`elem` ['0' .. '9']) x, not (null x)] of
            [] -> [1000, 10000]
            ns -> ns

    putStrLn "Unified Benchmark: CSMT vs MPF on RocksDB"
    putStrLn "==========================================="
    putStrLn "Sequential insertion, proof generation, deletion"
    putStrLn ""

    forM_ sizes $ \n -> do
        let testData = generateTestData n
        !_ <- pure $ length testData

        printf "\n--- N = %d ---\n" n
        printf
            "  %-14s | %14s | %15s | %13s | %11s | %s\n"
            ("" :: String)
            ("insert" :: String)
            ("proof gen" :: String)
            ("delete" :: String)
            ("proof CBOR" :: String)
            ("DB size" :: String)
        putStrLn $ replicate 95 '-'

        withSystemTempDirectory "unified-bench" $ \tmpDir -> do
            csmtResult <- runCSMTBench tmpDir testData
            printResults "CSMT" n csmtResult

            hFlush stdout

            mpfDirectResult <- runMPFDirectBench tmpDir testData
            printResults "MPF-Direct" n mpfDirectResult
            hFlush stdout

            mpfFaithfulResult <- runMPFFaithfulBench tmpDir testData
            printResults "MPF-Faithful" n mpfFaithfulResult
            hFlush stdout

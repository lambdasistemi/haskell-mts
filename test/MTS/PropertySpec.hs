{-# LANGUAGE DataKinds #-}

module MTS.PropertySpec (spec) where

import CSMT.Backend.Pure
    ( Pure
    , emptyInMemoryDB
    , pureDatabase
    , runPure
    )
import CSMT.Backend.Standalone
    ( Standalone (..)
    , StandaloneCF
    , StandaloneCodecs (..)
    , StandaloneOp
    )
import CSMT.Hashes (Hash, fromKVHashes, hashHashing, isoHash)
import CSMT.Insertion
    ( expandToBucketDepth
    , mergeSubtreeRoots
    )
import CSMT.Interface (FromKV (..), Indirect, Key)
import CSMT.MTS
    ( CommonOps (..)
    , CsmtImpl
    , DbState (..)
    , Ops (..)
    , ReadyState (..)
    , csmtKVOnlyStore
    , csmtManagedTransition
    , csmtMerkleTreeStore
    , csmtReplayJournal
    , encodePatchSentinel
    , journalEntriesToPatchOps
    , mkFullOps
    , mkKVOnlyOps
    , openOps
    , patchSentinelKey
    , readJournalChunkT
    )
import CSMT.Populate (patchParallel)
import Control.Exception (SomeException, try)
import Control.Lens (Iso', iso)
import Control.Monad (foldM, foldM_, forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Either (isLeft)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (foldl', nub)
import Data.Map.Strict qualified as Map
import Database.KV.Cursor
    ( Cursor
    , Entry (..)
    , firstEntry
    , nextEntry
    )
import Database.KV.Database (KeyOf, ValueOf)
import Database.KV.Transaction
    ( delete
    , insert
    , iterating
    , runTransactionUnguarded
    )
import Database.KV.Transaction qualified as Transaction
import MPF.Backend.Pure
    ( MPFPure
    , emptyMPFInMemoryDB
    , mpfPureDatabase
    , runMPFPure
    )
import MPF.Backend.Standalone (MPFStandaloneCodecs (..))
import MPF.Hashes
    ( MPFHash
    , mkMPFHash
    , mpfHashing
    , parseMPFHash
    , renderMPFHash
    )
import MPF.Interface (FromHexKV (..), byteStringToHexKey)
import MPF.MTS
    ( MpfImpl
    , mpfKVOnlyStore
    , mpfManagedTransition
    , mpfMerkleTreeStore
    , mpfReplayJournal
    )
import MTS.Interface
    ( MerkleTreeStore (..)
    , Mode (..)
    , MtsKV (..)
    , MtsMetrics (..)
    , MtsTransition (..)
    , MtsTree (..)
    , mtsKV
    , mtsTree
    )
import MTS.Properties
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldReturn
    , shouldThrow
    )
import Test.QuickCheck
    ( Gen
    , arbitrary
    , chooseInt
    , forAll
    , forAllShrink
    , listOf1
    , property
    , vectorOf
    )

-- ------------------------------------------------------------------
-- CSMT codecs (shared)
-- ------------------------------------------------------------------

csmtCodecs :: StandaloneCodecs ByteString ByteString Hash
csmtCodecs =
    StandaloneCodecs
        { keyCodec = iso id id
        , valueCodec = iso id id
        , nodeCodec = isoHash
        }

-- ------------------------------------------------------------------
-- CSMT store factory
-- ------------------------------------------------------------------

mkCsmtStore :: IO (MerkleTreeStore 'Full CsmtImpl IO)
mkCsmtStore = do
    ref <- newIORef emptyInMemoryDB
    let run :: forall b. Pure b -> IO b
        run action = do
            db <- readIORef ref
            let (a, db') = runPure db action
            writeIORef ref db'
            pure a
    csmtMerkleTreeStore
        []
        run
        (pureDatabase csmtCodecs)
        fromKVHashes
        hashHashing

-- ------------------------------------------------------------------
-- CSMT replay factory
-- ------------------------------------------------------------------

mkCsmtReplayEnv
    :: IO
        ( MerkleTreeStore 'KVOnly CsmtImpl IO
        , IO ()
        , IO (MerkleTreeStore 'Full CsmtImpl IO)
        )
mkCsmtReplayEnv = do
    ref <- newIORef emptyInMemoryDB
    let run :: forall b. Pure b -> IO b
        run action = do
            s <- readIORef ref
            let (a, s') = runPure s action
            writeIORef ref s'
            pure a
        db = pureDatabase csmtCodecs
    pure
        ( csmtKVOnlyStore run db fromKVHashes
        , csmtReplayJournal
            []
            100
            run
            db
            fromKVHashes
            hashHashing
            (const $ pure ())
        , csmtMerkleTreeStore [] run db fromKVHashes hashHashing
        )

-- ------------------------------------------------------------------
-- MPF codecs (shared)
-- ------------------------------------------------------------------

mpfCodecs :: MPFStandaloneCodecs ByteString ByteString MPFHash
mpfCodecs =
    MPFStandaloneCodecs
        { mpfKeyCodec = iso id id
        , mpfValueCodec = iso id id
        , mpfNodeCodec = isoMPFHash
        }

isoMPFHash :: Iso' ByteString MPFHash
isoMPFHash = iso parseMPFHashUnsafe renderMPFHash

parseMPFHashUnsafe :: ByteString -> MPFHash
parseMPFHashUnsafe bs = case parseMPFHash bs of
    Just h -> h
    Nothing -> mkMPFHash bs

fromHexKVBS :: FromHexKV ByteString ByteString MPFHash
fromHexKVBS =
    FromHexKV
        { fromHexK = byteStringToHexKey
        , fromHexV = mkMPFHash
        , hexTreePrefix = const []
        }

-- ------------------------------------------------------------------
-- MPF store factory
-- ------------------------------------------------------------------

mkMpfStore :: IO (MerkleTreeStore 'Full MpfImpl IO)
mkMpfStore = do
    ref <- newIORef emptyMPFInMemoryDB
    let run :: forall b. MPFPure b -> IO b
        run action = do
            db <- readIORef ref
            let (a, db') = runMPFPure db action
            writeIORef ref db'
            pure a
    mpfMerkleTreeStore
        []
        run
        (mpfPureDatabase mpfCodecs)
        fromHexKVBS
        mpfHashing

-- ------------------------------------------------------------------
-- MPF replay factory
-- ------------------------------------------------------------------

mkMpfReplayEnv
    :: IO
        ( MerkleTreeStore 'KVOnly MpfImpl IO
        , IO ()
        , IO (MerkleTreeStore 'Full MpfImpl IO)
        )
mkMpfReplayEnv = do
    ref <- newIORef emptyMPFInMemoryDB
    let run :: forall b. MPFPure b -> IO b
        run action = do
            s <- readIORef ref
            let (a, s') = runMPFPure s action
            writeIORef ref s'
            pure a
        db = mpfPureDatabase mpfCodecs
    pure
        ( mpfKVOnlyStore run db fromHexKVBS
        , mpfReplayJournal
            []
            100
            run
            db
            fromHexKVBS
            mpfHashing
            (const $ pure ())
        , mpfMerkleTreeStore [] run db fromHexKVBS mpfHashing
        )

-- ------------------------------------------------------------------
-- Transition factories
-- ------------------------------------------------------------------

mkCsmtTransition :: IO (MtsTransition CsmtImpl IO)
mkCsmtTransition = do
    ref <- newIORef emptyInMemoryDB
    let run :: forall b. Pure b -> IO b
        run action = do
            s <- readIORef ref
            let (a, s') = runPure s action
            writeIORef ref s'
            pure a
    csmtManagedTransition
        []
        100
        run
        (pureDatabase csmtCodecs)
        fromKVHashes
        hashHashing

mkMpfTransition :: IO (MtsTransition MpfImpl IO)
mkMpfTransition = do
    ref <- newIORef emptyMPFInMemoryDB
    let run :: forall b. MPFPure b -> IO b
        run action = do
            s <- readIORef ref
            let (a, s') = runMPFPure s action
            writeIORef ref s'
            pure a
    mpfManagedTransition
        []
        100
        run
        (mpfPureDatabase mpfCodecs)
        fromHexKVBS
        mpfHashing

-- | Test that Full construction fails when journal is non-empty.
mkCsmtStoreWithJournal :: IO (MerkleTreeStore 'Full CsmtImpl IO)
mkCsmtStoreWithJournal = do
    ref <- newIORef emptyInMemoryDB
    let run :: forall b. Pure b -> IO b
        run action = do
            s <- readIORef ref
            let (a, s') = runPure s action
            writeIORef ref s'
            pure a
        db = pureDatabase csmtCodecs
    -- Write to journal via KVOnly
    let kvStore = csmtKVOnlyStore run db fromKVHashes
    mtsInsert (mtsKV kvStore) "key" "value"
    -- Now try to construct Full — should fail
    csmtMerkleTreeStore [] run db fromKVHashes hashHashing

mkMpfStoreWithJournal :: IO (MerkleTreeStore 'Full MpfImpl IO)
mkMpfStoreWithJournal = do
    ref <- newIORef emptyMPFInMemoryDB
    let run :: forall b. MPFPure b -> IO b
        run action = do
            s <- readIORef ref
            let (a, s') = runMPFPure s action
            writeIORef ref s'
            pure a
        db = mpfPureDatabase mpfCodecs
    let kvStore = mpfKVOnlyStore run db fromHexKVBS
    mtsInsert (mtsKV kvStore) "key" "value"
    mpfMerkleTreeStore [] run db fromHexKVBS mpfHashing

-- ------------------------------------------------------------------
-- CSMT Ops GADT factory
-- ------------------------------------------------------------------

-- | Wrapper for a polymorphic transaction runner.
newtype RunTxPure
    = RunTxPure
        ( forall a
           . Transaction.Transaction
                Pure
                StandaloneCF
                (Standalone ByteString ByteString Hash)
                StandaloneOp
                a
          -> IO a
        )

-- | Create KVOnly Ops backed by the Pure in-memory backend.
-- Returns both the ops and the transaction runner.
mkCsmtKVOnlyOps
    :: IO
        ( Ops
            'KVOnly
            Pure
            StandaloneCF
            (Standalone ByteString ByteString Hash)
            StandaloneOp
            ByteString
            ByteString
            Hash
        , RunTxPure
        )
mkCsmtKVOnlyOps = do
    ref <- newIORef emptyInMemoryDB
    let run :: forall b. Pure b -> IO b
        run action = do
            s <- readIORef ref
            let (a, s') = runPure s action
            writeIORef ref s'
            pure a
        db = pureDatabase csmtCodecs
        runTx = run . runTransactionUnguarded db
    pure
        ( mkKVOnlyOps
            []
            2
            100
            StandaloneKVCol
            StandaloneCSMTCol
            StandaloneJournalCol
            StandaloneMetricsCol
            (iso id id)
            fromKVHashes
            hashHashing
            runTx
            runTx
            (const $ pure ())
        , RunTxPure runTx
        )

-- ------------------------------------------------------------------
-- Helpers
-- ------------------------------------------------------------------

-- | Count all entries in a column via cursor iteration.
countEntries :: (Monad m) => Cursor m c Int
countEntries = do
    me <- firstEntry
    case me of
        Nothing -> pure 0
        Just _ -> go 1
  where
    go n = do
        me <- nextEntry
        case me of
            Nothing -> pure n
            Just _ -> go (n + 1)

-- | Collect all (key, value) pairs from a column.
collectAll
    :: (Monad m) => Cursor m c [(KeyOf c, ValueOf c)]
collectAll = do
    me <- firstEntry
    case me of
        Nothing -> pure []
        Just e -> go [(entryKey e, entryValue e)]
  where
    go acc = do
        me <- nextEntry
        case me of
            Nothing -> pure (reverse acc)
            Just e -> go ((entryKey e, entryValue e) : acc)

-- | Parse journal entry tag byte.
-- 0x01 = JInsert, 0x02 = JUpdate, 0x00 = JDelete
data JTag = JIns | JUpd | JDel
    deriving stock (Eq, Show)

parseTag :: ByteString -> (JTag, ByteString)
parseTag bs = case B.uncons bs of
    Just (0x01, rest) -> (JIns, rest)
    Just (0x02, rest) -> (JUpd, rest)
    Just (0x00, rest) -> (JDel, rest)
    _ -> error "invalid journal tag"

-- | Apply a KVOp and track expected KV state.
applyOp
    :: CommonOps m cf d op ByteString ByteString
    -> (forall b. Transaction.Transaction m cf d op b -> IO b)
    -> Map.Map ByteString ByteString
    -> KVOp ByteString ByteString
    -> IO (Map.Map ByteString ByteString)
applyOp common rtx kv = \case
    Insert k v -> do
        rtx $ opsInsert common k v
        pure $ Map.insert k v kv
    Overwrite k v -> do
        rtx $ opsInsert common k v
        pure $ Map.insert k v kv
    Delete k -> do
        rtx $ opsDelete common k
        pure $ Map.delete k kv

-- ------------------------------------------------------------------
-- Generators
-- ------------------------------------------------------------------

genBSPair :: Gen (ByteString, ByteString)
genBSPair = do
    k <- B.pack <$> vectorOf 8 arbitrary
    v <- B.pack <$> vectorOf 8 arbitrary
    pure (k, v)

genBSPairs :: Gen [(ByteString, ByteString)]
genBSPairs = listOf1 genBSPair

genBSTriple :: Gen (ByteString, ByteString, ByteString)
genBSTriple = do
    k <- B.pack <$> vectorOf 8 arbitrary
    v <- B.pack <$> vectorOf 8 arbitrary
    v' <- B.pack <$> vectorOf 8 arbitrary
    pure (k, v, v')

-- | A single KV operation: insert, overwrite, or delete.
data KVOp k v
    = Insert k v
    | Overwrite k v
    | Delete k
    deriving stock (Show)

-- | Shrink an ops list by removing one op at a time,
-- keeping only sequences where deletes/overwrites target
-- keys that were previously inserted.
shrinkOps
    :: [KVOp ByteString ByteString] -> [[KVOp ByteString ByteString]]
shrinkOps ops =
    [ candidate
    | i <- [0 .. length ops - 1]
    , let candidate = take i ops ++ drop (i + 1) ops
    , isValid candidate
    ]
  where
    isValid = snd . foldl' step (Map.empty, True)
    step (_, False) _ = (Map.empty, False)
    step (live, True) (Insert k v) = (Map.insert k v live, True)
    step (live, True) (Overwrite k v) =
        (Map.insert k v live, Map.member k live)
    step (live, True) (Delete k) =
        (Map.delete k live, Map.member k live)

-- | Generate a realistic sequence of KV operations, one at
-- a time, maintaining a running set of live keys. At each
-- step: 60% insert new, 20% delete existing, 20% overwrite
-- existing. Models the actual UTxO lifecycle.
genOps :: Gen [KVOp ByteString ByteString]
genOps = go [] Map.empty
  where
    go acc live = do
        n <- chooseInt (0, 9)
        if length acc > 20
            then pure $ reverse acc
            else case (n, Map.toList live) of
                (_, []) -> doInsert acc live
                (n', _)
                    | n' < 6 -> doInsert acc live
                    | n' < 8 -> doDelete acc live
                    | otherwise -> doOverwrite acc live
    doInsert acc live = do
        k <- B.pack <$> vectorOf 8 arbitrary
        v <- B.pack <$> vectorOf 8 arbitrary
        go (Insert k v : acc) (Map.insert k v live)
    doDelete acc live = do
        let keys = Map.keys live
        i <- chooseInt (0, length keys - 1)
        let k = keys !! i
        go (Delete k : acc) (Map.delete k live)
    doOverwrite acc live = do
        let keys = Map.keys live
        i <- chooseInt (0, length keys - 1)
        let k = keys !! i
        v <- B.pack <$> vectorOf 8 arbitrary
        go (Overwrite k v : acc) (Map.insert k v live)

-- ------------------------------------------------------------------
-- Spec
-- ------------------------------------------------------------------

spec :: Spec
spec = do
    describe "CSMT shared properties" $ do
        it "insert-verify"
            $ propInsertVerify mkCsmtStore genBSPair
        it "multiple insert all verify"
            $ propMultipleInsertAllVerify mkCsmtStore genBSPairs
        it "insertion order independence"
            $ propInsertionOrderIndependence
                mkCsmtStore
                mkCsmtStore
                genBSPairs
        it "delete removes key"
            $ propDeleteRemovesKey mkCsmtStore genBSPair
        it "delete preserves siblings"
            $ propDeletePreservesSiblings
                mkCsmtStore
                genBSPair
                genBSPair
                genBSPair
        it "insert-delete-all empty"
            $ propInsertDeleteAllEmpty mkCsmtStore genBSPairs
        it "empty tree no root"
            $ propEmptyTreeNoRoot mkCsmtStore
        it "single insert has root"
            $ propSingleInsertHasRoot mkCsmtStore genBSPair
        it "wrong value rejects"
            $ propWrongValueRejects mkCsmtStore genBSTriple
        it "proof anchored to root"
            $ propProofAnchoredToRoot mkCsmtStore genBSPair
        it "completeness round-trip"
            $ propCompletenessRoundTrip mkCsmtStore genBSPairs
        it "completeness empty"
            $ propCompletenessEmpty mkCsmtStore
        it "completeness after delete"
            $ propCompletenessAfterDelete mkCsmtStore genBSPairs

    describe "CSMT replay properties" $ do
        it "kvonly then replay matches full"
            $ propKVOnlyThenReplayMatchesFull
                mkCsmtReplayEnv
                mkCsmtStore
                genBSPairs
        it "kvonly then replay proofs work"
            $ propKVOnlyThenReplayProofsWork
                mkCsmtReplayEnv
                genBSPairs
        it "kvonly delete then replay"
            $ propKVOnlyDeleteThenReplay
                mkCsmtReplayEnv
                genBSPairs
        it "replay idempotent"
            $ propReplayIdempotent mkCsmtReplayEnv
        it "journal compression"
            $ propJournalCompression mkCsmtReplayEnv genBSPair

    describe "MPF shared properties" $ do
        it "insert-verify"
            $ propInsertVerify mkMpfStore genBSPair
        it "multiple insert all verify"
            $ propMultipleInsertAllVerify mkMpfStore genBSPairs
        it "insertion order independence"
            $ propInsertionOrderIndependence
                mkMpfStore
                mkMpfStore
                genBSPairs
        it "delete removes key"
            $ propDeleteRemovesKey mkMpfStore genBSPair
        it "delete preserves siblings"
            $ propDeletePreservesSiblings
                mkMpfStore
                genBSPair
                genBSPair
                genBSPair
        it "insert-delete-all empty"
            $ propInsertDeleteAllEmpty mkMpfStore genBSPairs
        it "empty tree no root"
            $ propEmptyTreeNoRoot mkMpfStore
        it "single insert has root"
            $ propSingleInsertHasRoot mkMpfStore genBSPair
        it "wrong value rejects"
            $ propWrongValueRejects mkMpfStore genBSTriple
        it "proof anchored to root"
            $ propProofAnchoredToRoot mkMpfStore genBSPair
        it "completeness round-trip"
            $ propCompletenessRoundTrip mkMpfStore genBSPairs
        it "completeness empty"
            $ propCompletenessEmpty mkMpfStore
        it "completeness after delete"
            $ propCompletenessAfterDelete mkMpfStore genBSPairs

    describe "MPF replay properties" $ do
        it "kvonly then replay matches full"
            $ propKVOnlyThenReplayMatchesFull
                mkMpfReplayEnv
                mkMpfStore
                genBSPairs
        it "kvonly then replay proofs work"
            $ propKVOnlyThenReplayProofsWork
                mkMpfReplayEnv
                genBSPairs
        it "kvonly delete then replay"
            $ propKVOnlyDeleteThenReplay
                mkMpfReplayEnv
                genBSPairs
        it "replay idempotent"
            $ propReplayIdempotent mkMpfReplayEnv
        it "journal compression"
            $ propJournalCompression mkMpfReplayEnv genBSPair

    describe "CSMT mode exclusivity" $ do
        it "Full rejects non-empty journal"
            $ mkCsmtStoreWithJournal
            `shouldThrow` (\(_ :: SomeException) -> True)
        it "KVOnly throws after transition" $ do
            t <- mkCsmtTransition
            mtsInsert (mtsKV (transitionKVStore t)) "k" "v"
            _ <- transitionToFull t
            result <-
                try @SomeException
                    $ mtsInsert
                        (mtsKV (transitionKVStore t))
                        "k2"
                        "v2"
            pure (isLeft result) `shouldReturn` True

    describe "MPF mode exclusivity" $ do
        it "Full rejects non-empty journal"
            $ mkMpfStoreWithJournal
            `shouldThrow` (\(_ :: SomeException) -> True)
        it "KVOnly throws after transition" $ do
            t <- mkMpfTransition
            mtsInsert (mtsKV (transitionKVStore t)) "k" "v"
            _ <- transitionToFull t
            result <-
                try @SomeException
                    $ mtsInsert
                        (mtsKV (transitionKVStore t))
                        "k2"
                        "v2"
            pure (isLeft result) `shouldReturn` True

    describe "CSMT Ops GADT" $ do
        it "KVOnly -> Full produces correct root hash"
            $ property
            $ forAll genBSPairs
            $ \kvs -> do
                (ops, RunTxPure rtx) <- mkCsmtKVOnlyOps
                fullStore <- mkCsmtStore
                mapM_
                    ( \(k, v) ->
                        rtx (opsInsert (kvCommon ops) k v)
                    )
                    kvs
                mapM_ (uncurry (mtsInsert (mtsKV fullStore))) kvs
                mFull <- toFull ops
                case mFull of
                    Nothing -> fail "toFull returned Nothing"
                    Just fullOps -> do
                        expected <-
                            mtsRootHash (mtsTree fullStore)
                        actual <-
                            rtx (opsRootHash fullOps)
                        actual `shouldBe` expected
        it "journal is empty after toFull" $ do
            (ops, RunTxPure rtx) <- mkCsmtKVOnlyOps
            rtx (opsInsert (kvCommon ops) "k" "v")
            mFull <- toFull ops
            case mFull of
                Nothing -> fail "toFull returned Nothing"
                Just fullOps -> do
                    mKV <- toKVOnly fullOps
                    case mKV of
                        Nothing ->
                            fail "toKVOnly returned Nothing"
                        Just _ -> pure ()
        it "Full -> KVOnly -> insert -> Full cycle"
            $ property
            $ forAll genBSPairs
            $ \kvs -> do
                (ops, RunTxPure rtx) <- mkCsmtKVOnlyOps
                fullStore <- mkCsmtStore
                let (first, second) =
                        splitAt (length kvs `div` 2) kvs
                mapM_
                    ( \(k, v) ->
                        rtx (opsInsert (kvCommon ops) k v)
                    )
                    first
                Just fullOps <- toFull ops
                Just kvOps2 <- toKVOnly fullOps
                mapM_
                    ( \(k, v) ->
                        rtx (opsInsert (kvCommon kvOps2) k v)
                    )
                    second
                Just fullOps2 <- toFull kvOps2
                mapM_ (uncurry (mtsInsert (mtsKV fullStore))) kvs
                expected <- mtsRootHash (mtsTree fullStore)
                actual <-
                    rtx (opsRootHash fullOps2)
                actual `shouldBe` expected
        it "toKVOnly fails when journal is not empty" $ do
            ref <- newIORef emptyInMemoryDB
            let run :: forall b. Pure b -> IO b
                run action = do
                    s <- readIORef ref
                    let (a, s') = runPure s action
                    writeIORef ref s'
                    pure a
                db = pureDatabase csmtCodecs
                runTx = run . runTransactionUnguarded db
            let fullOps =
                    mkFullOps
                        []
                        2
                        100
                        StandaloneKVCol
                        StandaloneCSMTCol
                        StandaloneJournalCol
                        StandaloneMetricsCol
                        (iso id id)
                        fromKVHashes
                        hashHashing
                        runTx
                        runTx
                        (const $ pure ())
            runTx (opsInsert (fullCommon fullOps) "k" "v")
            mKV <- toKVOnly fullOps
            case mKV of
                Nothing ->
                    fail "toKVOnly returned Nothing"
                Just kvOps -> do
                    runTx (opsInsert (kvCommon kvOps) "k2" "v2")
                    let fullOps2 =
                            mkFullOps
                                []
                                2
                                100
                                StandaloneKVCol
                                StandaloneCSMTCol
                                StandaloneJournalCol
                                StandaloneMetricsCol
                                (iso id id)
                                fromKVHashes
                                hashHashing
                                runTx
                                runTx
                                (const $ pure ())
                    mKV2 <- toKVOnly fullOps2
                    case mKV2 of
                        Nothing -> pure ()
                        Just _ ->
                            fail "toKVOnly should fail with non-empty journal"
        -- ======================================================
        -- QC1: Genesis invariant — journal keys = KV keys,
        -- all entries are JInsert
        -- ======================================================
        it "QC1: genesis journal count == KV count"
            $ property
            $ forAll genOps
            $ \ops -> do
                (ops0, RunTxPure rtx) <- mkCsmtKVOnlyOps
                foldM_ (applyOp (kvCommon ops0) rtx) Map.empty ops
                kvCount <-
                    rtx
                        $ iterating StandaloneKVCol countEntries
                journalCount <-
                    rtx
                        $ iterating StandaloneJournalCol countEntries
                journalCount `shouldBe` kvCount
        it "QC1: genesis journal content matches KV"
            $ property
            $ forAll genOps
            $ \ops -> do
                (ops0, RunTxPure rtx) <- mkCsmtKVOnlyOps
                foldM_ (applyOp (kvCommon ops0) rtx) Map.empty ops
                kvEntries <-
                    rtx
                        $ iterating StandaloneKVCol collectAll
                journalEntries <-
                    rtx
                        $ iterating StandaloneJournalCol collectAll
                let kvMap = Map.fromList kvEntries
                    journalMap =
                        Map.fromList
                            $ map
                                (\(k, v) -> (k, snd $ parseTag v))
                                journalEntries
                journalMap `shouldBe` kvMap
        it "QC1: genesis journal entries are all JInsert"
            $ property
            $ forAll genOps
            $ \ops -> do
                (ops0, RunTxPure rtx) <- mkCsmtKVOnlyOps
                foldM_ (applyOp (kvCommon ops0) rtx) Map.empty ops
                journalEntries <-
                    rtx
                        $ iterating StandaloneJournalCol collectAll
                let tags = map (fst . parseTag . snd) journalEntries
                all (== JIns) tags `shouldBe` True
        -- ======================================================
        -- QC2: General invariant — KV = apply(journal, CSMT)
        -- After one cycle: ops → toFull → toKVOnly → more ops
        -- ======================================================
        it "QC2: KV = apply(journal, CSMT) after cycle + ops"
            $ property
            $ forAll genOps
            $ \ops -> do
                (ops0, RunTxPure rtx) <- mkCsmtKVOnlyOps
                let (first, second) = splitAt (length ops `div` 2) ops
                -- Round 1
                foldM_ (applyOp (kvCommon ops0) rtx) Map.empty first
                Just full1 <- toFull ops0
                Just kv2 <- toKVOnly full1
                -- Round 2 ops (no replay yet)
                foldM_ (applyOp (kvCommon kv2) rtx) Map.empty second
                -- Check general invariant: for each key,
                -- KV = apply(journal, CSMT)
                kvEntries <-
                    rtx
                        $ iterating StandaloneKVCol collectAll
                journalEntries <-
                    rtx
                        $ iterating StandaloneJournalCol collectAll
                let kvMap = Map.fromList kvEntries
                    journalMap = Map.fromList journalEntries
                -- Every JIns/JUpd journal key should be in KV
                -- Every JDel journal key should NOT be in KV
                mapM_
                    ( \(k, raw) -> do
                        let (tag, v) = parseTag raw
                        case tag of
                            JIns -> Map.lookup k kvMap `shouldBe` Just v
                            JUpd -> Map.lookup k kvMap `shouldBe` Just v
                            JDel -> Map.member k kvMap `shouldBe` False
                    )
                    journalEntries
        -- ======================================================
        -- Part 2: JInsert ↔ not in CSMT, JUpd/JDel ↔ in CSMT
        -- ======================================================
        it "QC2 part2: JInsert keys not in CSMT after cycle"
            $ property
            $ forAll genOps
            $ \ops -> do
                (ops0, RunTxPure rtx) <- mkCsmtKVOnlyOps
                let (first, second) = splitAt (length ops `div` 2) ops
                -- Round 1: build CSMT
                kv1 <- foldM (applyOp (kvCommon ops0) rtx) Map.empty first
                Just full1 <- toFull ops0
                -- CSMT now has kv1
                Just kv2 <- toKVOnly full1
                -- Round 2: ops without replay
                foldM_ (applyOp (kvCommon kv2) rtx) kv1 second
                journalEntries <-
                    rtx
                        $ iterating StandaloneJournalCol collectAll
                mapM_
                    ( \(k, raw) -> do
                        let (tag, _) = parseTag raw
                        case tag of
                            JIns ->
                                -- New key: should NOT have been
                                -- in CSMT (= kv1)
                                Map.member k kv1 `shouldBe` False
                            JUpd ->
                                -- Overwrite: should be in CSMT
                                Map.member k kv1 `shouldBe` True
                            JDel ->
                                -- Delete: should be in CSMT
                                Map.member k kv1 `shouldBe` True
                    )
                    journalEntries
        -- ======================================================
        -- Targeted tests
        -- ======================================================
        it "two-cycle insert-only matches fresh"
            $ property
            $ forAll genBSPairs
            $ \kvs -> do
                (ops0, RunTxPure rtx) <- mkCsmtKVOnlyOps
                let (first, second) = splitAt (length kvs `div` 2) kvs
                mapM_ (\(k, v) -> rtx $ opsInsert (kvCommon ops0) k v) first
                Just full1 <- toFull ops0
                Just kv2 <- toKVOnly full1
                mapM_ (\(k, v) -> rtx $ opsInsert (kvCommon kv2) k v) second
                Just full2 <- toFull kv2
                -- Compare against fresh
                freshStore <- mkCsmtStore
                mapM_ (uncurry $ mtsInsert $ mtsKV freshStore) kvs
                expected <- mtsRootHash $ mtsTree freshStore
                actual <- rtx $ opsRootHash full2
                actual `shouldBe` expected
        it "two-cycle with cross-boundary delete matches fresh"
            $ property
            $ forAll genBSPairs
            $ \kvs -> do
                (ops0, RunTxPure rtx) <- mkCsmtKVOnlyOps
                let (first, second) =
                        splitAt
                            (max 1 $ length kvs `div` 2)
                            kvs
                case first of
                    ((fk, _) : _) -> do
                        mapM_
                            ( \(k, v) ->
                                rtx
                                    $ opsInsert
                                        (kvCommon ops0)
                                        k
                                        v
                            )
                            first
                        Just full1 <- toFull ops0
                        Just kv2 <- toKVOnly full1
                        mapM_
                            ( \(k, v) ->
                                rtx
                                    $ opsInsert
                                        (kvCommon kv2)
                                        k
                                        v
                            )
                            second
                        rtx
                            $ opsDelete (kvCommon kv2) fk
                        Just full2 <- toFull kv2
                        freshStore <- mkCsmtStore
                        let expected' =
                                Map.delete fk
                                    $ Map.fromList kvs
                        mapM_
                            ( uncurry
                                $ mtsInsert
                                $ mtsKV freshStore
                            )
                            $ Map.toList expected'
                        expected <-
                            mtsRootHash
                                $ mtsTree freshStore
                        actual <-
                            rtx $ opsRootHash full2
                        actual `shouldBe` expected
                    _ -> pure ()
        it "two-cycle realistic ops matches fresh"
            $ property
            $ forAll genOps
            $ \ops -> do
                (ops0, RunTxPure rtx) <- mkCsmtKVOnlyOps
                let (chunk1, chunk2) = splitAt (length ops `div` 2) ops
                kv1 <- foldM (applyOp (kvCommon ops0) rtx) Map.empty chunk1
                Just full1 <- toFull ops0
                -- Verify round 1
                fresh1 <- mkCsmtStore
                mapM_ (uncurry $ mtsInsert $ mtsKV fresh1) $ Map.toList kv1
                e1 <- mtsRootHash $ mtsTree fresh1
                a1 <- rtx $ opsRootHash full1
                a1 `shouldBe` e1
                -- Round 2
                Just kv2ops <- toKVOnly full1
                kv2 <- foldM (applyOp (kvCommon kv2ops) rtx) kv1 chunk2
                Just full2 <- toFull kv2ops
                fresh2 <- mkCsmtStore
                mapM_ (uncurry $ mtsInsert $ mtsKV fresh2) $ Map.toList kv2
                e2 <- mtsRootHash $ mtsTree fresh2
                a2 <- rtx $ opsRootHash full2
                a2 `shouldBe` e2
        it "cross-boundary overwrite matches fresh"
            $ property
            $ forAll genBSPairs
            $ \kvs -> do
                (ops0, RunTxPure rtx) <- mkCsmtKVOnlyOps
                -- Round 1: insert all
                mapM_ (\(k, v) -> rtx $ opsInsert (kvCommon ops0) k v) kvs
                Just full1 <- toFull ops0
                -- Round 2: overwrite all with reversed values
                Just kv2 <- toKVOnly full1
                mapM_
                    (\(k, v) -> rtx $ opsInsert (kvCommon kv2) k $ B.reverse v)
                    kvs
                Just full2 <- toFull kv2
                -- Fresh tree with overwritten values
                freshStore <- mkCsmtStore
                mapM_
                    (\(k, v) -> mtsInsert (mtsKV freshStore) k $ B.reverse v)
                    kvs
                expected <- mtsRootHash $ mtsTree freshStore
                actual <- rtx $ opsRootHash full2
                actual `shouldBe` expected
        it "cross-boundary overwrite + delete matches fresh"
            $ property
            $ forAll (vectorOf 4 genBSPair)
            $ \kvs -> case kvs of
                ((k1, _) : (k2, _) : (k3, _) : _) -> do
                    (ops0, RunTxPure rtx) <- mkCsmtKVOnlyOps
                    mapM_
                        ( \(k, v) ->
                            rtx
                                $ opsInsert (kvCommon ops0) k v
                        )
                        kvs
                    Just full1 <- toFull ops0
                    Just kv2 <- toKVOnly full1
                    rtx $ opsInsert (kvCommon kv2) k1 "new1"
                    rtx $ opsInsert (kvCommon kv2) k2 "new2"
                    rtx $ opsDelete (kvCommon kv2) k3
                    Just full2 <- toFull kv2
                    freshStore <- mkCsmtStore
                    let expected' =
                            Map.delete k3
                                $ Map.insert k2 "new2"
                                $ Map.insert k1 "new1"
                                $ Map.fromList kvs
                    mapM_
                        ( uncurry
                            $ mtsInsert
                            $ mtsKV freshStore
                        )
                        $ Map.toList expected'
                    expected <-
                        mtsRootHash $ mtsTree freshStore
                    actual <- rtx $ opsRootHash full2
                    actual `shouldBe` expected
                _ -> pure ()
        it "cross-boundary new inserts + delete matches fresh"
            $ property
            $ forAll (vectorOf 6 genBSPair)
            $ \kvs -> do
                (ops0, RunTxPure rtx) <- mkCsmtKVOnlyOps
                let (first, second) = splitAt 3 kvs
                case first of
                    ((fk0, _) : (fk1, _) : _) -> do
                        -- Round 1
                        mapM_
                            ( \(k, v) ->
                                rtx
                                    $ opsInsert (kvCommon ops0) k v
                            )
                            first
                        Just full1 <- toFull ops0
                        -- Round 2: insert new + delete old
                        Just kv2 <- toKVOnly full1
                        mapM_
                            ( \(k, v) ->
                                rtx
                                    $ opsInsert (kvCommon kv2) k v
                            )
                            second
                        rtx $ opsDelete (kvCommon kv2) fk0
                        rtx
                            $ opsInsert
                                (kvCommon kv2)
                                fk1
                                "overwritten"
                        Just full2 <- toFull kv2
                        -- Fresh
                        freshStore <- mkCsmtStore
                        let expected' =
                                Map.insert fk1 "overwritten"
                                    $ Map.delete fk0
                                    $ Map.fromList kvs
                        mapM_
                            ( uncurry
                                $ mtsInsert
                                $ mtsKV freshStore
                            )
                            $ Map.toList expected'
                        expected <-
                            mtsRootHash $ mtsTree freshStore
                        actual <- rtx $ opsRootHash full2
                        actual `shouldBe` expected
                    _ -> pure ()
        it "single-round realistic ops produce correct hash"
            $ property
            $ forAll genOps
            $ \ops -> do
                (ops0, RunTxPure rtx) <- mkCsmtKVOnlyOps
                freshStore <- mkCsmtStore
                kv <- foldM (applyOp (kvCommon ops0) rtx) Map.empty ops
                Just full <- toFull ops0
                mapM_
                    (uncurry $ mtsInsert $ mtsKV freshStore)
                    $ Map.toList kv
                expected <- mtsRootHash $ mtsTree freshStore
                actual <- rtx $ opsRootHash full
                actual `shouldBe` expected
        it "overwrite across replay produces correct hash" $ do
            (ops0, RunTxPure rtx) <- mkCsmtKVOnlyOps
            freshStore <- mkCsmtStore
            -- Round 1: insert K with V1
            rtx $ opsInsert (kvCommon ops0) "key" "val1"
            Just full1 <- toFull ops0
            -- Round 2: overwrite K with V2
            Just kv2 <- toKVOnly full1
            rtx $ opsInsert (kvCommon kv2) "key" "val2"
            Just full2 <- toFull kv2
            -- Fresh tree with just K:V2
            mtsInsert (mtsKV freshStore) "key" "val2"
            expected <- mtsRootHash $ mtsTree freshStore
            actual <- rtx $ opsRootHash full2
            actual `shouldBe` expected
        it "delete all after replay cycle empties CSMT" $ do
            (ops0, RunTxPure rtx) <- mkCsmtKVOnlyOps
            rtx $ opsInsert (kvCommon ops0) "aaa" "111"
            rtx $ opsInsert (kvCommon ops0) "bbb" "222"
            Just full1 <- toFull ops0
            _ <- rtx $ opsRootHash full1
            Just kv2 <- toKVOnly full1
            rtx $ opsDelete (kvCommon kv2) "aaa"
            rtx $ opsDelete (kvCommon kv2) "bbb"
            Just full2 <- toFull kv2
            r2 <- rtx $ opsRootHash full2
            r2 `shouldBe` Nothing
        it "journal empty and CSMT == KV after each replay cycle"
            $ property
            $ forAll genOps
            $ \ops -> do
                (ops0, RunTxPure rtx) <- mkCsmtKVOnlyOps
                let applyChunk kvOps =
                        foldM (applyOp (kvCommon kvOps) rtx)
                    assertAfterReplay fullOps expectedKV = do
                        jn <-
                            rtx
                                $ iterating
                                    StandaloneJournalCol
                                    countEntries
                        jn `shouldBe` 0
                        freshStore <- mkCsmtStore
                        mapM_
                            (uncurry $ mtsInsert $ mtsKV freshStore)
                            $ Map.toList expectedKV
                        expected <- mtsRootHash $ mtsTree freshStore
                        actual <- rtx $ opsRootHash fullOps
                        actual `shouldBe` expected
                    -- Split ops into 3 chunks at arbitrary points
                    (chunk1, rest1) = splitAt (length ops `div` 3) ops
                    (chunk2, chunk3) = splitAt (length rest1 `div` 2) rest1
                -- Round 1
                kv1 <- applyChunk ops0 Map.empty chunk1
                Just full1 <- toFull ops0
                assertAfterReplay full1 kv1
                -- Round 2
                Just kvOps2 <- toKVOnly full1
                kv2 <- applyChunk kvOps2 kv1 chunk2
                Just full3 <- toFull kvOps2
                assertAfterReplay full3 kv2
                -- Round 3
                Just kvOps4 <- toKVOnly full3
                kv3 <- applyChunk kvOps4 kv2 chunk3
                Just full5 <- toFull kvOps4
                assertAfterReplay full5 kv3

    describe "CSMT crash recovery" $ do
        it "recovery after simulated crash matches clean replay"
            $ property
            $ forAll genBSPairs
            $ \kvs -> do
                -- Step 1: reference — clean KVOnly → Full
                refStore <- mkCsmtStore
                mapM_
                    (uncurry $ mtsInsert $ mtsKV refStore)
                    kvs
                expected <-
                    mtsRootHash $ mtsTree refStore

                -- Step 2: simulate crash mid-toFull
                ref <- newIORef emptyInMemoryDB
                let run :: forall b. Pure b -> IO b
                    run action = do
                        s <- readIORef ref
                        let (a, s') = runPure s action
                        writeIORef ref s'
                        pure a
                    db = pureDatabase csmtCodecs
                    rtx = run . runTransactionUnguarded db
                    bucketBits = 2

                -- Insert via KVOnly
                let kvOps =
                        mkKVOnlyOps
                            []
                            bucketBits
                            100
                            StandaloneKVCol
                            StandaloneCSMTCol
                            StandaloneJournalCol
                            StandaloneMetricsCol
                            (iso id id)
                            fromKVHashes
                            hashHashing
                            rtx
                            rtx
                            (const $ pure ())
                mapM_
                    ( \(k, v) ->
                        rtx
                            $ opsInsert (kvCommon kvOps) k v
                    )
                    kvs

                -- Write sentinel + expand atomically
                rtx $ do
                    insert
                        StandaloneJournalCol
                        (patchSentinelKey :: ByteString)
                        (encodePatchSentinel bucketBits [])
                    expandToBucketDepth
                        []
                        bucketBits
                        StandaloneCSMTCol
                -- Run a subset of bucket transactions
                entries <-
                    rtx
                        $ readJournalChunkT
                            StandaloneJournalCol
                            10000
                let ops =
                        journalEntriesToPatchOps
                            (iso id id)
                            fromKVHashes
                            entries
                    bucketTxns =
                        patchParallel
                            bucketBits
                            []
                            hashHashing
                            StandaloneCSMTCol
                            StandaloneJournalCol
                            ops
                -- Only run first half of bucket
                -- transactions (crash simulation)
                let halfN =
                        max 1
                            $ length bucketTxns `div` 2
                forM_ (take halfN bucketTxns) $ \(_, txn) ->
                    rtx txn
                -- DO NOT run mergeSubtreeRoots
                -- DO NOT delete sentinel
                -- → tree top is broken, sentinel present

                -- Step 3: open → must be NeedsRecovery
                state0 <-
                    openOps
                        []
                        bucketBits
                        100
                        StandaloneKVCol
                        StandaloneCSMTCol
                        StandaloneJournalCol
                        StandaloneMetricsCol
                        (iso id id)
                        fromKVHashes
                        hashHashing
                        rtx
                        rtx
                        (const $ pure ())
                case state0 of
                    NeedsRecovery recover -> do
                        Ready (ChooseKVOnly kvOps2) <-
                            recover
                        Just fullOps <- toFull kvOps2
                        actual <-
                            rtx $ opsRootHash fullOps
                        actual `shouldBe` expected
                    Ready _ ->
                        fail "expected NeedsRecovery"

    describe "CSMT metrics" $ do
        it "kvCount matches actual KV entries after ops"
            $ property
            $ forAll genOps
            $ \ops -> do
                store <- mkCsmtStore
                let kv = mtsKV store
                finalKV <-
                    foldM
                        ( \m -> \case
                            Insert k v -> do
                                mtsInsert kv k v
                                pure $ Map.insert k v m
                            Overwrite k v -> do
                                mtsInsert kv k v
                                pure $ Map.insert k v m
                            Delete k -> do
                                mtsDelete kv k
                                pure $ Map.delete k m
                        )
                        Map.empty
                        ops
                MtsMetrics{metricsKVCount} <-
                    mtsMetrics $ mtsKV store
                metricsKVCount
                    `shouldBe` Map.size finalKV
        it "journalSize matches actual journal entries"
            $ property
            $ forAll genOps
            $ \ops -> do
                (kvStore, _, _) <- mkCsmtReplayEnv
                let kv = mtsKV kvStore
                finalKV <-
                    foldM
                        ( \m -> \case
                            Insert k v -> do
                                mtsInsert kv k v
                                pure $ Map.insert k v m
                            Overwrite k v -> do
                                mtsInsert kv k v
                                pure $ Map.insert k v m
                            Delete k -> do
                                mtsDelete kv k
                                pure $ Map.delete k m
                        )
                        Map.empty
                        ops
                MtsMetrics{metricsJournalSize} <-
                    mtsMetrics $ mtsKV kvStore
                -- Journal has one entry per live key
                -- (genesis invariant: all JInsert)
                metricsJournalSize
                    `shouldBe` Map.size finalKV
        it "kvCount matches KV column scan after ops"
            $ property
            $ forAll genOps
            $ \ops -> do
                ref <- newIORef emptyInMemoryDB
                let run :: forall b. Pure b -> IO b
                    run action = do
                        s <- readIORef ref
                        let (a, s') = runPure s action
                        writeIORef ref s'
                        pure a
                    db = pureDatabase csmtCodecs
                    rtx = run . runTransactionUnguarded db
                store <-
                    csmtMerkleTreeStore
                        []
                        run
                        db
                        fromKVHashes
                        hashHashing
                let kv = mtsKV store
                mapM_
                    ( \case
                        Insert k v -> mtsInsert kv k v
                        Overwrite k v -> mtsInsert kv k v
                        Delete k -> mtsDelete kv k
                    )
                    ops
                -- Cached metric
                MtsMetrics{metricsKVCount} <-
                    mtsMetrics kv
                -- Actual scan
                actualCount <-
                    rtx
                        $ iterating
                            StandaloneKVCol
                            countEntries
                metricsKVCount
                    `shouldBe` actualCount

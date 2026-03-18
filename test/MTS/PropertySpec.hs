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
import CSMT.MTS
    ( CommonOps (..)
    , CsmtImpl
    , Ops (..)
    , csmtKVOnlyStore
    , csmtManagedTransition
    , csmtMerkleTreeStore
    , csmtReplayJournal
    , mkFullOps
    , mkKVOnlyOps
    )
import Control.Exception (SomeException, try)
import Control.Monad (foldM)
import Control.Lens (Iso', iso)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Either (isLeft)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Database.KV.Cursor
    ( Cursor
    , Entry (..)
    , firstEntry
    , nextEntry
    )
import Database.KV.Database (KeyOf, ValueOf)
import Database.KV.Transaction
    ( iterating
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
    ( MerkleTreeStore
    , Mode (..)
    , MtsKV (..)
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
        , csmtReplayJournal [] 100 run db fromKVHashes hashHashing
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
        , mpfReplayJournal [] 100 run db fromHexKVBS mpfHashing
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
            (iso id id)
            fromKVHashes
            hashHashing
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
                        (iso id id)
                        fromKVHashes
                        hashHashing
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
                                (iso id id)
                                fromKVHashes
                                hashHashing
                                runTx
                                (const $ pure ())
                    mKV2 <- toKVOnly fullOps2
                    case mKV2 of
                        Nothing -> pure ()
                        Just _ ->
                            fail "toKVOnly should fail with non-empty journal"
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
                let applyChunk kvOps kv chunk =
                        foldM (applyOp (kvCommon kvOps) rtx) kv chunk
                    assertAfterReplay fullOps expectedKV = do
                        jn <- rtx
                            $ iterating StandaloneJournalCol
                            $ countEntries
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

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
import Control.Lens (Iso', iso)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Either (isLeft)
import Data.IORef (newIORef, readIORef, writeIORef)
import Database.KV.Transaction (runTransactionUnguarded)
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

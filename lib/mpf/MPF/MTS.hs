-- | MPF implementation of the MTS interface.
--
-- Defines @MpfImpl@ phantom type with type family instances
-- and constructors that wrap MPF operations into
-- 'MerkleTreeStore'.
--
-- Full-mode constructors:
--
-- * 'mpfMerkleTreeStoreT' — prefix-scoped transactional store
-- * 'mpfMerkleTreeStore' — IO convenience wrapper
-- * 'mpfNamespacedMTST' — transactional namespaced store
-- * 'mpfNamespacedMTS' — IO namespaced store
--
-- KVOnly-mode constructors:
--
-- * 'mpfKVOnlyStoreT' — transactional, writes KV + journal
-- * 'mpfKVOnlyStore' — IO convenience wrapper
--
-- Journal operations:
--
-- * 'mpfReplayJournal' — replay journal against tree
-- * 'mpfJournalEmpty' — check if journal has entries
module MPF.MTS
    ( MpfImpl
    , mpfMerkleTreeStoreT
    , mpfMerkleTreeStore
    , mpfNamespacedMTST
    , mpfNamespacedMTS
    , mpfKVOnlyStoreT
    , mpfKVOnlyStore
    , mpfManagedTransition
    , mpfReplayJournal
    , mpfJournalEmpty
    )
where

import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe, isNothing)
import Database.KV.Cursor
    ( Cursor
    , Entry (..)
    , firstEntry
    , nextEntry
    )
import Database.KV.Database (Database, KV)
import Database.KV.Transaction
    ( GCompare
    , Selector
    , Transaction
    , delete
    , insert
    , iterating
    , query
    , runTransactionUnguarded
    )
import MPF.Backend.Standalone
    ( MPFStandalone (..)
    , MPFStandaloneCF
    , MPFStandaloneOp
    )
import MPF.Deletion
    ( deleteSubtree
    , deleting
    , deletingTreeOnly
    )
import MPF.Hashes (MPFHash, MPFHashing (..))
import MPF.Insertion
    ( MPFCompose
    , inserting
    , insertingBatch
    , insertingTreeOnly
    )
import MPF.Interface
    ( FromHexKV (..)
    , HexIndirect (..)
    , HexKey
    )
import MPF.Proof.Completeness
    ( collectMPFLeaves
    , foldMPFCompletenessProof
    , generateMPFCompletenessProof
    )
import MPF.Proof.Insertion
    ( MPFProof
    , foldMPFProof
    , mkMPFInclusionProof
    , verifyMPFInclusionProof
    )
import MTS.Interface
    ( MerkleTreeStore (..)
    , Mode (..)
    , MtsCompletenessProof
    , MtsHash
    , MtsKV (..)
    , MtsKey
    , MtsLeaf
    , MtsMetrics (..)
    , MtsPrefix
    , MtsProof
    , MtsTransition (..)
    , MtsTree (..)
    , MtsValue
    , NamespacedMTS (..)
    , hoistMTS
    , hoistNamespacedMTS
    )

-- | Phantom type tag for the MPF implementation.
data MpfImpl

type instance MtsKey MpfImpl = ByteString
type instance MtsValue MpfImpl = ByteString
type instance MtsHash MpfImpl = MPFHash
type instance MtsProof MpfImpl = MPFProof MPFHash
type instance MtsLeaf MpfImpl = HexIndirect MPFHash
type instance
    MtsCompletenessProof MpfImpl =
        MPFCompose MPFHash
type instance MtsPrefix MpfImpl = HexKey

-- | Journal entry tag bytes.
journalInsertTag, journalDeleteTag :: ByteString
journalInsertTag = B.singleton 0x01
journalDeleteTag = B.singleton 0x00

-- | Encode a journal insert entry.
encodeJournalInsert :: ByteString -> ByteString
encodeJournalInsert v = journalInsertTag <> v

-- | Encode a journal delete entry.
encodeJournalDelete :: ByteString -> ByteString
encodeJournalDelete v = journalDeleteTag <> v

-- | Tag for journal entry.
data JournalTag = JInsert | JDelete

-- | Parse a journal entry into tag + value payload.
parseJournalEntry
    :: ByteString -> (JournalTag, ByteString)
parseJournalEntry bs = case B.uncons bs of
    Just (0x01, rest) -> (JInsert, rest)
    Just (0x00, rest) -> (JDelete, rest)
    _ -> error "parseJournalEntry: invalid tag byte"

-- | Compute the MPF root hash from the root node.
mpfRootFromNode
    :: MPFHashing MPFHash
    -> HexIndirect MPFHash
    -> MPFHash
mpfRootFromNode hashing i =
    if hexIsLeaf i
        then
            leafHash hashing (hexJump i) (hexValue i)
        else hexValue i

-- ------------------------------------------------------------------
-- Metrics helpers
-- ------------------------------------------------------------------

-- | Metric counter key for KV entry count.
kvCountKey :: ByteString
kvCountKey = "kv"

-- | Metric counter key for journal entry count.
journalSizeKey :: ByteString
journalSizeKey = "j"

-- | Read a counter, defaulting to 0 if not set.
readCounter
    :: (Monad m, GCompare d)
    => Selector d ByteString Int
    -> ByteString
    -> Transaction m cf d op Int
readCounter sel key =
    fromMaybe 0 <$> query sel key

-- | Adjust a counter by a delta.
adjustCounter
    :: (Monad m, GCompare d)
    => Selector d ByteString Int
    -> ByteString
    -> Int
    -> Transaction m cf d op ()
adjustCounter sel key delta = do
    current <- readCounter sel key
    insert sel key (current + delta)

-- | Read current MTS metrics from the metrics column.
readMetricsT
    :: (Monad m, GCompare d)
    => Selector d ByteString Int
    -> Transaction m cf d op MtsMetrics
readMetricsT sel =
    MtsMetrics
        <$> readCounter sel kvCountKey
        <*> readCounter sel journalSizeKey

-- ------------------------------------------------------------------
-- Full mode
-- ------------------------------------------------------------------

-- | Build a transactional 'Full' 'MerkleTreeStore' for MPF.
mpfMerkleTreeStoreT
    :: (MonadFail m)
    => HexKey
    -> FromHexKV ByteString ByteString MPFHash
    -> MPFHashing MPFHash
    -> MerkleTreeStore
        'Full
        MpfImpl
        ( Transaction
            m
            MPFStandaloneCF
            ( MPFStandalone
                ByteString
                ByteString
                MPFHash
            )
            MPFStandaloneOp
        )
mpfMerkleTreeStoreT prefix fromKV hashing =
    MkFull kv tree
  where
    kv =
        MtsKV
            { mtsInsert = \k v -> do
                existed <-
                    query MPFStandaloneKVCol k
                inserting
                    prefix
                    fromKV
                    hashing
                    MPFStandaloneKVCol
                    MPFStandaloneMPFCol
                    k
                    v
                when (isNothing existed)
                    $ adjustCounter
                        MPFStandaloneMetricsCol
                        kvCountKey
                        1
            , mtsDelete = \k -> do
                existed <-
                    query MPFStandaloneKVCol k
                deleting
                    prefix
                    fromKV
                    hashing
                    MPFStandaloneKVCol
                    MPFStandaloneMPFCol
                    k
                when (not $ isNothing existed)
                    $ adjustCounter
                        MPFStandaloneMetricsCol
                        kvCountKey
                        (-1)
            , mtsMetrics =
                readMetricsT MPFStandaloneMetricsCol
            }
    tree =
        MtsTree
            { mtsRootHash = do
                mi <-
                    query MPFStandaloneMPFCol prefix
                pure
                    $ fmap (mpfRootFromNode hashing) mi
            , mtsMkProof = \k -> do
                mp <-
                    mkMPFInclusionProof
                        prefix
                        fromKV
                        hashing
                        MPFStandaloneMPFCol
                        k
                case mp of
                    Nothing -> pure Nothing
                    Just proof -> do
                        mi <-
                            query
                                MPFStandaloneMPFCol
                                prefix
                        pure $ case mi of
                            Nothing -> Nothing
                            Just i ->
                                Just
                                    ( mpfRootFromNode
                                        hashing
                                        i
                                    , proof
                                    )
            , mtsVerifyProof =
                verifyMPFInclusionProof
                    prefix
                    fromKV
                    MPFStandaloneMPFCol
                    hashing
            , mtsFoldProof =
                foldMPFProof hashing
            , mtsBatchInsert =
                insertingBatch
                    prefix
                    fromKV
                    hashing
                    MPFStandaloneKVCol
                    MPFStandaloneMPFCol
            , mtsCollectLeaves =
                collectMPFLeaves
                    MPFStandaloneMPFCol
                    prefix
            , mtsMkCompletenessProof =
                generateMPFCompletenessProof
                    MPFStandaloneMPFCol
                    prefix
            , mtsVerifyCompletenessProof =
                \leaves proof -> do
                    mi <-
                        query
                            MPFStandaloneMPFCol
                            prefix
                    let currentRoot =
                            fmap
                                (mpfRootFromNode hashing)
                                mi
                        computed =
                            foldMPFCompletenessProof
                                hashing
                                leaves
                                proof
                    pure $ case (currentRoot, computed) of
                        (Just r, Just computedRoot) ->
                            computedRoot == r
                        _ -> False
            }

-- | Build an IO 'Full' 'MerkleTreeStore' for MPF.
--
-- Checks that the journal is empty before constructing the
-- store. Fails if there are unplayed journal entries.
mpfMerkleTreeStore
    :: (MonadFail m)
    => HexKey
    -> (forall b. m b -> IO b)
    -> Database
        m
        MPFStandaloneCF
        ( MPFStandalone
            ByteString
            ByteString
            MPFHash
        )
        MPFStandaloneOp
    -> FromHexKV ByteString ByteString MPFHash
    -> MPFHashing MPFHash
    -> IO (MerkleTreeStore 'Full MpfImpl IO)
mpfMerkleTreeStore prefix run db fromKV hashing = do
    empty <- mpfJournalEmpty run db
    unless empty
        $ fail
            "mpfMerkleTreeStore: journal is not empty, replay first"
    pure
        $ hoistMTS
            (run . runTransactionUnguarded db)
            ( mpfMerkleTreeStoreT
                prefix
                fromKV
                hashing
            )

-- | Build a transactional 'NamespacedMTS' for MPF.
mpfNamespacedMTST
    :: (MonadFail m)
    => FromHexKV ByteString ByteString MPFHash
    -> MPFHashing MPFHash
    -> NamespacedMTS
        MpfImpl
        ( Transaction
            m
            MPFStandaloneCF
            ( MPFStandalone
                ByteString
                ByteString
                MPFHash
            )
            MPFStandaloneOp
        )
mpfNamespacedMTST fromKV hashing =
    NamespacedMTS
        { nsStore = \prefix ->
            mpfMerkleTreeStoreT
                prefix
                fromKV
                hashing
        , nsDelete =
            deleteSubtree MPFStandaloneMPFCol
        }

-- | Build an IO 'NamespacedMTS' for MPF.
mpfNamespacedMTS
    :: (MonadFail m)
    => (forall b. m b -> IO b)
    -> Database
        m
        MPFStandaloneCF
        ( MPFStandalone
            ByteString
            ByteString
            MPFHash
        )
        MPFStandaloneOp
    -> FromHexKV ByteString ByteString MPFHash
    -> MPFHashing MPFHash
    -> NamespacedMTS MpfImpl IO
mpfNamespacedMTS run db fromKV hashing =
    hoistNamespacedMTS
        (run . runTransactionUnguarded db)
        (mpfNamespacedMTST fromKV hashing)

-- ------------------------------------------------------------------
-- KVOnly mode
-- ------------------------------------------------------------------

-- | Build a transactional 'KVOnly' 'MerkleTreeStore' for MPF.
mpfKVOnlyStoreT
    :: (Monad m)
    => FromHexKV ByteString ByteString MPFHash
    -> MerkleTreeStore
        'KVOnly
        MpfImpl
        ( Transaction
            m
            MPFStandaloneCF
            ( MPFStandalone
                ByteString
                ByteString
                MPFHash
            )
            MPFStandaloneOp
        )
mpfKVOnlyStoreT _fromKV =
    MkKVOnly
        MtsKV
            { mtsInsert = \k v -> do
                existing <-
                    query MPFStandaloneKVCol k
                mj <-
                    query MPFStandaloneJournalCol k
                insert MPFStandaloneKVCol k v
                insert
                    MPFStandaloneJournalCol
                    k
                    (encodeJournalInsert v)
                when (isNothing existing)
                    $ adjustCounter
                        MPFStandaloneMetricsCol
                        kvCountKey
                        1
                when (isNothing mj)
                    $ adjustCounter
                        MPFStandaloneMetricsCol
                        journalSizeKey
                        1
            , mtsDelete = \k -> do
                mv <- query MPFStandaloneKVCol k
                case mv of
                    Nothing -> pure ()
                    Just v -> do
                        delete MPFStandaloneKVCol k
                        adjustCounter
                            MPFStandaloneMetricsCol
                            kvCountKey
                            (-1)
                        mj <-
                            query
                                MPFStandaloneJournalCol
                                k
                        case mj of
                            Nothing -> do
                                insert
                                    MPFStandaloneJournalCol
                                    k
                                    (encodeJournalDelete v)
                                adjustCounter
                                    MPFStandaloneMetricsCol
                                    journalSizeKey
                                    1
                            Just _ ->
                                insert
                                    MPFStandaloneJournalCol
                                    k
                                    (encodeJournalDelete v)
            , mtsMetrics =
                readMetricsT MPFStandaloneMetricsCol
            }

-- | Build an IO 'KVOnly' 'MerkleTreeStore' for MPF.
mpfKVOnlyStore
    :: (MonadFail m)
    => (forall b. m b -> IO b)
    -> Database
        m
        MPFStandaloneCF
        ( MPFStandalone
            ByteString
            ByteString
            MPFHash
        )
        MPFStandaloneOp
    -> FromHexKV ByteString ByteString MPFHash
    -> MerkleTreeStore 'KVOnly MpfImpl IO
mpfKVOnlyStore run db fromKV =
    hoistMTS
        (run . runTransactionUnguarded db)
        (mpfKVOnlyStoreT fromKV)

-- ------------------------------------------------------------------
-- Managed transition
-- ------------------------------------------------------------------

-- | Create a managed lifecycle handle for MPF.
--
-- Returns a 'MtsTransition' that bundles a 'KVOnly' store with
-- a one-shot transition action. After 'transitionToFull' is
-- called, any operation on 'transitionKVStore' throws.
mpfManagedTransition
    :: forall m
     . (MonadFail m)
    => HexKey
    -> Int
    -- ^ Chunk size for journal replay
    -> (forall b. m b -> IO b)
    -> Database
        m
        MPFStandaloneCF
        ( MPFStandalone
            ByteString
            ByteString
            MPFHash
        )
        MPFStandaloneOp
    -> FromHexKV ByteString ByteString MPFHash
    -> MPFHashing MPFHash
    -> IO (MtsTransition MpfImpl IO)
mpfManagedTransition
    prefix
    chunkSize
    run
    db
    fromKV
    hashing = do
        locked <- newIORef False
        let guardedRun
                :: forall b
                 . Transaction
                    m
                    MPFStandaloneCF
                    ( MPFStandalone
                        ByteString
                        ByteString
                        MPFHash
                    )
                    MPFStandaloneOp
                    b
                -> IO b
            guardedRun txn = do
                isLocked <- readIORef locked
                when isLocked
                    $ fail
                        "KVOnly store disabled after transition"
                run (runTransactionUnguarded db txn)
        pure
            MtsTransition
                { transitionKVStore =
                    hoistMTS
                        guardedRun
                        (mpfKVOnlyStoreT fromKV)
                , transitionToFull = do
                    writeIORef locked True
                    mpfReplayJournal
                        prefix
                        chunkSize
                        run
                        db
                        fromKV
                        hashing
                    pure
                        $ hoistMTS
                            ( run
                                . runTransactionUnguarded
                                    db
                            )
                            ( mpfMerkleTreeStoreT
                                prefix
                                fromKV
                                hashing
                            )
                }

-- ------------------------------------------------------------------
-- Journal replay
-- ------------------------------------------------------------------

-- | Check if the journal column is empty.
mpfJournalEmpty
    :: (MonadFail m)
    => (forall b. m b -> IO b)
    -> Database
        m
        MPFStandaloneCF
        ( MPFStandalone
            ByteString
            ByteString
            MPFHash
        )
        MPFStandaloneOp
    -> IO Bool
mpfJournalEmpty run db =
    run $ runTransactionUnguarded db $ do
        me <-
            iterating
                MPFStandaloneJournalCol
                firstEntry
        pure $ case me of
            Nothing -> True
            Just _ -> False

-- | Replay journal entries against the tree, then clear them.
mpfReplayJournal
    :: (MonadFail m)
    => HexKey
    -> Int
    -> (forall b. m b -> IO b)
    -> Database
        m
        MPFStandaloneCF
        ( MPFStandalone
            ByteString
            ByteString
            MPFHash
        )
        MPFStandaloneOp
    -> FromHexKV ByteString ByteString MPFHash
    -> MPFHashing MPFHash
    -> IO ()
mpfReplayJournal
    prefix
    chunkSize
    run
    db
    fromKV
    hashing = loop
      where
        loop = do
            done <-
                run $ runTransactionUnguarded db $ do
                    entries <-
                        iterating
                            MPFStandaloneJournalCol
                            $ do
                                me <- firstEntry
                                case me of
                                    Nothing -> pure []
                                    Just e ->
                                        collectN
                                            (chunkSize - 1)
                                            [e]
                    if null entries
                        then pure True
                        else do
                            mpfReplayEntries
                                prefix
                                fromKV
                                hashing
                                entries
                            adjustCounter
                                MPFStandaloneMetricsCol
                                journalSizeKey
                                ( negate
                                    $ length entries
                                )
                            pure False
            unless done loop

-- | Collect up to @n@ more entries after the first.
collectN
    :: (Monad m)
    => Int
    -> [Entry c]
    -> Cursor m c [Entry c]
collectN 0 acc = pure (reverse acc)
collectN n acc = do
    me <- nextEntry
    case me of
        Nothing -> pure (reverse acc)
        Just e -> collectN (n - 1) (e : acc)

-- | Apply journal entries to the MPF tree and clear them.
mpfReplayEntries
    :: (Monad m)
    => HexKey
    -> FromHexKV ByteString ByteString MPFHash
    -> MPFHashing MPFHash
    -> [Entry (KV ByteString ByteString)]
    -> Transaction
        m
        MPFStandaloneCF
        ( MPFStandalone
            ByteString
            ByteString
            MPFHash
        )
        MPFStandaloneOp
        ()
mpfReplayEntries prefix fromKV hashing entries = do
    mapM_ applyEntry entries
    mapM_
        (delete MPFStandaloneJournalCol . entryKey)
        entries
  where
    applyEntry e =
        let (tag, v) =
                parseJournalEntry (entryValue e)
            k = entryKey e
        in  case tag of
                JInsert ->
                    insertingTreeOnly
                        prefix
                        fromKV
                        hashing
                        MPFStandaloneMPFCol
                        k
                        v
                JDelete ->
                    deletingTreeOnly
                        prefix
                        fromKV
                        hashing
                        MPFStandaloneMPFCol
                        k
                        v

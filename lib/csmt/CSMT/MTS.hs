-- | CSMT implementation of the MTS interface.
--
-- Defines @CsmtImpl@ phantom type with type family instances
-- and constructors that wrap CSMT operations into
-- 'MerkleTreeStore'.
--
-- Full-mode constructors:
--
-- * 'csmtMerkleTreeStoreT' — prefix-scoped transactional store
-- * 'csmtMerkleTreeStore' — IO convenience wrapper
-- * 'csmtNamespacedMTST' — transactional namespaced store
-- * 'csmtNamespacedMTS' — IO namespaced store
--
-- KVOnly-mode constructors:
--
-- * 'csmtKVOnlyStoreT' — transactional, writes KV + journal
-- * 'csmtKVOnlyStore' — IO convenience wrapper
--
-- Journal operations:
--
-- * 'csmtReplayJournal' — replay journal against tree
-- * 'csmtJournalEmpty' — check if journal has entries
module CSMT.MTS
    ( CsmtImpl
    , csmtMerkleTreeStoreT
    , csmtMerkleTreeStore
    , csmtNamespacedMTST
    , csmtNamespacedMTS
    , csmtKVOnlyStoreT
    , csmtKVOnlyStore
    , csmtManagedTransition
    , csmtReplayJournal
    , csmtJournalEmpty
    )
where

import CSMT.Backend.Standalone
    ( Standalone (..)
    , StandaloneCF
    , StandaloneOp
    )
import CSMT.Deletion (deleteSubtree, deleting, deletingTreeOnly)
import CSMT.Hashes (Hash)
import CSMT.Insertion (inserting, insertingTreeOnly)
import CSMT.Interface
    ( FromKV (..)
    , Hashing (..)
    , Indirect (..)
    , Key
    , root
    )
import CSMT.Proof.Completeness
    ( CompletenessProof (..)
    , collectValues
    , foldCompletenessProof
    , generateProof
    )
import CSMT.Proof.Insertion
    ( InclusionProof (..)
    , buildInclusionProof
    , computeRootHash
    , verifyInclusionProof
    )
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.IORef (newIORef, readIORef, writeIORef)
import Database.KV.Cursor
    ( Cursor
    , Entry (..)
    , firstEntry
    , nextEntry
    )
import Database.KV.Database (Database, KV)
import Database.KV.Transaction
    ( Transaction
    , delete
    , insert
    , iterating
    , query
    , runTransactionUnguarded
    )
import MTS.Interface
    ( MerkleTreeStore (..)
    , Mode (..)
    , MtsCompletenessProof
    , MtsHash
    , MtsKV (..)
    , MtsKey
    , MtsLeaf
    , MtsPrefix
    , MtsProof
    , MtsTransition (..)
    , MtsTree (..)
    , MtsValue
    , NamespacedMTS (..)
    , hoistMTS
    , hoistNamespacedMTS
    )

-- | Phantom type tag for the CSMT implementation.
data CsmtImpl

type instance MtsKey CsmtImpl = ByteString
type instance MtsValue CsmtImpl = ByteString
type instance MtsHash CsmtImpl = Hash
type instance MtsProof CsmtImpl = InclusionProof Hash
type instance MtsLeaf CsmtImpl = Indirect Hash
type instance MtsCompletenessProof CsmtImpl = CompletenessProof Hash
type instance MtsPrefix CsmtImpl = Key

-- | Journal entry tag bytes.
journalInsertTag, journalDeleteTag :: ByteString
journalInsertTag = B.singleton 0x01
journalDeleteTag = B.singleton 0x00

-- | Encode a journal insert entry: @0x01 ++ value@.
encodeJournalInsert :: ByteString -> ByteString
encodeJournalInsert v = journalInsertTag <> v

-- | Encode a journal delete entry: @0x00 ++ oldValue@.
encodeJournalDelete :: ByteString -> ByteString
encodeJournalDelete v = journalDeleteTag <> v

-- | Tag for journal entry.
data JournalTag = JInsert | JDelete

-- | Parse a journal entry into tag + value payload.
parseJournalEntry :: ByteString -> (JournalTag, ByteString)
parseJournalEntry bs = case B.uncons bs of
    Just (0x01, rest) -> (JInsert, rest)
    Just (0x00, rest) -> (JDelete, rest)
    _ -> error "parseJournalEntry: invalid tag byte"

-- ------------------------------------------------------------------
-- Full mode
-- ------------------------------------------------------------------

-- | Build a transactional 'Full' 'MerkleTreeStore' for CSMT
-- scoped to a prefix.
csmtMerkleTreeStoreT
    :: (Monad m)
    => Key
    -- ^ Prefix (use @[]@ for root)
    -> FromKV ByteString ByteString Hash
    -> Hashing Hash
    -> MerkleTreeStore
        'Full
        CsmtImpl
        ( Transaction
            m
            StandaloneCF
            (Standalone ByteString ByteString Hash)
            StandaloneOp
        )
csmtMerkleTreeStoreT prefix fromKV hashing =
    MkFull kv tree
  where
    kv =
        MtsKV
            { mtsInsert =
                inserting
                    prefix
                    fromKV
                    hashing
                    StandaloneKVCol
                    StandaloneCSMTCol
            , mtsDelete =
                deleting
                    prefix
                    fromKV
                    hashing
                    StandaloneKVCol
                    StandaloneCSMTCol
            }
    tree =
        MtsTree
            { mtsRootHash =
                root hashing StandaloneCSMTCol prefix
            , mtsMkProof = \k -> do
                mp <-
                    buildInclusionProof
                        prefix
                        fromKV
                        StandaloneKVCol
                        StandaloneCSMTCol
                        hashing
                        k
                case mp of
                    Nothing -> pure Nothing
                    Just (_, proof) -> do
                        mr <-
                            root hashing StandaloneCSMTCol prefix
                        pure $ case mr of
                            Nothing -> Nothing
                            Just r -> Just (r, proof)
            , mtsVerifyProof = \v proof ->
                pure
                    $ proofValue proof == fromV fromKV v
                        && verifyInclusionProof hashing proof
            , mtsFoldProof =
                computeRootHash hashing
            , mtsBatchInsert =
                mapM_
                    ( uncurry
                        ( inserting
                            prefix
                            fromKV
                            hashing
                            StandaloneKVCol
                            StandaloneCSMTCol
                        )
                    )
            , mtsCollectLeaves =
                collectValues StandaloneCSMTCol prefix []
            , mtsMkCompletenessProof =
                generateProof StandaloneCSMTCol prefix []
            , mtsVerifyCompletenessProof = \leaves proof -> do
                currentRoot <-
                    root hashing StandaloneCSMTCol prefix
                let computed =
                        foldCompletenessProof
                            hashing
                            []
                            leaves
                            proof
                pure $ case (currentRoot, computed) of
                    (Just r, Just computedRoot) ->
                        computedRoot == r
                    _ -> False
            }

-- | Build an IO 'Full' 'MerkleTreeStore' for CSMT scoped to a
-- prefix.
--
-- Checks that the journal is empty before constructing the
-- store. Fails if there are unplayed journal entries.
csmtMerkleTreeStore
    :: (MonadFail m)
    => Key
    -- ^ Prefix (use @[]@ for root)
    -> (forall b. m b -> IO b)
    -> Database
        m
        StandaloneCF
        (Standalone ByteString ByteString Hash)
        StandaloneOp
    -> FromKV ByteString ByteString Hash
    -> Hashing Hash
    -> IO (MerkleTreeStore 'Full CsmtImpl IO)
csmtMerkleTreeStore prefix run db fromKV hashing = do
    empty <- csmtJournalEmpty run db
    unless empty
        $ fail
            "csmtMerkleTreeStore: journal is not empty, replay first"
    pure
        $ hoistMTS
            (run . runTransactionUnguarded db)
            (csmtMerkleTreeStoreT prefix fromKV hashing)

-- | Build a transactional 'NamespacedMTS' for CSMT.
csmtNamespacedMTST
    :: (Monad m)
    => FromKV ByteString ByteString Hash
    -> Hashing Hash
    -> NamespacedMTS
        CsmtImpl
        ( Transaction
            m
            StandaloneCF
            (Standalone ByteString ByteString Hash)
            StandaloneOp
        )
csmtNamespacedMTST fromKV hashing =
    NamespacedMTS
        { nsStore = \prefix ->
            csmtMerkleTreeStoreT prefix fromKV hashing
        , nsDelete =
            deleteSubtree StandaloneCSMTCol
        }

-- | Build an IO 'NamespacedMTS' for CSMT.
csmtNamespacedMTS
    :: (MonadFail m)
    => (forall b. m b -> IO b)
    -> Database
        m
        StandaloneCF
        (Standalone ByteString ByteString Hash)
        StandaloneOp
    -> FromKV ByteString ByteString Hash
    -> Hashing Hash
    -> NamespacedMTS CsmtImpl IO
csmtNamespacedMTS run db fromKV hashing =
    hoistNamespacedMTS
        (run . runTransactionUnguarded db)
        (csmtNamespacedMTST fromKV hashing)

-- ------------------------------------------------------------------
-- KVOnly mode
-- ------------------------------------------------------------------

-- | Build a transactional 'KVOnly' 'MerkleTreeStore' for CSMT.
--
-- Each insert/delete writes KV + journal atomically.
-- No tree operations are available.
csmtKVOnlyStoreT
    :: (Monad m)
    => FromKV ByteString ByteString Hash
    -> MerkleTreeStore
        'KVOnly
        CsmtImpl
        ( Transaction
            m
            StandaloneCF
            (Standalone ByteString ByteString Hash)
            StandaloneOp
        )
csmtKVOnlyStoreT _fromKV =
    MkKVOnly
        MtsKV
            { mtsInsert = \k v -> do
                insert StandaloneKVCol k v
                insert
                    StandaloneJournalCol
                    k
                    (encodeJournalInsert v)
            , mtsDelete = \k -> do
                mv <- query StandaloneKVCol k
                case mv of
                    Nothing -> pure ()
                    Just v -> do
                        delete StandaloneKVCol k
                        insert
                            StandaloneJournalCol
                            k
                            (encodeJournalDelete v)
            }

-- | Build an IO 'KVOnly' 'MerkleTreeStore' for CSMT.
csmtKVOnlyStore
    :: (MonadFail m)
    => (forall b. m b -> IO b)
    -> Database
        m
        StandaloneCF
        (Standalone ByteString ByteString Hash)
        StandaloneOp
    -> FromKV ByteString ByteString Hash
    -> MerkleTreeStore 'KVOnly CsmtImpl IO
csmtKVOnlyStore run db fromKV =
    hoistMTS
        (run . runTransactionUnguarded db)
        (csmtKVOnlyStoreT fromKV)

-- ------------------------------------------------------------------
-- Managed transition
-- ------------------------------------------------------------------

-- | Create a managed lifecycle handle for CSMT.
--
-- Returns a 'MtsTransition' that bundles a 'KVOnly' store with
-- a one-shot transition action. After 'transitionToFull' is
-- called, any operation on 'transitionKVStore' throws.
csmtManagedTransition
    :: forall m
     . (MonadFail m)
    => Key
    -- ^ Prefix (use @[]@ for root)
    -> Int
    -- ^ Chunk size for journal replay
    -> (forall b. m b -> IO b)
    -> Database
        m
        StandaloneCF
        (Standalone ByteString ByteString Hash)
        StandaloneOp
    -> FromKV ByteString ByteString Hash
    -> Hashing Hash
    -> IO (MtsTransition CsmtImpl IO)
csmtManagedTransition prefix chunkSize run db fromKV hashing = do
    locked <- newIORef False
    let guardedRun
            :: forall b
             . Transaction
                m
                StandaloneCF
                (Standalone ByteString ByteString Hash)
                StandaloneOp
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
                    (csmtKVOnlyStoreT fromKV)
            , transitionToFull = do
                writeIORef locked True
                csmtReplayJournal
                    prefix
                    chunkSize
                    run
                    db
                    fromKV
                    hashing
                pure
                    $ hoistMTS
                        (run . runTransactionUnguarded db)
                        ( csmtMerkleTreeStoreT
                            prefix
                            fromKV
                            hashing
                        )
            }

-- ------------------------------------------------------------------
-- Journal replay
-- ------------------------------------------------------------------

-- | Check if the journal column is empty.
csmtJournalEmpty
    :: (MonadFail m)
    => (forall b. m b -> IO b)
    -> Database
        m
        StandaloneCF
        (Standalone ByteString ByteString Hash)
        StandaloneOp
    -> IO Bool
csmtJournalEmpty run db =
    run $ runTransactionUnguarded db $ do
        me <- iterating StandaloneJournalCol firstEntry
        pure $ case me of
            Nothing -> True
            Just _ -> False

-- | Replay journal entries against the tree, then clear them.
--
-- Processes entries in chunks. Each chunk reads up to
-- @chunkSize@ entries, applies tree-only operations, and
-- deletes the replayed journal entries, all in one transaction.
-- Repeats until the journal is empty.
csmtReplayJournal
    :: (MonadFail m)
    => Key
    -- ^ Prefix (use @[]@ for root)
    -> Int
    -- ^ Chunk size
    -> (forall b. m b -> IO b)
    -> Database
        m
        StandaloneCF
        (Standalone ByteString ByteString Hash)
        StandaloneOp
    -> FromKV ByteString ByteString Hash
    -> Hashing Hash
    -> IO ()
csmtReplayJournal prefix chunkSize run db fromKV hashing = loop
  where
    loop = do
        done <- run $ runTransactionUnguarded db $ do
            entries <- iterating StandaloneJournalCol $ do
                me <- firstEntry
                case me of
                    Nothing -> pure []
                    Just e -> collectN (chunkSize - 1) [e]
            if null entries
                then pure True
                else do
                    replayEntries prefix fromKV hashing entries
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

-- | Apply journal entries to the tree and clear them.
replayEntries
    :: (Monad m)
    => Key
    -> FromKV ByteString ByteString Hash
    -> Hashing Hash
    -> [Entry (KV ByteString ByteString)]
    -> Transaction
        m
        StandaloneCF
        (Standalone ByteString ByteString Hash)
        StandaloneOp
        ()
replayEntries prefix fromKV hashing entries = do
    mapM_ applyEntry entries
    mapM_
        (delete StandaloneJournalCol . entryKey)
        entries
  where
    applyEntry e =
        let (tag, v) = parseJournalEntry (entryValue e)
            k = entryKey e
        in  case tag of
                JInsert ->
                    insertingTreeOnly
                        prefix
                        fromKV
                        hashing
                        StandaloneCSMTCol
                        k
                        v
                JDelete ->
                    deletingTreeOnly
                        prefix
                        fromKV
                        hashing
                        StandaloneCSMTCol
                        k
                        v

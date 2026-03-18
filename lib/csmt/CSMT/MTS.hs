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
--
-- Split-mode operations:
--
-- * 'CommonOps' — shared KV operations for both modes
-- * 'Ops' — GADT indexed by 'Mode' with bidirectional
--   transitions
-- * 'mkKVOnlyOps' — build 'KVOnly' ops with 'toFull' replay
-- * 'mkFullOps' — build 'Full' ops with 'toKVOnly' transition
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
    , replayJournalChunkT
    , journalEmptyT

      -- * Split-mode Ops GADT
    , CommonOps (..)
    , Ops
        ( OpsKVOnly
        , OpsFull
        , kvCommon
        , toFull
        , fullCommon
        , opsRootHash
        , toKVOnly
        )
    , mkKVOnlyOps
    , mkFullOps

      -- * Replay tracing
    , ReplayEvent (..)
    )
where

import CSMT.Backend.Standalone
    ( Standalone (..)
    , StandaloneCF
    , StandaloneOp
    )
import CSMT.Deletion (deleteSubtree, deleting, deletingTreeOnly)
import CSMT.Hashes (Hash)
import CSMT.Insertion
    ( expandToBucketDepth
    , inserting
    , insertingTreeOnly
    , mergeSubtreeRoots
    )
import CSMT.Interface
    ( FromKV (..)
    , Hashing (..)
    , Indirect (..)
    , Key
    , root
    )
import CSMT.Populate (PatchOp (..), patchParallel)
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
import Control.Concurrent.Async (mapConcurrently_)
import Control.Lens (Iso', review, view)
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
    ( GCompare
    , Selector
    , Transaction
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
            cf
            (Standalone ByteString ByteString Hash)
            op
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
            cf
            (Standalone ByteString ByteString Hash)
            op
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
            cf
            (Standalone ByteString ByteString Hash)
            op
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
    run
        $ runTransactionUnguarded db
        $ journalEmptyT StandaloneJournalCol

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
        done <-
            run
                $ runTransactionUnguarded db
                $ replayJournalChunkT
                    prefix
                    chunkSize
                    fromKV
                    hashing
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

-- | Check if the journal column is empty (transactional).
--
-- Polymorphic in @cf@, @op@, and column type @d@ so it can
-- be used with any column definition.
journalEmptyT
    :: (Monad m, GCompare d)
    => Selector d k ByteString
    -- ^ Journal column
    -> Transaction m cf d op Bool
journalEmptyT journalCol = do
    me <- iterating journalCol firstEntry
    pure $ case me of
        Nothing -> True
        Just _ -> False

-- | Process one chunk of journal entries (transactional).
--
-- Reads up to @chunkSize@ journal entries, applies tree-only
-- operations, and deletes the replayed entries. Returns 'True'
-- when the journal is empty (all done), 'False' if more chunks
-- remain.
--
-- Polymorphic in @cf@ and @op@ so it can be composed with
-- 'mapColumns' into richer column types.
replayJournalChunkT
    :: (Monad m)
    => Key
    -- ^ Prefix (use @[]@ for root)
    -> Int
    -- ^ Chunk size
    -> FromKV ByteString ByteString Hash
    -> Hashing Hash
    -> Transaction
        m
        cf
        (Standalone ByteString ByteString Hash)
        op
        Bool
replayJournalChunkT prefix chunkSize fromKV hashing = do
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

-- | Apply journal entries to the tree and clear them.
replayEntries
    :: (Monad m)
    => Key
    -> FromKV ByteString ByteString Hash
    -> Hashing Hash
    -> [Entry (KV ByteString ByteString)]
    -> Transaction
        m
        cf
        (Standalone ByteString ByteString Hash)
        op
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

-- ------------------------------------------------------------------
-- Split-mode Ops GADT
-- ------------------------------------------------------------------

-- | Shared KV operations available in both modes.
data CommonOps m cf d ops k v = CommonOps
    { opsInsert
        :: k
        -> v
        -> Transaction m cf d ops ()
    -- ^ Insert a key-value pair
    , opsDelete
        :: k
        -> Transaction m cf d ops ()
    -- ^ Delete a key
    , opsQuery
        :: k
        -> Transaction m cf d ops (Maybe v)
    -- ^ Query a key
    }

-- | Mode-indexed operations with bidirectional transitions.
--
-- In 'KVOnly' mode, mutations write KV + journal. 'toFull'
-- replays the journal via 'patchParallel' and returns 'Full'
-- ops.
--
-- In 'Full' mode, mutations write KV + update CSMT tree.
-- 'toKVOnly' verifies the journal is empty and returns
-- 'KVOnly' ops. Fails if journal is not empty.
data Ops (mode :: Mode) m cf d ops k v a where
    OpsKVOnly
        :: { kvCommon :: CommonOps m cf d ops k v
           , toFull
                :: IO (Maybe (Ops 'Full m cf d ops k v a))
           }
        -> Ops 'KVOnly m cf d ops k v a
    OpsFull
        :: { fullCommon :: CommonOps m cf d ops k v
           , opsRootHash
                :: Transaction m cf d ops (Maybe a)
           , toKVOnly
                :: IO
                    ( Maybe
                        ( Ops
                            'KVOnly
                            m
                            cf
                            d
                            ops
                            k
                            v
                            a
                        )
                    )
           }
        -> Ops 'Full m cf d ops k v a

-- | Replay trace events. Emitted in pairs: 'ReplayStart'
-- before the concurrent batch, 'ReplayStop' after.
data ReplayEvent
    = -- | About to run bucket transactions concurrently.
      ReplayStart
        { rsChunkSize :: Int
        -- ^ Journal entries in this chunk
        , rsBuckets :: Int
        -- ^ Active buckets (with ops)
        , rsTotalBuckets :: Int
        -- ^ Total bucket count (2^bucketBits)
        , rsOpsPerBucket :: [Int]
        -- ^ Ops per active bucket
        }
    | -- | Concurrent batch completed.
      ReplayStop
    deriving stock (Show)

-- | Build 'KVOnly' ops for generic column types.
--
-- Insert/delete write KV + journal atomically. Query reads
-- KV. 'toFull' replays the journal via 'patchParallel'.
mkKVOnlyOps
    :: (Monad m, GCompare d, Ord k)
    => Key
    -- ^ Prefix
    -> Int
    -- ^ Bucket bits for parallel replay
    -> Int
    -- ^ Chunk size for journal batches
    -> Selector d k v
    -- ^ KV column
    -> Selector d Key (Indirect a)
    -- ^ CSMT column
    -> Selector d k ByteString
    -- ^ Journal column
    -> Iso' v ByteString
    -- ^ Journal value serialization
    -> FromKV k v a
    -> Hashing a
    -> (forall b. Transaction m cf d ops b -> IO b)
    -- ^ Transaction runner (must be thread-safe)
    -> (ReplayEvent -> IO ())
    -- ^ Trace callback (called per replay chunk)
    -> Ops 'KVOnly m cf d ops k v a
mkKVOnlyOps
    prefix
    bucketBits
    chunkSize
    kvCol
    csmtCol
    journalCol
    journalIso
    fromKV
    hashing
    runTx
    trace =
        OpsKVOnly
            { kvCommon =
                CommonOps
                    { opsInsert = \k v -> do
                        insert kvCol k v
                        insert
                            journalCol
                            k
                            ( encodeJournalInsert
                                (view journalIso v)
                            )
                    , opsDelete = \k -> do
                        mv <- query kvCol k
                        case mv of
                            Nothing -> pure ()
                            Just v -> do
                                delete kvCol k
                                insert
                                    journalCol
                                    k
                                    ( encodeJournalDelete
                                        (view journalIso v)
                                    )
                    , opsQuery = query kvCol
                    }
            , toFull = do
                runTx
                    $ expandToBucketDepth
                        prefix
                        bucketBits
                        csmtCol
                replayLoop
                runTx
                    $ mergeSubtreeRoots
                        prefix
                        hashing
                        csmtCol
                        bucketBits
                pure
                    $ Just
                    $ mkFullOps
                        prefix
                        bucketBits
                        chunkSize
                        kvCol
                        csmtCol
                        journalCol
                        journalIso
                        fromKV
                        hashing
                        runTx
                        trace
            }
      where
        totalBuckets = 2 ^ bucketBits :: Int
        replayLoop = do
            entries <-
                runTx
                    $ readJournalChunkT
                        journalCol
                        chunkSize
            if null entries
                then pure ()
                else do
                    let ops =
                            journalEntriesToPatchOps
                                journalIso
                                fromKV
                                entries
                        bucketTxns =
                            patchParallel
                                bucketBits
                                prefix
                                hashing
                                csmtCol
                                journalCol
                                ops
                    trace
                        ReplayStart
                            { rsChunkSize = length entries
                            , rsBuckets = length bucketTxns
                            , rsTotalBuckets = totalBuckets
                            , rsOpsPerBucket =
                                map fst bucketTxns
                            }
                    mapConcurrently_
                        (runTx . snd)
                        bucketTxns
                    trace ReplayStop
                    replayLoop

-- | Build 'Full' ops for generic column types.
--
-- Insert/delete write KV + update CSMT tree. Query reads KV.
-- 'toKVOnly' verifies journal is empty and returns 'KVOnly'
-- ops.
mkFullOps
    :: (Monad m, GCompare d, Ord k)
    => Key
    -- ^ Prefix
    -> Int
    -- ^ Bucket bits (passed through to 'mkKVOnlyOps')
    -> Int
    -- ^ Chunk size (passed through to 'mkKVOnlyOps')
    -> Selector d k v
    -- ^ KV column
    -> Selector d Key (Indirect a)
    -- ^ CSMT column
    -> Selector d k ByteString
    -- ^ Journal column
    -> Iso' v ByteString
    -- ^ Journal value serialization
    -> FromKV k v a
    -> Hashing a
    -> (forall b. Transaction m cf d ops b -> IO b)
    -- ^ Transaction runner
    -> (ReplayEvent -> IO ())
    -- ^ Trace callback (passed through to 'mkKVOnlyOps')
    -> Ops 'Full m cf d ops k v a
mkFullOps
    prefix
    bucketBits
    chunkSize
    kvCol
    csmtCol
    journalCol
    journalIso
    fromKV
    hashing
    runTx
    trace =
        OpsFull
            { fullCommon =
                CommonOps
                    { opsInsert =
                        inserting
                            prefix
                            fromKV
                            hashing
                            kvCol
                            csmtCol
                    , opsDelete =
                        deleting
                            prefix
                            fromKV
                            hashing
                            kvCol
                            csmtCol
                    , opsQuery = query kvCol
                    }
            , opsRootHash =
                root hashing csmtCol prefix
            , toKVOnly = do
                empty <- runTx (journalEmptyT journalCol)
                if empty
                    then
                        pure
                            $ Just
                            $ mkKVOnlyOps
                                prefix
                                bucketBits
                                chunkSize
                                kvCol
                                csmtCol
                                journalCol
                                journalIso
                                fromKV
                                hashing
                                runTx
                                trace
                    else pure Nothing
            }

-- | Read up to @n@ journal entries (transactional).
readJournalChunkT
    :: (Monad m, GCompare d)
    => Selector d k ByteString
    -- ^ Journal column
    -> Int
    -> Transaction m cf d op [(k, ByteString)]
readJournalChunkT journalCol chunkSize = do
    entries <- iterating journalCol $ do
        me <- firstEntry
        case me of
            Nothing -> pure []
            Just e -> collectN (chunkSize - 1) [e]
    pure [(entryKey e, entryValue e) | e <- entries]

-- | Convert journal entries to 'PatchOp' pairs for
-- 'patchParallel'.
journalEntriesToPatchOps
    :: Iso' v ByteString
    -- ^ Journal value serialization
    -> FromKV k v a
    -> [(k, ByteString)]
    -- ^ (journal key, encoded journal value)
    -> [(k, PatchOp Key a)]
journalEntriesToPatchOps journalIso fromKV = map convert
  where
    convert (k, raw) =
        let (tag, serializedV) = parseJournalEntry raw
            v = review journalIso serializedV
            treeKey =
                treePrefix fromKV v <> view (isoK fromKV) k
            hash = fromV fromKV v
        in  case tag of
                JInsert ->
                    (k, PatchInsert treeKey hash)
                JDelete ->
                    (k, PatchDelete treeKey)

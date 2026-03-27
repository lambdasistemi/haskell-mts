-- | Shared interface for Merkle Tree Store implementations.
--
-- Uses type families so each implementation phantom type
-- (@CsmtImpl@, @MpfImpl@) determines key, value, hash, and
-- proof types.
--
-- The store is parameterised by a @mode@ ('KVOnly' or 'Full')
-- that determines which operations are available at the type
-- level.
module MTS.Interface
    ( -- * Mode
      Mode (..)

      -- * Metrics
    , MtsMetrics (..)

      -- * Replay tracing
    , ReplayEvent (..)

      -- * Operation records
    , MtsKV (..)
    , MtsTree (..)

      -- * Store GADT
    , MerkleTreeStore (..)
    , mtsKV
    , mtsTree

      -- * Lifecycle transition
    , MtsTransition (..)

      -- * Natural transformations
    , hoistMtsKV
    , hoistMtsTree
    , hoistMTS

      -- * Type families
    , MtsKey
    , MtsValue
    , MtsHash
    , MtsProof
    , MtsLeaf
    , MtsCompletenessProof
    , MtsPrefix

      -- * Namespaced store
    , NamespacedMTS (..)
    , hoistNamespacedMTS
    )
where

-- | Key type for an implementation.
type family MtsKey imp

-- | Value type for an implementation.
type family MtsValue imp

-- | Hash type for an implementation.
type family MtsHash imp

-- | Proof type for an implementation.
type family MtsProof imp

-- | Leaf type for completeness proofs.
type family MtsLeaf imp

-- | Completeness proof type for an implementation.
type family MtsCompletenessProof imp

-- | Store mode: 'KVOnly' during bootstrap (no tree ops),
-- 'Full' after replay (all operations).
data Mode = KVOnly | Full

-- | Persistent store metrics. Maintained transactionally
-- alongside data mutations for O(1) queries.
data MtsMetrics = MtsMetrics
    { metricsKVCount :: Int
    -- ^ Number of key-value entries
    , metricsJournalSize :: Int
    -- ^ Number of pending journal entries
    }
    deriving stock (Show, Eq)

-- | Replay trace events. Emitted in pairs: 'ReplayStart'
-- before processing a chunk, 'ReplayStop' after.
data ReplayEvent
    = -- | About to process a journal chunk.
      ReplayStart
        { rsChunkSize :: Int
        -- ^ Journal entries in this chunk
        , rsBuckets :: Int
        -- ^ Active buckets (with ops)
        , rsTotalBuckets :: Int
        -- ^ Total bucket count (2^bucketBits)
        , rsOpsPerBucket :: [Int]
        -- ^ Ops per active bucket
        , rsEntriesRemaining :: Int
        -- ^ Journal entries left to process after this chunk
        }
    | -- | Chunk processing completed.
      ReplayStop
    deriving stock (Show)

-- | KV operations -- available in both modes.
data MtsKV imp m = MtsKV
    { mtsInsert :: MtsKey imp -> MtsValue imp -> m ()
    -- ^ Insert a key-value pair
    , mtsDelete :: MtsKey imp -> m ()
    -- ^ Delete a key
    , mtsMetrics :: m MtsMetrics
    -- ^ Query current metrics
    }

-- | Tree operations -- only available in 'Full' mode.
data MtsTree imp m = MtsTree
    { mtsRootHash :: m (Maybe (MtsHash imp))
    -- ^ Query the current root hash
    , mtsMkProof
        :: MtsKey imp
        -> m (Maybe (MtsHash imp, MtsProof imp))
    -- ^ Generate a membership proof
    , mtsVerifyProof :: MtsValue imp -> MtsProof imp -> m Bool
    -- ^ Verify a membership proof
    , mtsFoldProof :: MtsProof imp -> MtsHash imp
    -- ^ Compute root hash from a proof
    , mtsBatchInsert :: [(MtsKey imp, MtsValue imp)] -> m ()
    -- ^ Batch insert multiple key-value pairs
    , mtsCollectLeaves :: m [MtsLeaf imp]
    -- ^ Collect all leaves from the tree, sorted
    , mtsMkCompletenessProof
        :: m (Maybe (MtsCompletenessProof imp))
    -- ^ Generate a completeness proof for the whole tree
    , mtsVerifyCompletenessProof
        :: [MtsLeaf imp]
        -> MtsCompletenessProof imp
        -> m Bool
    -- ^ Verify a completeness proof against leaves
    }

-- | Mode-indexed Merkle tree store.
--
-- In 'KVOnly' mode only KV operations are available (insert,
-- delete). In 'Full' mode both KV and tree operations
-- (root hash, proofs, batch insert) are available.
data MerkleTreeStore (mode :: Mode) imp m where
    MkKVOnly :: MtsKV imp m -> MerkleTreeStore 'KVOnly imp m
    MkFull :: MtsKV imp m -> MtsTree imp m -> MerkleTreeStore 'Full imp m

-- | Extract KV operations from any mode.
mtsKV :: MerkleTreeStore mode imp m -> MtsKV imp m
mtsKV (MkKVOnly kv) = kv
mtsKV (MkFull kv _) = kv

-- | Extract tree operations (only from 'Full').
mtsTree :: MerkleTreeStore 'Full imp m -> MtsTree imp m
mtsTree (MkFull _ tree) = tree

-- | Lifecycle handle for managed mode transitions.
--
-- Bundles a 'KVOnly' store with a one-shot transition action
-- that replays the journal and returns a 'Full' store. After
-- 'transitionToFull' is called, operations on
-- 'transitionKVStore' throw to prevent concurrent usage.
data MtsTransition imp m = MtsTransition
    { transitionKVStore :: MerkleTreeStore 'KVOnly imp m
    -- ^ KVOnly store (disabled after transition)
    , transitionToFull :: m (MerkleTreeStore 'Full imp m)
    -- ^ Replay journal and return Full store.
    --   Permanently disables 'transitionKVStore'.
    }

-- | Transform the monad of 'MtsKV'.
hoistMtsKV
    :: (forall a. m a -> n a)
    -> MtsKV imp m
    -> MtsKV imp n
hoistMtsKV f s =
    MtsKV
        { mtsInsert = \k v -> f (mtsInsert s k v)
        , mtsDelete = f . mtsDelete s
        , mtsMetrics = f (mtsMetrics s)
        }

-- | Transform the monad of 'MtsTree'.
hoistMtsTree
    :: (forall a. m a -> n a)
    -> MtsTree imp m
    -> MtsTree imp n
hoistMtsTree f s =
    MtsTree
        { mtsRootHash = f (mtsRootHash s)
        , mtsMkProof = f . mtsMkProof s
        , mtsVerifyProof = \v p -> f (mtsVerifyProof s v p)
        , mtsFoldProof = mtsFoldProof s
        , mtsBatchInsert = f . mtsBatchInsert s
        , mtsCollectLeaves = f (mtsCollectLeaves s)
        , mtsMkCompletenessProof = f (mtsMkCompletenessProof s)
        , mtsVerifyCompletenessProof =
            \ls p -> f (mtsVerifyCompletenessProof s ls p)
        }

-- | Transform the monad of a 'MerkleTreeStore' via a natural
-- transformation.
hoistMTS
    :: (forall a. m a -> n a)
    -> MerkleTreeStore mode imp m
    -> MerkleTreeStore mode imp n
hoistMTS f (MkKVOnly kv) = MkKVOnly (hoistMtsKV f kv)
hoistMTS f (MkFull kv tree) =
    MkFull (hoistMtsKV f kv) (hoistMtsTree f tree)

-- | Namespace prefix type for an implementation.
type family MtsPrefix imp

-- | A namespaced Merkle tree store supporting multiple independent
-- namespaces within one database. Always 'Full' mode.
data NamespacedMTS imp m = NamespacedMTS
    { nsStore :: MtsPrefix imp -> MerkleTreeStore 'Full imp m
    -- ^ Get a scoped store for a namespace.
    --   Creation is implicit on first insert.
    , nsDelete :: MtsPrefix imp -> m ()
    -- ^ Delete an entire namespace (all nodes under prefix).
    }

-- | Transform the monad of a 'NamespacedMTS'.
hoistNamespacedMTS
    :: (forall a. m a -> n a)
    -> NamespacedMTS imp m
    -> NamespacedMTS imp n
hoistNamespacedMTS f ns =
    NamespacedMTS
        { nsStore = hoistMTS f . nsStore ns
        , nsDelete = f . nsDelete ns
        }

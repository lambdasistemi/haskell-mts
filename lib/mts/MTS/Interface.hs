-- | Shared interface for Merkle Tree Store implementations.
--
-- Uses type families so each implementation phantom type
-- (@CsmtImpl@, @MpfImpl@) determines key, value, hash, and
-- proof types. The 'MerkleTreeStore' record is parameterised
-- by just the implementation tag and the monad.
module MTS.Interface
    ( MerkleTreeStore (..)
    , hoistMTS
    , MtsKey
    , MtsValue
    , MtsHash
    , MtsProof
    , MtsLeaf
    , MtsCompletenessProof
    , MtsPrefix
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

-- | A generic Merkle tree store providing insert, delete,
-- proof generation and verification.
data MerkleTreeStore imp m = MerkleTreeStore
    { mtsInsert :: MtsKey imp -> MtsValue imp -> m ()
    -- ^ Insert a key-value pair
    , mtsDelete :: MtsKey imp -> m ()
    -- ^ Delete a key
    , mtsRootHash :: m (Maybe (MtsHash imp))
    -- ^ Query the current root hash
    , mtsMkProof :: MtsKey imp -> m (Maybe (MtsHash imp, MtsProof imp))
    -- ^ Generate a membership proof anchored to the current root hash
    , mtsVerifyProof :: MtsValue imp -> MtsProof imp -> m Bool
    -- ^ Verify a membership proof for a value
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

-- | Transform the monad of a 'MerkleTreeStore' via a natural
-- transformation. Use this to run a transactional store in 'IO'
-- by supplying @run . runTransactionUnguarded db@.
hoistMTS
    :: (forall a. m a -> n a)
    -> MerkleTreeStore imp m
    -> MerkleTreeStore imp n
hoistMTS f s =
    MerkleTreeStore
        { mtsInsert = \k v -> f (mtsInsert s k v)
        , mtsDelete = f . mtsDelete s
        , mtsRootHash = f (mtsRootHash s)
        , mtsMkProof = f . mtsMkProof s
        , mtsVerifyProof = \v p -> f (mtsVerifyProof s v p)
        , mtsFoldProof = mtsFoldProof s
        , mtsBatchInsert = f . mtsBatchInsert s
        , mtsCollectLeaves = f (mtsCollectLeaves s)
        , mtsMkCompletenessProof = f (mtsMkCompletenessProof s)
        , mtsVerifyCompletenessProof =
            \ls p -> f (mtsVerifyCompletenessProof s ls p)
        }

-- | Namespace prefix type for an implementation.
type family MtsPrefix imp

-- | A namespaced Merkle tree store supporting multiple independent
-- namespaces within one database.
data NamespacedMTS imp m = NamespacedMTS
    { nsStore :: MtsPrefix imp -> MerkleTreeStore imp m
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

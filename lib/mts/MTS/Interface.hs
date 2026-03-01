-- | Shared interface for Merkle Tree Store implementations.
--
-- Uses type families so each implementation phantom type
-- (@CsmtImpl@, @MpfImpl@) determines key, value, hash, and
-- proof types. The 'MerkleTreeStore' record is parameterised
-- by just the implementation tag and the monad.
module MTS.Interface
    ( MerkleTreeStore (..)
    , MtsKey
    , MtsValue
    , MtsHash
    , MtsProof
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

-- | A generic Merkle tree store providing insert, delete,
-- proof generation and verification.
data MerkleTreeStore imp m = MerkleTreeStore
    { mtsInsert :: MtsKey imp -> MtsValue imp -> m ()
    -- ^ Insert a key-value pair
    , mtsDelete :: MtsKey imp -> m ()
    -- ^ Delete a key
    , mtsRootHash :: m (Maybe (MtsHash imp))
    -- ^ Query the current root hash
    , mtsMkProof :: MtsKey imp -> m (Maybe (MtsProof imp))
    -- ^ Generate a membership proof
    , mtsVerifyProof :: MtsValue imp -> MtsProof imp -> m Bool
    -- ^ Verify a membership proof for a value
    , mtsFoldProof :: MtsHash imp -> MtsProof imp -> MtsHash imp
    -- ^ Compute root hash from a proof
    , mtsBatchInsert :: [(MtsKey imp, MtsValue imp)] -> m ()
    -- ^ Batch insert multiple key-value pairs
    }

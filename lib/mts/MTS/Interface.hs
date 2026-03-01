-- | Shared interface for Merkle Tree Store implementations.
--
-- Both CSMT (binary trie) and MPF (16-ary trie) provide
-- constructors that wrap their operations into this record,
-- enabling shared QuickCheck properties across implementations.
module MTS.Interface
    ( MerkleTreeStore (..)
    )
where

-- | A generic Merkle tree store providing insert, delete,
-- proof generation and verification.
data MerkleTreeStore m k v hash proof = MerkleTreeStore
    { mtsInsert :: k -> v -> m ()
    -- ^ Insert a key-value pair
    , mtsDelete :: k -> m ()
    -- ^ Delete a key
    , mtsRootHash :: m (Maybe hash)
    -- ^ Query the current root hash
    , mtsMkProof :: k -> m (Maybe proof)
    -- ^ Generate a membership proof
    , mtsVerifyProof :: v -> proof -> m Bool
    -- ^ Verify a membership proof for a value
    , mtsFoldProof :: hash -> proof -> hash
    -- ^ Compute root hash from a proof
    , mtsBatchInsert :: [(k, v)] -> m ()
    -- ^ Batch insert multiple key-value pairs
    }

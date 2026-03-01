-- | Shared QuickCheck properties for Merkle tree store
-- implementations. Each property takes a store constructor
-- and generators as arguments, returning a 'Property'.
module MTS.Properties
    ( propInsertVerify
    , propMultipleInsertAllVerify
    , propInsertionOrderIndependence
    , propDeleteRemovesKey
    , propDeletePreservesSiblings
    , propBatchEqualsSequential
    , propInsertDeleteAllEmpty
    , propEmptyTreeNoRoot
    , propSingleInsertHasRoot
    , propWrongValueRejects
    )
where

import Data.List (sortBy)
import Data.Ord (comparing)
import MTS.Interface (MerkleTreeStore (..))
import Test.QuickCheck
    ( Gen
    , Property
    , forAll
    , property
    , (===)
    , (.&&.)
    )

-- | After inserting k v, verify k v returns True.
propInsertVerify
    :: (Show k, Show v, Eq v)
    => IO (MerkleTreeStore IO k v hash proof)
    -> Gen (k, v)
    -> Property
propInsertVerify mkStore gen = property $ forAll gen $ \(k, v) -> do
    store <- mkStore
    mtsInsert store k v
    mp <- mtsMkProof store k
    case mp of
        Nothing -> pure False
        Just proof -> mtsVerifyProof store v proof

-- | Insert N pairs, all verify.
propMultipleInsertAllVerify
    :: (Show k, Show v, Eq v)
    => IO (MerkleTreeStore IO k v hash proof)
    -> Gen [(k, v)]
    -> Property
propMultipleInsertAllVerify mkStore gen =
    property $ forAll gen $ \kvs -> do
        store <- mkStore
        mapM_ (uncurry $ mtsInsert store) kvs
        results <- mapM (\(k, v) -> do
            mp <- mtsMkProof store k
            case mp of
                Nothing -> pure False
                Just proof -> mtsVerifyProof store v proof
            ) kvs
        pure $ and results

-- | Same keys in any order produce the same root hash.
propInsertionOrderIndependence
    :: (Show k, Show v, Eq hash, Ord k)
    => IO (MerkleTreeStore IO k v hash proof)
    -> IO (MerkleTreeStore IO k v hash proof)
    -> Gen [(k, v)]
    -> Property
propInsertionOrderIndependence mkStore1 mkStore2 gen =
    property $ forAll gen $ \kvs -> do
        let sorted = sortBy (comparing fst) kvs
            reversed' = reverse sorted
        store1 <- mkStore1
        mapM_ (uncurry $ mtsInsert store1) sorted
        h1 <- mtsRootHash store1
        store2 <- mkStore2
        mapM_ (uncurry $ mtsInsert store2) reversed'
        h2 <- mtsRootHash store2
        pure $ h1 == h2

-- | Insert k v, delete k, verify k v returns False.
propDeleteRemovesKey
    :: (Show k, Show v, Eq v)
    => IO (MerkleTreeStore IO k v hash proof)
    -> Gen (k, v)
    -> Property
propDeleteRemovesKey mkStore gen =
    property $ forAll gen $ \(k, v) -> do
        store <- mkStore
        mtsInsert store k v
        mtsDelete store k
        mp <- mtsMkProof store k
        case mp of
            Nothing -> pure True
            Just proof -> not <$> mtsVerifyProof store v proof

-- | Insert 3, delete one, other two still verify.
propDeletePreservesSiblings
    :: (Show k, Show v, Eq v)
    => IO (MerkleTreeStore IO k v hash proof)
    -> Gen (k, v)
    -> Gen (k, v)
    -> Gen (k, v)
    -> Property
propDeletePreservesSiblings mkStore gen1 gen2 gen3 =
    property $ forAll gen1 $ \(k1, v1) ->
        forAll gen2 $ \(k2, v2) ->
            forAll gen3 $ \(k3, v3) -> do
                store <- mkStore
                mtsInsert store k1 v1
                mtsInsert store k2 v2
                mtsInsert store k3 v3
                mtsDelete store k2
                r1 <- do
                    mp <- mtsMkProof store k1
                    case mp of
                        Nothing -> pure False
                        Just proof -> mtsVerifyProof store v1 proof
                r3 <- do
                    mp <- mtsMkProof store k3
                    case mp of
                        Nothing -> pure False
                        Just proof -> mtsVerifyProof store v3 proof
                pure $ r1 && r3

-- | Batch insert produces same root as sequential.
propBatchEqualsSequential
    :: (Show k, Show v, Eq hash)
    => IO (MerkleTreeStore IO k v hash proof)
    -> IO (MerkleTreeStore IO k v hash proof)
    -> Gen [(k, v)]
    -> Property
propBatchEqualsSequential mkSeqStore mkBatchStore gen =
    property $ forAll gen $ \kvs -> do
        seqStore <- mkSeqStore
        mapM_ (uncurry $ mtsInsert seqStore) kvs
        h1 <- mtsRootHash seqStore
        batchStore <- mkBatchStore
        mtsBatchInsert batchStore kvs
        h2 <- mtsRootHash batchStore
        pure $ h1 == h2

-- | Insert N, delete all N, root is Nothing.
propInsertDeleteAllEmpty
    :: (Show k, Show v)
    => IO (MerkleTreeStore IO k v hash proof)
    -> Gen [(k, v)]
    -> Property
propInsertDeleteAllEmpty mkStore gen =
    property $ forAll gen $ \kvs -> do
        store <- mkStore
        mapM_ (uncurry $ mtsInsert store) kvs
        mapM_ (mtsDelete store . fst) kvs
        h <- mtsRootHash store
        pure $ h == Nothing

-- | Empty tree has no root hash.
propEmptyTreeNoRoot
    :: IO (MerkleTreeStore IO k v hash proof)
    -> Property
propEmptyTreeNoRoot mkStore = property $ do
    store <- mkStore
    h <- mtsRootHash store
    pure $ h === Nothing

-- | Single insert produces a root hash.
propSingleInsertHasRoot
    :: (Show k, Show v, Show hash)
    => IO (MerkleTreeStore IO k v hash proof)
    -> Gen (k, v)
    -> Property
propSingleInsertHasRoot mkStore gen =
    property $ forAll gen $ \(k, v) -> do
        store <- mkStore
        mtsInsert store k v
        h <- mtsRootHash store
        pure $ h /= Nothing

-- | Insert k v, verify k v' where v /= v' returns False.
propWrongValueRejects
    :: (Show k, Show v, Eq v)
    => IO (MerkleTreeStore IO k v hash proof)
    -> Gen (k, v, v)
    -- ^ Generate (key, correct value, wrong value) where values differ
    -> Property
propWrongValueRejects mkStore gen =
    property $ forAll gen $ \(k, v, v') -> do
        store <- mkStore
        mtsInsert store k v
        mp <- mtsMkProof store k
        case mp of
            Nothing -> pure False
            Just proof -> not <$> mtsVerifyProof store v' proof

{-# LANGUAGE DataKinds #-}

-- | Shared QuickCheck properties for Merkle tree store
-- implementations. Each property takes a store constructor
-- and generators as arguments, returning a 'Property'.
--
-- Existing properties operate on @'Full@-mode stores.
-- Replay properties test the @'KVOnly@ to @'Full@ transition.
module MTS.Properties
    ( -- * Full-mode properties
      propInsertVerify
    , propMultipleInsertAllVerify
    , propInsertionOrderIndependence
    , propDeleteRemovesKey
    , propDeletePreservesSiblings
    , propBatchEqualsSequential
    , propInsertDeleteAllEmpty
    , propEmptyTreeNoRoot
    , propSingleInsertHasRoot
    , propWrongValueRejects
    , propProofAnchoredToRoot
    , propCompletenessRoundTrip
    , propCompletenessEmpty
    , propCompletenessAfterDelete

      -- * Replay properties
    , propKVOnlyThenReplayMatchesFull
    , propKVOnlyThenReplayProofsWork
    , propKVOnlyDeleteThenReplay
    , propReplayIdempotent
    , propJournalCompression

      -- * Trace properties
    , propReplayTraceMonotonic
    )
where

import Data.List (sortBy)
import Data.Maybe (isJust, isNothing)
import Data.Ord (comparing)
import MTS.Interface
    ( MerkleTreeStore (..)
    , Mode (..)
    , MtsHash
    , MtsKV (..)
    , MtsKey
    , MtsTree (..)
    , MtsValue
    , ReplayEvent (..)
    , mtsKV
    , mtsTree
    )
import Test.QuickCheck
    ( Gen
    , Property
    , forAll
    , ioProperty
    , property
    , (==>)
    )

-- ------------------------------------------------------------------
-- Full-mode properties
-- ------------------------------------------------------------------

-- | After inserting k v, verify k v returns True.
propInsertVerify
    :: ( Show (MtsKey imp)
       , Show (MtsValue imp)
       )
    => IO (MerkleTreeStore 'Full imp IO)
    -> Gen (MtsKey imp, MtsValue imp)
    -> Property
propInsertVerify mkStore gen =
    property $ forAll gen $ \(k, v) -> ioProperty $ do
        store <- mkStore
        mtsInsert (mtsKV store) k v
        mp <- mtsMkProof (mtsTree store) k
        case mp of
            Nothing -> pure False
            Just (_, proof) ->
                mtsVerifyProof (mtsTree store) v proof

-- | Insert N pairs, all verify.
propMultipleInsertAllVerify
    :: ( Show (MtsKey imp)
       , Show (MtsValue imp)
       )
    => IO (MerkleTreeStore 'Full imp IO)
    -> Gen [(MtsKey imp, MtsValue imp)]
    -> Property
propMultipleInsertAllVerify mkStore gen =
    property $ forAll gen $ \kvs -> ioProperty $ do
        store <- mkStore
        mapM_ (uncurry $ mtsInsert (mtsKV store)) kvs
        results <-
            mapM
                ( \(k, v) -> do
                    mp <- mtsMkProof (mtsTree store) k
                    case mp of
                        Nothing -> pure False
                        Just (_, proof) ->
                            mtsVerifyProof (mtsTree store) v proof
                )
                kvs
        pure $ and results

-- | Same keys in any order produce the same root hash.
propInsertionOrderIndependence
    :: ( Show (MtsKey imp)
       , Show (MtsValue imp)
       , Eq (MtsHash imp)
       , Ord (MtsKey imp)
       )
    => IO (MerkleTreeStore 'Full imp IO)
    -> IO (MerkleTreeStore 'Full imp IO)
    -> Gen [(MtsKey imp, MtsValue imp)]
    -> Property
propInsertionOrderIndependence mkStore1 mkStore2 gen =
    property $ forAll gen $ \kvs -> ioProperty $ do
        let sorted = sortBy (comparing fst) kvs
            reversed' = reverse sorted
        store1 <- mkStore1
        mapM_ (uncurry $ mtsInsert (mtsKV store1)) sorted
        h1 <- mtsRootHash (mtsTree store1)
        store2 <- mkStore2
        mapM_ (uncurry $ mtsInsert (mtsKV store2)) reversed'
        h2 <- mtsRootHash (mtsTree store2)
        pure $ h1 == h2

-- | Insert k v, delete k, verify k v returns False.
propDeleteRemovesKey
    :: ( Show (MtsKey imp)
       , Show (MtsValue imp)
       )
    => IO (MerkleTreeStore 'Full imp IO)
    -> Gen (MtsKey imp, MtsValue imp)
    -> Property
propDeleteRemovesKey mkStore gen =
    property $ forAll gen $ \(k, v) -> ioProperty $ do
        store <- mkStore
        mtsInsert (mtsKV store) k v
        mtsDelete (mtsKV store) k
        mp <- mtsMkProof (mtsTree store) k
        case mp of
            Nothing -> pure True
            Just (_, proof) ->
                not <$> mtsVerifyProof (mtsTree store) v proof

-- | Insert 3 distinct keys, delete one, other two still verify.
propDeletePreservesSiblings
    :: ( Show (MtsKey imp)
       , Show (MtsValue imp)
       , Eq (MtsKey imp)
       )
    => IO (MerkleTreeStore 'Full imp IO)
    -> Gen (MtsKey imp, MtsValue imp)
    -> Gen (MtsKey imp, MtsValue imp)
    -> Gen (MtsKey imp, MtsValue imp)
    -> Property
propDeletePreservesSiblings mkStore gen1 gen2 gen3 =
    property
        $ forAll gen1
        $ \(k1, v1) ->
            forAll gen2 $ \(k2, v2) ->
                forAll gen3 $ \(k3, v3) ->
                    k1 /= k2 && k2 /= k3 && k1 /= k3 ==>
                        ioProperty $ do
                            store <- mkStore
                            mtsInsert (mtsKV store) k1 v1
                            mtsInsert (mtsKV store) k2 v2
                            mtsInsert (mtsKV store) k3 v3
                            mtsDelete (mtsKV store) k2
                            r1 <- do
                                mp <-
                                    mtsMkProof (mtsTree store) k1
                                case mp of
                                    Nothing -> pure False
                                    Just (_, proof) ->
                                        mtsVerifyProof
                                            (mtsTree store)
                                            v1
                                            proof
                            r3 <- do
                                mp <-
                                    mtsMkProof (mtsTree store) k3
                                case mp of
                                    Nothing -> pure False
                                    Just (_, proof) ->
                                        mtsVerifyProof
                                            (mtsTree store)
                                            v3
                                            proof
                            pure $ r1 && r3

-- | Batch insert produces same root as sequential.
propBatchEqualsSequential
    :: ( Show (MtsKey imp)
       , Show (MtsValue imp)
       , Eq (MtsHash imp)
       )
    => IO (MerkleTreeStore 'Full imp IO)
    -> IO (MerkleTreeStore 'Full imp IO)
    -> Gen [(MtsKey imp, MtsValue imp)]
    -> Property
propBatchEqualsSequential mkSeqStore mkBatchStore gen =
    property $ forAll gen $ \kvs -> ioProperty $ do
        seqStore <- mkSeqStore
        mapM_ (uncurry $ mtsInsert (mtsKV seqStore)) kvs
        h1 <- mtsRootHash (mtsTree seqStore)
        batchStore <- mkBatchStore
        mtsBatchInsert (mtsTree batchStore) kvs
        h2 <- mtsRootHash (mtsTree batchStore)
        pure $ h1 == h2

-- | Insert N, delete all N, root is Nothing.
propInsertDeleteAllEmpty
    :: ( Show (MtsKey imp)
       , Show (MtsValue imp)
       )
    => IO (MerkleTreeStore 'Full imp IO)
    -> Gen [(MtsKey imp, MtsValue imp)]
    -> Property
propInsertDeleteAllEmpty mkStore gen =
    property $ forAll gen $ \kvs -> ioProperty $ do
        store <- mkStore
        mapM_ (uncurry $ mtsInsert (mtsKV store)) kvs
        mapM_ (mtsDelete (mtsKV store) . fst) kvs
        h <- mtsRootHash (mtsTree store)
        pure $ isNothing h

-- | Empty tree has no root hash.
propEmptyTreeNoRoot
    :: IO (MerkleTreeStore 'Full imp IO)
    -> Property
propEmptyTreeNoRoot mkStore = ioProperty $ do
    store <- mkStore
    h <- mtsRootHash (mtsTree store)
    pure $ isNothing h

-- | Single insert produces a root hash.
propSingleInsertHasRoot
    :: ( Show (MtsKey imp)
       , Show (MtsValue imp)
       )
    => IO (MerkleTreeStore 'Full imp IO)
    -> Gen (MtsKey imp, MtsValue imp)
    -> Property
propSingleInsertHasRoot mkStore gen =
    property $ forAll gen $ \(k, v) -> ioProperty $ do
        store <- mkStore
        mtsInsert (mtsKV store) k v
        h <- mtsRootHash (mtsTree store)
        pure $ isJust h

-- | Insert k v, verify k v' where v /= v' returns False.
propWrongValueRejects
    :: ( Show (MtsKey imp)
       , Show (MtsValue imp)
       )
    => IO (MerkleTreeStore 'Full imp IO)
    -> Gen (MtsKey imp, MtsValue imp, MtsValue imp)
    -> Property
propWrongValueRejects mkStore gen =
    property $ forAll gen $ \(k, v, v') -> ioProperty $ do
        store <- mkStore
        mtsInsert (mtsKV store) k v
        mp <- mtsMkProof (mtsTree store) k
        case mp of
            Nothing -> pure False
            Just (_, proof) ->
                not
                    <$> mtsVerifyProof (mtsTree store) v' proof

-- | The root hash returned by mtsMkProof matches mtsFoldProof
-- and mtsRootHash.
propProofAnchoredToRoot
    :: ( Show (MtsKey imp)
       , Show (MtsValue imp)
       , Eq (MtsHash imp)
       )
    => IO (MerkleTreeStore 'Full imp IO)
    -> Gen (MtsKey imp, MtsValue imp)
    -> Property
propProofAnchoredToRoot mkStore gen =
    property $ forAll gen $ \(k, v) -> ioProperty $ do
        store <- mkStore
        mtsInsert (mtsKV store) k v
        mp <- mtsMkProof (mtsTree store) k
        case mp of
            Nothing -> pure False
            Just (anchoredRoot, proof) -> do
                currentRoot <- mtsRootHash (mtsTree store)
                let foldedRoot =
                        mtsFoldProof (mtsTree store) proof
                pure
                    $ currentRoot == Just anchoredRoot
                        && foldedRoot == anchoredRoot

-- | Insert N pairs, collect leaves, generate completeness
-- proof, verify it returns True.
propCompletenessRoundTrip
    :: ( Show (MtsKey imp)
       , Show (MtsValue imp)
       )
    => IO (MerkleTreeStore 'Full imp IO)
    -> Gen [(MtsKey imp, MtsValue imp)]
    -> Property
propCompletenessRoundTrip mkStore gen =
    property $ forAll gen $ \kvs -> ioProperty $ do
        store <- mkStore
        mapM_ (uncurry $ mtsInsert (mtsKV store)) kvs
        leaves <- mtsCollectLeaves (mtsTree store)
        mp <- mtsMkCompletenessProof (mtsTree store)
        case mp of
            Nothing -> pure False
            Just proof ->
                mtsVerifyCompletenessProof
                    (mtsTree store)
                    leaves
                    proof

-- | Empty tree has no completeness proof.
propCompletenessEmpty
    :: IO (MerkleTreeStore 'Full imp IO)
    -> Property
propCompletenessEmpty mkStore = ioProperty $ do
    store <- mkStore
    mp <- mtsMkCompletenessProof (mtsTree store)
    pure $ case mp of
        Nothing -> True
        Just _ -> False

-- | Insert N, delete some, completeness proof still
-- verifies for remaining tree.
propCompletenessAfterDelete
    :: ( Show (MtsKey imp)
       , Show (MtsValue imp)
       )
    => IO (MerkleTreeStore 'Full imp IO)
    -> Gen [(MtsKey imp, MtsValue imp)]
    -> Property
propCompletenessAfterDelete mkStore gen =
    property $ forAll gen $ \kvs -> ioProperty $ do
        store <- mkStore
        mapM_ (uncurry $ mtsInsert (mtsKV store)) kvs
        let toDelete = take (length kvs `div` 2) kvs
        mapM_ (mtsDelete (mtsKV store) . fst) toDelete
        root' <- mtsRootHash (mtsTree store)
        case root' of
            Nothing -> pure True
            Just _ -> do
                leaves <- mtsCollectLeaves (mtsTree store)
                mp <- mtsMkCompletenessProof (mtsTree store)
                case mp of
                    Nothing -> pure False
                    Just proof ->
                        mtsVerifyCompletenessProof
                            (mtsTree store)
                            leaves
                            proof

-- ------------------------------------------------------------------
-- Replay properties
-- ------------------------------------------------------------------

-- | Insert N pairs via KVOnly, replay journal, root hash
-- matches Full-mode store with same inserts.
propKVOnlyThenReplayMatchesFull
    :: ( Show (MtsKey imp)
       , Show (MtsValue imp)
       , Eq (MtsHash imp)
       )
    => IO
        ( MerkleTreeStore 'KVOnly imp IO
        , IO ()
        , IO (MerkleTreeStore 'Full imp IO)
        )
    -> IO (MerkleTreeStore 'Full imp IO)
    -> Gen [(MtsKey imp, MtsValue imp)]
    -> Property
propKVOnlyThenReplayMatchesFull mkReplayEnv mkRefStore gen =
    property $ forAll gen $ \kvs -> ioProperty $ do
        (kvStore, replay, mkFull) <- mkReplayEnv
        mapM_
            (uncurry $ mtsInsert (mtsKV kvStore))
            kvs
        replay
        fullStore <- mkFull
        h1 <- mtsRootHash (mtsTree fullStore)
        refStore <- mkRefStore
        mapM_
            (uncurry $ mtsInsert (mtsKV refStore))
            kvs
        h2 <- mtsRootHash (mtsTree refStore)
        pure $ h1 == h2

-- | After KVOnly inserts and replay, all keys have valid
-- proofs.
propKVOnlyThenReplayProofsWork
    :: ( Show (MtsKey imp)
       , Show (MtsValue imp)
       )
    => IO
        ( MerkleTreeStore 'KVOnly imp IO
        , IO ()
        , IO (MerkleTreeStore 'Full imp IO)
        )
    -> Gen [(MtsKey imp, MtsValue imp)]
    -> Property
propKVOnlyThenReplayProofsWork mkReplayEnv gen =
    property $ forAll gen $ \kvs -> ioProperty $ do
        (kvStore, replay, mkFull) <- mkReplayEnv
        mapM_
            (uncurry $ mtsInsert (mtsKV kvStore))
            kvs
        replay
        fullStore <- mkFull
        results <-
            mapM
                ( \(k, v) -> do
                    mp <- mtsMkProof (mtsTree fullStore) k
                    case mp of
                        Nothing -> pure False
                        Just (_, proof) ->
                            mtsVerifyProof
                                (mtsTree fullStore)
                                v
                                proof
                )
                kvs
        pure $ and results

-- | Insert + delete some via KVOnly, replay, surviving keys
-- have valid proofs and deleted keys do not.
--
-- Note: we verify via proofs rather than root hash comparison
-- because journal compression (last-write-wins) means replay
-- may build the tree from scratch rather than replaying the
-- full insert+delete history. For implementations where
-- deletion does not fully normalize branch structure (e.g.
-- MPF), this can produce a structurally different but
-- semantically correct tree.
propKVOnlyDeleteThenReplay
    :: ( Show (MtsKey imp)
       , Show (MtsValue imp)
       )
    => IO
        ( MerkleTreeStore 'KVOnly imp IO
        , IO ()
        , IO (MerkleTreeStore 'Full imp IO)
        )
    -> Gen [(MtsKey imp, MtsValue imp)]
    -> Property
propKVOnlyDeleteThenReplay mkReplayEnv gen =
    property $ forAll gen $ \kvs -> ioProperty $ do
        (kvStore, replay, mkFull) <- mkReplayEnv
        mapM_
            (uncurry $ mtsInsert (mtsKV kvStore))
            kvs
        let toDelete = take (length kvs `div` 2) kvs
            surviving = drop (length kvs `div` 2) kvs
        mapM_
            (mtsDelete (mtsKV kvStore) . fst)
            toDelete
        replay
        fullStore <- mkFull
        -- Surviving keys must have valid proofs
        okSurviving <-
            mapM
                ( \(k, v) -> do
                    mp <- mtsMkProof (mtsTree fullStore) k
                    case mp of
                        Nothing -> pure False
                        Just (_, proof) ->
                            mtsVerifyProof
                                (mtsTree fullStore)
                                v
                                proof
                )
                surviving
        -- Deleted keys must not verify
        okDeleted <-
            mapM
                ( \(k, v) -> do
                    mp <- mtsMkProof (mtsTree fullStore) k
                    case mp of
                        Nothing -> pure True
                        Just (_, proof) ->
                            not
                                <$> mtsVerifyProof
                                    (mtsTree fullStore)
                                    v
                                    proof
                )
                toDelete
        pure $ and okSurviving && and okDeleted

-- | Replaying an empty journal is a no-op.
propReplayIdempotent
    :: IO
        ( MerkleTreeStore 'KVOnly imp IO
        , IO ()
        , IO (MerkleTreeStore 'Full imp IO)
        )
    -> Property
propReplayIdempotent mkReplayEnv = ioProperty $ do
    (_, replay, mkFull) <- mkReplayEnv
    replay
    fullStore <- mkFull
    h <- mtsRootHash (mtsTree fullStore)
    pure $ isNothing h

-- | Insert k v then delete k via KVOnly; after replay the
-- tree is empty. Verifies that conflicting operations on
-- the same key are handled correctly by the journal.
propJournalCompression
    :: ( Show (MtsKey imp)
       , Show (MtsValue imp)
       )
    => IO
        ( MerkleTreeStore 'KVOnly imp IO
        , IO ()
        , IO (MerkleTreeStore 'Full imp IO)
        )
    -> Gen (MtsKey imp, MtsValue imp)
    -> Property
propJournalCompression mkReplayEnv gen =
    property $ forAll gen $ \(k, v) -> ioProperty $ do
        (kvStore, replay, mkFull) <- mkReplayEnv
        mtsInsert (mtsKV kvStore) k v
        mtsDelete (mtsKV kvStore) k
        replay
        fullStore <- mkFull
        h <- mtsRootHash (mtsTree fullStore)
        pure $ isNothing h

-- ------------------------------------------------------------------
-- Trace properties
-- ------------------------------------------------------------------

-- | After inserting N unique keys via KVOnly and replaying,
-- the 'ReplayStart' events have monotonically decreasing
-- 'rsEntriesRemaining' ending at 0.
propReplayTraceMonotonic
    :: ( Show (MtsKey imp)
       , Show (MtsValue imp)
       )
    => IO
        ( MerkleTreeStore 'KVOnly imp IO
        , IO [ReplayEvent]
        , IO (MerkleTreeStore 'Full imp IO)
        )
    -> Gen [(MtsKey imp, MtsValue imp)]
    -> Property
propReplayTraceMonotonic mkReplayEnv genPairs =
    forAll genPairs $ \pairs ->
        not (null pairs) ==>
            ioProperty $ do
                (kvStore, replay, _) <- mkReplayEnv
                mapM_
                    (uncurry $ mtsInsert (mtsKV kvStore))
                    pairs
                events <- replay
                let starts =
                        [ rsEntriesRemaining
                        | ReplayStart{rsEntriesRemaining} <-
                            events
                        ]
                    monotonic xs = and $ zipWith (>=) xs (drop 1 xs)
                pure
                    $ not (null starts)
                        && monotonic starts
                        && last starts == 0

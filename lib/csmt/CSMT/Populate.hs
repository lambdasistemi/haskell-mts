-- |
-- Module      : CSMT.Populate
-- Description : Parallel CSMT patching from a stream of operations
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- Patches a CSMT in parallel by bucketing operations by tree key
-- prefix and applying them in subtrees concurrently. Works on both
-- empty and non-empty trees. Supports both inserts and deletes.
--
-- The caller provides operations via a callback. The library handles
-- tree preparation, bucketing, parallel consumers with batched
-- transactions, and final merge of the top levels.
module CSMT.Populate
    ( patchParallel
    , PatchOp (..)
    )
where

import CSMT.Deletion (deletingDirect)
import CSMT.Insertion
    ( allPrefixes
    , bucketIndex
    , expandToBucketDepth
    , insertingDirect
    , mergeSubtreeRoots
    )
import CSMT.Interface
    ( Hashing
    , Indirect
    , Key
    )
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM
    ( atomically
    , newTBQueueIO
    , readTBQueue
    , writeTBQueue
    )
import Control.Monad (forM, forM_)
import Database.KV.Transaction
    ( GCompare
    , Selector
    , Transaction
    )
import Numeric.Natural (Natural)

-- | An operation to apply to the tree.
data PatchOp key value
    = -- | Insert or update a key-value pair.
      PatchInsert key value
    | -- | Delete a key.
      PatchDelete key
    deriving stock (Eq, Show)

-- |
-- Patch a CSMT in parallel.
--
-- Spawns @2^bucketBits@ consumer threads, each applying
-- operations to its subtree in batched transactions. The
-- caller produces operations via the @producer@ callback.
-- When the producer returns, the library drains all consumers
-- and merges the top levels.
--
-- The @runTx@ callback must support concurrent calls from
-- different threads (e.g. 'runTransactionUnguarded'). Each
-- consumer writes to a disjoint key prefix, so no locking is
-- needed.
patchParallel
    :: (GCompare d)
    => Int
    -- ^ Bucket bits (e.g. 4 → 16 buckets)
    -> Int
    -- ^ Batch size per transaction (e.g. 1000)
    -> Natural
    -- ^ TBQueue bound per bucket (backpressure)
    -> Key
    -- ^ Global prefix (usually @[]@)
    -> Hashing a
    -> Selector d Key (Indirect a)
    -> (forall b. Transaction IO cf d ops b -> IO b)
    -- ^ Run a transaction (must be thread-safe)
    -> ((PatchOp Key a -> IO ()) -> IO ())
    -- ^ Producer: given a feed function, emit all
    -- operations. When this returns, the stream is over.
    -> IO ()
patchParallel bucketBits batchSize queueBound pfx hashing csmtCol runTx producer = do
    let prefixes = allPrefixes bucketBits

    -- Expand existing tree so no jump crosses bucket boundary
    runTx $ expandToBucketDepth pfx bucketBits csmtCol

    -- Create one TBQueue per bucket
    queues <- forM prefixes $ \_ -> newTBQueueIO queueBound

    -- Spawn all consumer threads
    asyncs <- forM (zip prefixes queues) $ \(bpfx, q) ->
        async $ consumeQueue (pfx <> bpfx) q

    -- Producer runs in the current thread
    producer $ \op -> do
        let treeKey = opKey op
            (bucket, stripped) = splitAt bucketBits treeKey
            idx = bucketIndex bucket
        atomically
            $ writeTBQueue (queues !! idx) (Just (setOpKey stripped op))

    -- Signal end-of-stream to all queues
    forM_ queues $ \q -> atomically $ writeTBQueue q Nothing

    -- Wait for all consumers to finish
    mapM_ wait asyncs

    -- All subtrees built — merge top levels
    runTx $ mergeSubtreeRoots pfx hashing csmtCol bucketBits
  where
    consumeQueue bpfx q = loop []
      where
        loop batch = do
            mEntry <- atomically $ readTBQueue q
            case mEntry of
                Nothing ->
                    flushBatch bpfx batch
                Just entry -> do
                    let batch' = entry : batch
                    if length batch' >= batchSize
                        then do
                            flushBatch bpfx batch'
                            loop []
                        else loop batch'

    flushBatch _ [] = pure ()
    flushBatch bpfx batch =
        runTx $ mapM_ (applyOp bpfx) batch

    applyOp bpfx (PatchInsert k v) =
        insertingDirect bpfx hashing csmtCol k v
    applyOp bpfx (PatchDelete k) =
        deletingDirect bpfx hashing csmtCol k

-- | Extract the tree key from an operation.
opKey :: PatchOp Key a -> Key
opKey (PatchInsert k _) = k
opKey (PatchDelete k) = k

-- | Replace the key in an operation.
setOpKey :: Key -> PatchOp Key a -> PatchOp Key a
setOpKey k (PatchInsert _ v) = PatchInsert k v
setOpKey k (PatchDelete _) = PatchDelete k

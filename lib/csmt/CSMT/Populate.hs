-- |
-- Module      : CSMT.Populate
-- Description : Parallel CSMT population from a stream of entries
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- Populates an empty CSMT in parallel by bucketing entries by
-- tree key prefix and building subtrees concurrently.
--
-- The caller provides entries via a callback. The library handles
-- bucketing, parallel consumers with batched transactions,
-- and final merge of the top levels.
module CSMT.Populate
    ( populateCSMT
    )
where

import CSMT.Insertion
    ( allPrefixes
    , bucketIndex
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

-- |
-- Populate an empty CSMT in parallel.
--
-- Spawns @2^bucketBits@ consumer threads, each building its
-- subtree in batched transactions. The caller produces entries
-- via the @producer@ callback. When the producer returns, the
-- library drains all consumers and merges the top levels.
--
-- The @runTx@ callback must support concurrent calls from
-- different threads (e.g. 'runTransactionUnguarded'). Each
-- consumer writes to a disjoint key prefix, so no locking is
-- needed.
populateCSMT
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
    -> ((Key -> a -> IO ()) -> IO ())
    -- ^ Producer: given a feed function, emit all
    -- @(treeKey, hash)@ pairs. When this returns,
    -- the stream is over.
    -> IO ()
populateCSMT bucketBits batchSize queueBound pfx hashing csmtCol runTx producer = do
    let prefixes = allPrefixes bucketBits

    -- Create one TBQueue per bucket
    queues <- forM prefixes $ \_ -> newTBQueueIO queueBound

    -- Spawn all consumer threads
    asyncs <- forM (zip prefixes queues) $ \(bpfx, q) ->
        async $ consumeQueue (pfx <> bpfx) q

    -- Producer runs in the current thread
    producer $ \treeKey hash -> do
        let (bucket, stripped) = splitAt bucketBits treeKey
            idx = bucketIndex bucket
        atomically
            $ writeTBQueue (queues !! idx) (Just (stripped, hash))

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
                    -- Flush remaining batch
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
        runTx
            $ mapM_
                (\(k, h) -> insertingDirect bpfx hashing csmtCol k h)
                batch

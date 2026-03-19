-- | Transaction-level rollback operations.
--
-- All functions take a column selector as their
-- first argument. Standalone callers pass
-- 'RollbackPoints'; downstream consumers embedding
-- the rollback column in a larger GADT pass their
-- own constructor (e.g. @CageRollbacks@).
--
-- The library stores and retrieves inverse
-- operations but does not know how to /apply/
-- them. Rollback functions take a callback
-- for inverse application.
--
-- The @key@ parameter is the column key type.
-- Downstream chooses it (e.g. @WithOrigin slot@,
-- @Maybe slot@). The library only requires
-- @Ord key@.
--
-- == Counted variants
--
-- Functions prefixed with @counted@ maintain a
-- persistent counter that tracks the number of
-- rollback points. The counter is updated
-- atomically in the same transaction. Use
-- 'readCount' to query it.
module MTS.Rollbacks.Store
    ( -- * Forward (store rollback point)
      storeRollbackPoint

      -- * Tip query
    , queryTip

      -- * Rollback
    , RollbackResult (..)
    , rollbackTo

      -- * Finality (prune old points)
    , pruneBelow
    , pruneExcess

      -- * Armageddon (full cleanup)
    , armageddonCleanup
    , armageddonSetup

      -- * Inspection
    , countPoints

      -- * Persistent counter
    , RollbackCounter (..)
    , readCount
    , countedStore
    , countedRollbackTo
    , countedPruneBelow
    , countedArmageddonCleanup
    , countedArmageddonSetup
    )
where

import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString)
import Data.Function (fix)
import Data.Maybe (fromMaybe)
import Database.KV.Cursor
    ( Entry (..)
    , firstEntry
    , lastEntry
    , nextEntry
    , prevEntry
    , seekKey
    )
import Database.KV.Transaction
    ( GCompare
    , Selector
    , Transaction
    , delete
    , insert
    , iterating
    , query
    )
import MTS.Rollbacks.Column
    ( RollbackCol
    )
import MTS.Rollbacks.Types
    ( RollbackPoint (..)
    )

-- | Store a rollback point at the given key.
--
-- Call this during forward-tip processing after
-- computing inverse operations.
storeRollbackPoint
    :: (Ord key, GCompare t)
    => RollbackCol t key inv meta
    -- ^ Column selector
    -> key
    -- ^ Key for the rollback point
    -> RollbackPoint inv meta
    -- ^ Inverses and metadata
    -> Transaction m cf t op ()
storeRollbackPoint = insert

-- | Query the current tip (last key).
--
-- Returns 'Nothing' if no rollback points exist
-- (database not initialized).
queryTip
    :: (Monad m, GCompare t)
    => RollbackCol t key inv meta
    -- ^ Column selector
    -> Transaction m cf t op (Maybe key)
queryTip col =
    iterating col $ do
        fmap entryKey <$> lastEntry

-- | Result of a rollback attempt.
data RollbackResult
    = -- | Rollback succeeded. The 'Int' is the
      -- number of points deleted.
      RollbackSucceeded Int
    | -- | Target key not found. Database must
      -- be truncated (armageddon).
      RollbackImpossible
    deriving stock (Eq, Show)

-- | Roll back to the given key.
--
-- Iterates backward from the tip, calling the
-- provided callback for each rollback point
-- strictly after the target. Points after the
-- target are deleted; the target's point is kept.
--
-- The callback receives each 'RollbackPoint' in
-- reverse chronological order (most recent first).
rollbackTo
    :: (Ord key, Monad m, GCompare t)
    => RollbackCol t key inv meta
    -- ^ Column selector
    -> ( RollbackPoint inv meta
         -> Transaction m cf t op ()
       )
    -- ^ Apply inverses from one rollback point
    -> key
    -- ^ Target key to roll back to
    -> Transaction m cf t op RollbackResult
rollbackTo col applyInverses targetKey =
    iterating col $ do
        mTarget <- seekKey targetKey
        case mTarget of
            Nothing -> pure RollbackImpossible
            Just Entry{entryKey}
                | entryKey /= targetKey ->
                    pure RollbackImpossible
                | otherwise -> do
                    -- Target found, now walk from tip
                    -- backward deleting everything
                    -- strictly after target
                    ml <- lastEntry
                    n <- walkBack ml
                    pure (RollbackSucceeded n)
  where
    walkBack cur =
        ($ cur) $ fix $ \go -> \case
            Nothing -> pure 0
            Just Entry{entryKey, entryValue}
                | entryKey > targetKey -> do
                    lift
                        $ applyInverses entryValue
                    lift
                        $ delete col entryKey
                    prev <- prevEntry
                    (+ 1) <$> go prev
                | otherwise -> pure 0

-- | Prune all rollback points strictly before
-- the given key. Returns the number of points
-- pruned.
pruneBelow
    :: (Ord key, Monad m, GCompare t)
    => RollbackCol t key inv meta
    -- ^ Column selector
    -> key
    -- ^ Lower bound (exclusive)
    -> Transaction m cf t op Int
pruneBelow col k =
    iterating col $ do
        me <- firstEntry
        ($ me) $ fix $ \go -> \case
            Nothing -> pure 0
            Just Entry{entryKey}
                | entryKey < k -> do
                    lift
                        $ delete col entryKey
                    next <- nextEntry
                    (+ 1) <$> go next
                | otherwise -> pure 0

-- | Keep at most @maxToKeep@ rollback points,
-- deleting the oldest excess. Returns the number
-- of points pruned.
pruneExcess
    :: (Ord key, Monad m, GCompare t)
    => RollbackCol t key inv meta
    -- ^ Column selector
    -> Int
    -- ^ Maximum number of points to keep
    -> Transaction m cf t op Int
pruneExcess col maxToKeep = do
    total <- countPoints col
    let toDelete = max 0 (total - maxToKeep)
    if toDelete <= 0
        then pure 0
        else iterating col $ do
            me <- firstEntry
            ($ (me, toDelete)) $ fix $ \go -> \case
                (_, 0) -> pure 0
                (Nothing, _) -> pure 0
                (Just Entry{entryKey}, remaining) -> do
                    lift
                        $ delete col entryKey
                    next <- nextEntry
                    (+ 1) <$> go (next, remaining - 1)

-- | Delete rollback points in a batch. Returns
-- 'True' if more entries remain (caller should
-- loop).
--
-- This is for armageddon (full DB reset) when
-- rollback is impossible. Run in a loop with
-- a transaction runner.
armageddonCleanup
    :: (Ord key, Monad m, GCompare t)
    => RollbackCol t key inv meta
    -- ^ Column selector
    -> Int
    -- ^ Batch size (entries per transaction)
    -> Transaction m cf t op Bool
armageddonCleanup col batchSz =
    iterating col $ do
        me <- firstEntry
        ($ (me, 0 :: Int)) $ fix $ \go -> \case
            (Nothing, _) -> pure False
            (_, n) | n >= batchSz -> pure True
            (Just Entry{entryKey}, n) -> do
                lift
                    $ delete col entryKey
                next <- nextEntry
                go (next, n + 1)

-- | Initialize the rollback column with a
-- sentinel point carrying empty inverses.
--
-- Call after 'armageddonCleanup' completes, or
-- on fresh database setup.
armageddonSetup
    :: (Ord key, GCompare t)
    => RollbackCol t key inv meta
    -- ^ Column selector
    -> key
    -- ^ Sentinel key (e.g. Origin, Nothing)
    -> Maybe meta
    -- ^ Optional metadata for the sentinel
    -> Transaction m cf t op ()
armageddonSetup col sentinel meta =
    insert col sentinel
        $ RollbackPoint
            { rpInverses = []
            , rpMeta = meta
            }

-- | Count total rollback points.
countPoints
    :: (Monad m, GCompare t)
    => RollbackCol t key inv meta
    -- ^ Column selector
    -> Transaction m cf t op Int
countPoints col =
    iterating col $ do
        me <- firstEntry
        ($ me) $ fix $ \go -> \case
            Nothing -> pure 0
            Just _ -> do
                next <- nextEntry
                (+ 1) <$> go next

-- ------------------------------------------------------------------
-- Persistent counter
-- ------------------------------------------------------------------

-- | A persistent counter for rollback points,
-- stored in a metrics column. The counter is
-- updated atomically in the same transaction
-- as the rollback point operations.
data RollbackCounter t = RollbackCounter
    { rcSelector :: Selector t ByteString Int
    -- ^ Metrics column selector
    , rcKey :: ByteString
    -- ^ Counter key within the metrics column
    }

-- | Read the current rollback point count.
readCount
    :: (Monad m, GCompare t)
    => RollbackCounter t
    -> Transaction m cf t op Int
readCount RollbackCounter{rcSelector, rcKey} =
    fromMaybe 0 <$> query rcSelector rcKey

-- | Adjust the counter by a delta.
adjustCount
    :: (Monad m, GCompare t)
    => RollbackCounter t
    -> Int
    -> Transaction m cf t op ()
adjustCount rc delta = do
    current <- readCount rc
    insert (rcSelector rc) (rcKey rc) (current + delta)

-- | Store a rollback point and increment the
-- counter.
countedStore
    :: (Ord key, Monad m, GCompare t)
    => RollbackCol t key inv meta
    -> RollbackCounter t
    -> key
    -> RollbackPoint inv meta
    -> Transaction m cf t op ()
countedStore col rc key rp = do
    storeRollbackPoint col key rp
    adjustCount rc 1

-- | Roll back and decrement the counter by the
-- number of deleted points.
countedRollbackTo
    :: (Ord key, Monad m, GCompare t)
    => RollbackCol t key inv meta
    -> RollbackCounter t
    -> ( RollbackPoint inv meta
         -> Transaction m cf t op ()
       )
    -> key
    -> Transaction m cf t op RollbackResult
countedRollbackTo col rc applyInverses key = do
    result <- rollbackTo col applyInverses key
    case result of
        RollbackSucceeded n ->
            adjustCount rc (negate n)
        RollbackImpossible -> pure ()
    pure result

-- | Prune and decrement the counter by the
-- number of pruned points.
countedPruneBelow
    :: (Ord key, Monad m, GCompare t)
    => RollbackCol t key inv meta
    -> RollbackCounter t
    -> key
    -> Transaction m cf t op Int
countedPruneBelow col rc key = do
    n <- pruneBelow col key
    adjustCount rc (negate n)
    pure n

-- | Armageddon cleanup. Resets the counter to 0
-- on the last batch (when no more entries remain).
countedArmageddonCleanup
    :: (Ord key, Monad m, GCompare t)
    => RollbackCol t key inv meta
    -> RollbackCounter t
    -> Int
    -> Transaction m cf t op Bool
countedArmageddonCleanup col rc batchSz = do
    more <- armageddonCleanup col batchSz
    if more
        then pure True
        else do
            insert (rcSelector rc) (rcKey rc) 0
            pure False

-- | Armageddon setup. Sets the counter to 1
-- (the sentinel point).
countedArmageddonSetup
    :: (Ord key, Monad m, GCompare t)
    => RollbackCol t key inv meta
    -> RollbackCounter t
    -> key
    -> Maybe meta
    -> Transaction m cf t op ()
countedArmageddonSetup col rc key meta = do
    armageddonSetup col key meta
    insert (rcSelector rc) (rcKey rc) 1

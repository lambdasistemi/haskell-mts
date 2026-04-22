-- |
-- Module      : CSMT.Populate
-- Description : Parallel CSMT patching via bucketed transactions
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- Patches a CSMT by bucketing operations by tree key prefix and
-- returning independent transactions for each bucket. The caller
-- is responsible for running these transactions concurrently
-- (e.g. via 'mapConcurrently_').
--
-- Supports both inserts and deletes. Works on both empty and
-- non-empty trees.
module CSMT.Populate
    ( patchParallel
    , PatchOp (..)
    , expandToBucketDepth
    , mergeSubtreeRoots
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
import Data.List (foldl')
import Data.Map.Strict qualified as Map
import Database.KV.Transaction
    ( GCompare
    , Selector
    , Transaction
    , delete
    )

-- | An operation to apply to the tree.
data PatchOp key value
    = -- | Insert or update a key-value pair.
      PatchInsert key value
    | -- | Delete a key.
      PatchDelete key
    deriving stock (Eq, Show)

-- |
-- Build independent bucket transactions from a batch of
-- operations.
--
-- Groups operations by tree key bucket prefix and returns one
-- 'Transaction' per non-empty bucket. Each transaction applies
-- tree operations for its bucket and deletes the corresponding
-- journal entries.
--
-- The caller must run 'expandToBucketDepth' before and
-- 'mergeSubtreeRoots' after these transactions. The returned
-- transactions are independent and can be run concurrently.
--
-- @journalCol@ is optional: pass journal keys alongside
-- operations to have them deleted atomically with the tree
-- updates. When used for initial population (no journal),
-- pass entries with dummy journal keys and a no-op column.
patchParallel
    :: (GCompare d, Ord jk, Monad m)
    => Int
    -- ^ Bucket bits (e.g. 4 -> 16 buckets)
    -> Key
    -- ^ Global prefix (usually @[]@)
    -> Hashing a
    -> Selector d Key (Indirect a)
    -- ^ CSMT column
    -> Selector d jk v
    -- ^ Journal column (for deleting replayed entries)
    -> [(jk, PatchOp Key a)]
    -- ^ (journal key, tree operation) pairs
    -> [(Int, Transaction m cf d ops ())]
    -- ^ (op count, transaction) per active bucket
patchParallel bucketBits pfx hashing csmtCol journalCol entries =
    map mkBucketTx (Map.toList buckets)
  where
    prefixes = allPrefixes bucketBits

    -- Group entries by bucket index
    buckets =
        foldl' addEntry Map.empty entries

    addEntry m (jk, op) =
        let treeKey = opKey op
            (bucket, stripped) = splitAt bucketBits treeKey
            idx = bucketIndex bucket
            op' = setOpKey stripped op
        in  Map.insertWith
                (++)
                idx
                [(jk, op')]
                m

    -- Build a transaction for one bucket
    mkBucketTx (idx, ops) =
        let bpfx = pfx <> (prefixes !! idx)
        in  ( length ops
            , do
                mapM_ (applyOp bpfx . snd) ops
                mapM_ (delete journalCol . fst) ops
            )

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

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : CSMT.Insertion
-- Description : Insertion algorithm for Compact Sparse Merkle Trees
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- This module implements the core insertion algorithm for CSMTs.
--
-- The insertion process:
--
-- 1. Build a 'Compose' tree representing the structural changes needed
-- 2. Scan the compose tree to compute hashes and generate database operations
-- 3. Apply all operations atomically
--
-- The 'Compose' type represents a pending tree modification before hashes
-- are computed, allowing for efficient batch processing of changes.
--
-- == Batch Operations
--
-- For bulk loading into an empty tree, use 'insertingBatch' which
-- builds the tree in one pure pass using divide-and-conquer:
-- O(n log n) vs O(n²) for sequential inserts.
--
-- For parallel population, use 'insertingBucketed' which splits
-- the key space by the first @n@ bits, builds each subtree
-- independently, then merges the top levels. The subtree
-- construction is pure and can be parallelised by the caller.
module CSMT.Insertion
    ( -- * Single insertion
      inserting
    , insertingTreeOnly

      -- * Batch insertion
    , insertingBatch
    , insertingBucketed

      -- * Internal (for testing)
    , buildComposeTree
    , buildComposeFromList
    , buildBucket
    , mergeComposeForest
    , scanCompose
    , Compose (..)
    )
where

import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import CSMT.Interface
    ( Direction (..)
    , FromKV (..)
    , Hashing (..)
    , Indirect (..)
    , Key
    , compareKeys
    , oppositeDirection
    )
import Control.Lens (view)
import Database.KV.Transaction
    ( GCompare
    , Selector
    , Transaction
    , insert
    , query
    )

-- |
-- A binary tree structure representing pending modifications.
--
-- * 'Compose' - An internal node with a jump key and two children
-- * 'Leaf' - A leaf node containing an indirect reference
--
-- This structure captures the shape of changes before hashes are computed,
-- allowing the insertion algorithm to build up the tree structure first
-- and then compute all hashes in a single pass.
data Compose a
    = -- | Internal node with jump path and left/right children
      Compose Key (Compose a) (Compose a)
    | -- | Leaf node with indirect value
      Leaf (Indirect a)
    deriving (Show, Eq)

-- | Construct a Compose node with children ordered by direction.
compose :: Direction -> Key -> Compose a -> Compose a -> Compose a
compose L j left right = Compose j left right
compose R j left right = Compose j right left

-- |
-- Insert a key-value pair into the CSMT.
--
-- This function:
--
-- 1. Stores the original key-value pair in the KV store
-- 2. Builds a Compose tree representing the structural changes
-- 3. Computes hashes and applies all CSMT updates atomically
inserting
    :: (Monad m, Ord k, GCompare d)
    => Key
    -- ^ Prefix (use @[]@ for root)
    -> FromKV k v a
    -> Hashing a
    -> Selector d k v
    -> Selector d Key (Indirect a)
    -> k
    -> v
    -> Transaction m cf d ops ()
inserting pfx FromKV{isoK, fromV, treePrefix} hashing kVCol csmtCol k v = do
    insert kVCol k v
    let treeKey = treePrefix v <> view isoK k
    c <- buildComposeTree csmtCol pfx treeKey (fromV v)
    mapM_ (uncurry $ insert csmtCol) $ snd $ scanCompose pfx hashing c

-- | Insert into the tree column only (no KV write).
-- Used during journal replay when KV is already up to date.
insertingTreeOnly
    :: (Monad m, GCompare d)
    => Key
    -- ^ Prefix (use @[]@ for root)
    -> FromKV k v a
    -> Hashing a
    -> Selector d Key (Indirect a)
    -> k
    -> v
    -> Transaction m cf d ops ()
insertingTreeOnly pfx FromKV{isoK, fromV, treePrefix} hashing csmtCol k v = do
    let treeKey = treePrefix v <> view isoK k
    c <- buildComposeTree csmtCol pfx treeKey (fromV v)
    mapM_ (uncurry $ insert csmtCol) $ snd $ scanCompose pfx hashing c

-- |
-- Scan a Compose tree bottom-up, computing hashes and collecting database operations.
--
-- Returns the root indirect value and a list of (key, value) pairs to insert.
-- Hashes are computed by combining child hashes at each internal node.
scanCompose
    :: Key
    -- ^ Prefix (use @[]@ for root)
    -> Hashing a
    -> Compose a
    -> (Indirect a, [(Key, Indirect a)])
scanCompose pfx Hashing{combineHash} = go pfx
  where
    go k (Leaf i) = (i, [(k, i)])
    go k (Compose jump left right) =
        let k' = k <> jump
            (hl, ls) = go (k' <> [L]) left
            (hr, rs) = go (k' <> [R]) right
            value = combineHash hl hr
            i = Indirect{jump, value}
        in  (i, ls <> rs <> [(k, i)])

-- |
-- Build a Compose tree for inserting a value at the given key.
--
-- Traverses the existing tree structure to find where the new value
-- should be inserted, handling:
--
-- * Empty slots - create a new leaf
-- * Existing values - split nodes as needed
-- * Path compression - maintain compact representation
buildComposeTree
    :: forall a d ops cf m
     . (Monad m, GCompare d)
    => Selector d Key (Indirect a)
    -> Key
    -- ^ Prefix (use @[]@ for root)
    -> Key
    -> a
    -> Transaction m cf d ops (Compose a)
buildComposeTree csmtCol pfx key h = go key pfx pure
  where
    go [] _ cont = cont $ Leaf $ Indirect [] h
    go target current cont = do
        mi <- query csmtCol current
        case mi of
            Nothing -> cont $ Leaf $ Indirect target h
            Just Indirect{jump, value} -> do
                let (common, other, us) = compareKeys jump target
                case (other, us) of
                    ([], []) -> cont $ Leaf $ Indirect common h
                    ([], z : zs) -> do
                        mov <- query csmtCol (current <> common <> [oppositeDirection z])
                        case mov of
                            Nothing -> error "a jump pointed to a non-existing node"
                            Just i ->
                                go zs (current <> common <> [z]) $ \c ->
                                    cont $ compose z common c $ Leaf i
                    (_ : os, z : zs) ->
                        go zs (current <> common <> [z]) $ \c ->
                            cont $ compose z common c $ Leaf $ Indirect{jump = os, value}
                    _ ->
                        error
                            "there is at least on key longer than the requested key to insert"

-- |
-- Batch insert multiple key-value pairs into an empty CSMT.
--
-- Builds the entire tree in one pure pass using divide-and-conquer:
-- O(n log n) vs O(n²) for sequential inserts.
--
-- Assumes the tree is empty. For inserting into an existing tree,
-- use 'inserting'.
insertingBatch
    :: (Monad m, Ord k, GCompare d)
    => Key
    -- ^ Prefix (use @[]@ for root)
    -> FromKV k v a
    -> Hashing a
    -> Selector d k v
    -> Selector d Key (Indirect a)
    -> [(k, v)]
    -> Transaction m cf d ops ()
insertingBatch pfx FromKV{isoK, fromV, treePrefix} hashing kvCol csmtCol kvs = do
    mapM_ (uncurry $ insert kvCol) kvs
    let keyVals = [(treePrefix v <> view isoK k, fromV v) | (k, v) <- kvs]
    case buildComposeFromList keyVals of
        Nothing -> pure ()
        Just c -> mapM_ (uncurry $ insert csmtCol) $ snd $ scanCompose pfx hashing c

-- |
-- Bucketed batch insert for parallel CSMT population.
--
-- Splits the key space by the first @bucketBits@ bits, builds
-- each subtree independently, then merges the top levels in a
-- single-threaded pass.
--
-- The caller provides a @runBuckets@ function that receives a
-- list of @(bucketPrefix, [(Key, a)])@ pairs and must return the
-- DB writes and root 'Indirect' for each bucket. This function
-- can parallelise the work across threads.
--
-- Each bucket's writes are for disjoint key prefixes, so they
-- can be applied to the DB concurrently without conflicts.
insertingBucketed
    :: (Monad m, Ord k, GCompare d)
    => Key
    -- ^ Prefix (use @[]@ for root)
    -> FromKV k v a
    -> Hashing a
    -> Selector d k v
    -> Selector d Key (Indirect a)
    -> Int
    -- ^ Bucket depth in bits
    -> ( [(Key, [(Key, a)])]
         -> [(Key, Indirect a, [(Key, Indirect a)])]
       )
    -- ^ Run buckets: given @(bucketPrefix, items)@ pairs,
    -- return @(bucketPrefix, rootIndirect, dbWrites)@ triples.
    -- This is where the caller injects parallelism.
    -> [(k, v)]
    -> Transaction m cf d ops ()
insertingBucketed pfx FromKV{isoK, fromV, treePrefix} hashing kvCol csmtCol bucketBits runBuckets kvs = do
    -- Write all KV pairs
    mapM_ (uncurry $ insert kvCol) kvs
    -- Convert to tree keys
    let keyVals = [(treePrefix v <> view isoK k, fromV v) | (k, v) <- kvs]
    -- Split into buckets by first N bits
    let buckets = Map.toList $ groupByPrefix bucketBits keyVals
    -- Build subtrees (caller controls parallelism)
    let results =
            runBuckets
                [ (bpfx, items)
                | (bpfx, items) <- buckets
                ]
    -- Write all subtree internal nodes (not roots)
    mapM_
        (\(_, _, writes) -> mapM_ (uncurry $ insert csmtCol) writes)
        results
    -- Merge top levels and write
    let bucketRoots = [(bpfx, rootInd) | (bpfx, rootInd, _) <- results]
    case mergeComposeForest bucketRoots of
        Nothing -> pure ()
        Just topTree -> do
            let (_, topWrites) = scanCompose pfx hashing topTree
            mapM_ (uncurry $ insert csmtCol) topWrites

-- |
-- Build one bucket's subtree. Pure function — no DB access.
--
-- Returns @(rootIndirect, dbWrites)@ where @dbWrites@ contains
-- all nodes except the root (the root may need its 'jump'
-- adjusted during the merge phase).
buildBucket
    :: Key
    -- ^ Bucket prefix
    -> Hashing a
    -> [(Key, a)]
    -- ^ Items with keys relative to bucket prefix (prefix stripped)
    -> Maybe (Indirect a, [(Key, Indirect a)])
buildBucket bpfx hashing items = case buildComposeFromList items of
    Nothing -> Nothing
    Just c ->
        let (rootInd, allWrites) = scanCompose bpfx hashing c
            -- All writes except the root (which the merge phase handles)
            subWrites = filter (\(k, _) -> k /= bpfx) allWrites
        in  Just (rootInd, subWrites)

-- |
-- Build a 'Compose' tree from a list of key-value pairs.
--
-- Uses divide-and-conquer: find common prefix, split by first
-- differing bit, recurse on each side.
buildComposeFromList :: [(Key, a)] -> Maybe (Compose a)
buildComposeFromList [] = Nothing
buildComposeFromList [(key, value)] =
    Just $ Leaf $ Indirect key value
buildComposeFromList kvs =
    let pfx = commonPrefixAll (map fst kvs)
        pfxLen = length pfx
        stripped = [(drop pfxLen k, v) | (k, v) <- kvs]
        grouped = groupByFirstDir stripped
        mLeft = buildComposeFromList (Map.findWithDefault [] L grouped)
        mRight = buildComposeFromList (Map.findWithDefault [] R grouped)
    in  case (mLeft, mRight) of
            (Nothing, Nothing) -> Nothing
            (Just l, Nothing) -> Just $ prependPrefix pfx l
            (Nothing, Just r) -> Just $ prependPrefix pfx r
            (Just l, Just r) -> Just $ Compose pfx l r

-- |
-- Merge bucket roots into a top-level 'Compose' tree.
--
-- Takes @(bucketPrefix, rootIndirect)@ pairs and builds the
-- tree structure for the top levels, handling path compression
-- correctly when buckets are empty.
mergeComposeForest :: [(Key, Indirect a)] -> Maybe (Compose a)
mergeComposeForest [] = Nothing
mergeComposeForest roots =
    buildComposeFromList [(bpfx <> jump i, value i) | (bpfx, i) <- roots]

-- | Prepend a prefix to a 'Compose' tree.
prependPrefix :: Key -> Compose a -> Compose a
prependPrefix [] c = c
prependPrefix p (Leaf (Indirect j v)) = Leaf (Indirect (p <> j) v)
prependPrefix p (Compose j l r) = Compose (p <> j) l r

-- | Find the common prefix of all keys.
commonPrefixAll :: [Key] -> Key
commonPrefixAll [] = []
commonPrefixAll [k] = k
commonPrefixAll (k : ks) = foldl' commonPrefix2 k ks

-- | Find common prefix of two keys.
commonPrefix2 :: Key -> Key -> Key
commonPrefix2 [] _ = []
commonPrefix2 _ [] = []
commonPrefix2 (x : xs) (y : ys)
    | x == y = x : commonPrefix2 xs ys
    | otherwise = []

-- | Group key-value pairs by their first direction.
groupByFirstDir :: [(Key, a)] -> Map Direction [(Key, a)]
groupByFirstDir = foldl' addToGroup Map.empty
  where
    addToGroup acc ([], _) = acc
    addToGroup acc (d : rest, v) =
        Map.insertWith (++) d [(rest, v)] acc

-- | Group items by their first @n@ direction bits.
groupByPrefix :: Int -> [(Key, a)] -> Map Key [(Key, a)]
groupByPrefix !n = foldl' addToGroup Map.empty
  where
    addToGroup acc (k, v) =
        let (bpfx, rest) = splitAt n k
        in  Map.insertWith (++) bpfx [(rest, v)] acc

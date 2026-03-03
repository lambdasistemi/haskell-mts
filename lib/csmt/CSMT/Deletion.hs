-- |
-- Module      : CSMT.Deletion
-- Description : Deletion algorithm for Compact Sparse Merkle Trees
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- This module implements the deletion algorithm for CSMTs.
--
-- The deletion process:
--
-- 1. Build a 'DeletionPath' representing the path to the value being deleted
-- 2. Convert the path to database operations (deletions and updates)
-- 3. Apply all operations atomically
--
-- When a value is deleted, the tree structure may need to be compacted
-- by extending jump paths where branches become unnecessary.
module CSMT.Deletion
    ( deleting
    , newDeletionPath
    , DeletionPath (..)
    , deletionPathToOps
    , deleteSubtree
    )
where

import CSMT.Interface
    ( Direction (..)
    , FromKV (..)
    , Hashing (..)
    , Indirect (..)
    , Key
    , addWithDirection
    , compareKeys
    , oppositeDirection
    )
import Control.Lens (view)
import Control.Monad (guard)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Database.KV.Transaction
    ( GCompare
    , Selector
    , Transaction
    , delete
    , insert
    , query
    )

-- |
-- A path through the tree to a value being deleted.
--
-- * 'Value' - The leaf node containing the value to delete
-- * 'Branch' - An internal node along the path, recording the sibling
--   that will need to be updated after deletion
--
-- This structure captures the entire path from root to leaf, which is
-- needed to properly update parent hashes after deletion.
data DeletionPath a where
    -- | A leaf node with its jump path and value
    Value :: Key -> a -> DeletionPath a
    -- | A branch node with jump path, direction taken, child path, and sibling
    Branch
        :: Key -> Direction -> DeletionPath a -> Indirect a -> DeletionPath a
    deriving (Show, Eq)

-- |
-- Delete a key-value pair from the CSMT.
--
-- This function:
--
-- 1. Builds a deletion path from root to the target key
-- 2. Removes the key from the KV store
-- 3. Updates the tree structure and recomputes affected hashes
deleting
    :: (Monad m, Ord k, GCompare d)
    => Key
    -- ^ Prefix (use @[]@ for root)
    -> FromKV k v a
    -> Hashing a
    -> Selector d k v
    -> Selector d Key (Indirect a)
    -> k
    -> Transaction m cf d ops ()
deleting pfx FromKV{isoK, treePrefix} hashing kvSel csmtSel key = do
    mv <- query kvSel key
    case mv of
        Nothing -> pure ()
        Just v -> do
            let treeKey = treePrefix v <> view isoK key
            mpath <- newDeletionPath pfx csmtSel treeKey
            case mpath of
                Nothing -> pure ()
                Just path -> do
                    delete kvSel key
                    mapM_ (applyOp csmtSel)
                        $ deletionPathToOps pfx hashing path

-- | Apply a single database operation (insert or delete).
applyOp
    :: GCompare d
    => Selector d Key (Indirect a)
    -> (Key, Maybe (Indirect a))
    -> Transaction m cf d ops ()
applyOp csmtSel (k, Nothing) = delete csmtSel k
applyOp csmtSel (k, Just i) = insert csmtSel k i

-- |
-- Convert a deletion path to a list of database operations.
--
-- Traverses the path bottom-up, computing updated hashes and generating
-- insert/delete operations. When a branch loses one child, its sibling's
-- jump path is extended to maintain the compact representation.
deletionPathToOps
    :: forall a
     . Key
    -- ^ Prefix (use @[]@ for root)
    -> Hashing a
    -> DeletionPath a
    -> [(Key, Maybe (Indirect a))]
deletionPathToOps pfx hashing = snd . go pfx
  where
    go
        :: Key
        -> DeletionPath a
        -> (Maybe (Indirect a), [(Key, Maybe (Indirect a))])
    go k (Value _ _v) = (Nothing, [(k, Nothing)])
    go k (Branch j d v i) =
        let
            (msb, xs) = go (k <> j <> [d]) v
        in
            case msb of
                Just i' ->
                    let h = addWithDirection hashing d i' i
                        i'' = Indirect{jump = j, value = h}
                    in  ( Just i''
                        , [(k, Just i'')] <> xs
                        )
                Nothing ->
                    let i' =
                            Indirect
                                { jump = j <> [oppositeDirection d] <> jump i
                                , value = value i
                                }
                    in  ( Just i'
                        , [ (k, Just i')
                          , (k <> j <> [oppositeDirection d], Nothing)
                          ]
                            <> xs
                        )

-- |
-- Build a deletion path for the given key.
--
-- Traverses the tree from root to the target key, recording each branch
-- along the way. Returns 'Nothing' if the key does not exist in the tree.
newDeletionPath
    :: forall a d ops cf m
     . (Monad m, GCompare d)
    => Key
    -- ^ Prefix (use @[]@ for root)
    -> Selector d Key (Indirect a)
    -> Key
    -> Transaction m cf d ops (Maybe (DeletionPath a))
newDeletionPath pfx csmtSel = runMaybeT . go pfx
  where
    go
        :: Key
        -> Key
        -> MaybeT (Transaction m cf d ops) (DeletionPath a)
    go current remaining = do
        Indirect{jump = j, value = v} <- MaybeT $ query csmtSel current
        let (_common, other, remaining') = compareKeys j remaining
        guard $ null other
        case remaining' of
            [] -> pure $ Value j v
            (r : remaining'') -> do
                let current' = current <> j
                sibling <-
                    MaybeT $ query csmtSel (current' <> [oppositeDirection r])
                p <- go (current' <> [r]) remaining''
                pure $ Branch j r p sibling

-- | Delete all nodes under a prefix (entire namespace).
-- Walks the binary trie from @prefix@, deleting every node encountered.
deleteSubtree
    :: (Monad m, GCompare d)
    => Selector d Key (Indirect a)
    -> Key
    -> Transaction m cf d ops ()
deleteSubtree csmtSel = go
  where
    go current = do
        mi <- query csmtSel current
        case mi of
            Nothing -> pure ()
            Just Indirect{jump} -> do
                delete csmtSel current
                let base = current <> jump
                go (base <> [L])
                go (base <> [R])

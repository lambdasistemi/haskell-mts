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
module CSMT.Insertion
    ( inserting
    , buildComposeTree
    , scanCompose
    , Compose (..)
    )
where

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

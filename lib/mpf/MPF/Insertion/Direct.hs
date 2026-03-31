{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

-- | Direct single-path insertion for MPF.
--
-- Walks a single path from root to leaf, reading and writing only the
-- nodes on the insertion path. Returns the node hash up the call stack
-- (like the JS tryInsert's parents list) instead of re-reading from DB.
module MPF.Insertion.Direct
    ( insertingDirect
    )
where

import Database.KV.Transaction
    ( GCompare
    , Selector
    , Transaction
    , insert
    , query
    )
import MPF.Hashes (MPFHashing (..))
import MPF.Interface
    ( FromHexKV (..)
    , HexDigit (..)
    , HexIndirect (..)
    , HexKey
    , compareHexKeys
    , mkBranchIndirect
    , mkLeafIndirect
    )

-- | Insert a key-value pair by walking a single path.
-- Returns the node hash of the root after insertion.
insertingDirect
    :: forall m k v a d cf ops
     . (Monad m, Ord k, GCompare d)
    => HexKey
    -> FromHexKV k v a
    -> MPFHashing a
    -> Selector d k v
    -> Selector d HexKey (HexIndirect a)
    -> k
    -> v
    -> Transaction m cf d ops ()
insertingDirect prefix FromHexKV{fromHexK, fromHexV, hexTreePrefix} hashing kvCol mpfCol k v = do
    insert kvCol k v
    let treeKey = hexTreePrefix v <> fromHexK k
        valHash = fromHexV v
    _ <- go prefix treeKey valHash
    pure ()
  where
    -- Compute the node hash for a HexIndirect (leaf or branch)
    nodeHash :: HexIndirect a -> a
    nodeHash HexIndirect{hexJump, hexValue, hexIsLeaf}
        | hexIsLeaf = leafHash hashing hexJump hexValue
        | otherwise = hexValue

    -- Read child hashes at a branch, returning sparse 16-element list
    readChildHashList
        :: HexKey -> Transaction m cf d ops [Maybe a]
    readChildHashList branchPath =
        mapM
            ( \n -> do
                mi <- query mpfCol (branchPath <> [HexDigit n])
                pure $ fmap nodeHash mi
            )
            [0 .. 15]

    -- Build a branch node from jump prefix and child hash list, return it
    makeBranch :: HexKey -> [Maybe a] -> HexIndirect a
    makeBranch jump childHashes =
        let mr = merkleRoot hashing childHashes
            bh = branchHash hashing jump mr
        in  mkBranchIndirect jump bh

    -- \| Walk the trie, insert, return the NODE HASH of the node at `current`
    go
        :: HexKey
        -> HexKey
        -> a
        -> Transaction m cf d ops a
    go current target valHash = do
        mi <- query mpfCol current
        case mi of
            Nothing -> do
                -- Empty slot: create leaf
                let stored = mkLeafIndirect target valHash
                insert mpfCol current stored
                pure $ nodeHash stored
            Just existing@HexIndirect{hexJump, hexValue, hexIsLeaf} -> do
                let (common, existRest, newRest) = compareHexKeys hexJump target
                case (existRest, newRest) of
                    ([], []) -> do
                        -- Exact key match: replace value
                        let stored = mkLeafIndirect common valHash
                        insert mpfCol current stored
                        pure $ nodeHash stored
                    ([], newD : newDs)
                        | not hexIsLeaf -> do
                            -- Descend into existing branch child
                            updatedChildHash <- go (current <> common <> [newD]) newDs valHash
                            -- Read sibling hashes (NOT the updated child)
                            childHashes <-
                                mapM
                                    ( \n -> do
                                        let digit = HexDigit n
                                        if digit == newD
                                            then pure (Just updatedChildHash)
                                            else do
                                                mi' <- query mpfCol (current <> common <> [digit])
                                                pure $ fmap nodeHash mi'
                                    )
                                    [0 .. 15]
                            let branch = makeBranch common childHashes
                            insert mpfCol current branch
                            pure $ nodeHash branch
                    (existD : existDs, newD : newDs) -> do
                        -- Divergence: create branch with 2 children
                        -- For a branch, need to recompute hash with new (shorter) jump
                        existChild <-
                            if hexIsLeaf
                                then pure $ mkLeafIndirect existDs hexValue
                                else do
                                    -- Read children of the existing branch to recompute with new jump
                                    existChildHashes <- readChildHashList (current <> hexJump)
                                    let mr = merkleRoot hashing existChildHashes
                                        bh = branchHash hashing existDs mr
                                    pure $ mkBranchIndirect existDs bh
                        let newChild = mkLeafIndirect newDs valHash
                            existChildHash = nodeHash existChild
                            newChildHash = nodeHash newChild
                            childHashes =
                                [ if HexDigit n == existD
                                    then Just existChildHash
                                    else
                                        if HexDigit n == newD
                                            then Just newChildHash
                                            else Nothing
                                | n <- [0 .. 15]
                                ]
                            branch = makeBranch common childHashes
                        insert mpfCol current branch
                        insert mpfCol (current <> common <> [existD]) existChild
                        insert mpfCol (current <> common <> [newD]) newChild
                        pure $ nodeHash branch
                    _ ->
                        error "insertingDirect: unexpected key relationship"

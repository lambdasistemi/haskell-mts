{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

-- | Faithful port of the JS @aiken-lang/merkle-patricia-forestry@ insertion.
--
-- Replicates the logic of @tryInsert@ from @trie.js@ (lines 1613-1679):
--
--   1. Walk a single path from root, collecting parent branch nodes.
--   2. At the insertion point, create/split nodes.
--   3. Walk back up the parent list, recomputing each branch hash.
--
-- This avoids the bug in "MPF.Insertion.Direct" where a branch node's
-- hash is not recomputed when its prefix is shortened due to a
-- divergence split.
module MPF.Insertion.Faithful
    ( insertingFaithful
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

-- | Insert a key-value pair by faithfully replicating the JS tryInsert logic.
--
-- Walks a single path from root to insertion point, collecting parent
-- branch paths.  After the insertion, walks back up recomputing each
-- parent's branch hash from its (now-updated) children.
insertingFaithful
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
insertingFaithful prefix FromHexKV{fromHexK, fromHexV, hexTreePrefix} hashing kvCol mpfCol k v = do
    insert kvCol k v
    let treeKey = hexTreePrefix v <> fromHexK k
        valHash = fromHexV v
    parents <- descend prefix treeKey valHash
    walkBackUp parents
  where
    -- \| Compute the node hash of a stored HexIndirect.
    -- Leaves store VALUE hash, so we must apply leafHash.
    -- Branches store BRANCH hash (already computed), so we return as-is.
    nodeHash :: HexIndirect a -> a
    nodeHash HexIndirect{hexJump, hexValue, hexIsLeaf}
        | hexIsLeaf = leafHash hashing hexJump hexValue
        | otherwise = hexValue

    -- \| Read the 16 child node-hashes at a branch location.
    readChildHashes :: HexKey -> Transaction m cf d ops [Maybe a]
    readChildHashes branchPath =
        mapM
            ( \n -> do
                mi <- query mpfCol (branchPath <> [HexDigit n])
                pure $ fmap nodeHash mi
            )
            [0 .. 15]

    -- \| Compute and store a branch node from its child hashes.
    saveBranch :: HexKey -> HexKey -> Transaction m cf d ops ()
    saveBranch storePath jump = do
        childHashes <- readChildHashes (storePath <> jump)
        let mr = merkleRoot hashing childHashes
            bh = branchHash hashing jump mr
        insert mpfCol storePath (mkBranchIndirect jump bh)

    -- \| Descend from @current@ following @target@ path.
    -- Returns the list of (storePath, jump) pairs for parent branches
    -- that need their hashes recomputed (innermost first).
    descend
        :: HexKey
        -> HexKey
        -> a
        -> Transaction m cf d ops [(HexKey, HexKey)]
    descend current target valHash = do
        mi <- query mpfCol current
        case mi of
            Nothing -> do
                -- Empty slot: create leaf
                insert mpfCol current (mkLeafIndirect target valHash)
                pure []
            Just HexIndirect{hexJump, hexValue, hexIsLeaf} -> do
                let (common, existRest, newRest) = compareHexKeys hexJump target
                case (existRest, newRest) of
                    ([], []) -> do
                        -- Exact key match: replace value
                        insert mpfCol current (mkLeafIndirect common valHash)
                        pure []
                    ([], newD : newDs)
                        | not hexIsLeaf -> do
                            -- Descend into existing branch child
                            parents <-
                                descend
                                    (current <> common <> [newD])
                                    newDs
                                    valHash
                            -- This branch needs recomputing on walk-back
                            pure $ parents <> [(current, common)]
                    ([], newD : newDs)
                        | hexIsLeaf -> do
                            -- Existing node is a leaf with matching prefix
                            -- but we have more path remaining.
                            -- This means the existing leaf's key is a prefix
                            -- of the new key's path. Split: the existing leaf
                            -- stays at its nibble, new leaf at newD.
                            --
                            -- Actually this case can't happen in a well-formed
                            -- trie because hex keys (blake2b hashes) are all
                            -- the same length. If it did, it would be an error.
                            error
                                "insertingFaithful: existing leaf is prefix of new key"
                    (existD : existDs, newD : newDs) -> do
                        -- Divergence: prefix splits.
                        -- Create a new branch at @current@ with prefix @common@
                        -- containing the existing node and the new leaf.

                        -- The existing node needs its prefix shortened.
                        -- For a leaf, hexValue is the VALUE hash — that's fine.
                        -- For a branch, hexValue is the BRANCH hash computed
                        -- with the OLD full prefix. We must recompute it with
                        -- the shortened prefix.
                        if hexIsLeaf
                            then do
                                -- Leaf: just shorten prefix, keep value hash
                                insert
                                    mpfCol
                                    (current <> common <> [existD])
                                    (mkLeafIndirect existDs hexValue)
                            else do
                                -- Branch: hexValue was branchHash(hexJump, mr).
                                -- We need branchHash(existDs, mr) instead.
                                -- To get mr, we read children at the OLD
                                -- branch location (current <> hexJump) and
                                -- compute merkleRoot.
                                childHashes <-
                                    readChildHashes (current <> hexJump)
                                let mr = merkleRoot hashing childHashes
                                    newBH = branchHash hashing existDs mr

                                -- Move all children from old location
                                -- (current <> hexJump <> [n]) to new location
                                -- (current <> common <> [existD] <> existDs <> [n])
                                -- Actually the children are stored at absolute
                                -- paths. The old branch was at @current@ with
                                -- jump @hexJump@, so children are at
                                -- @current <> hexJump <> [n]@.
                                -- The new branch will be at
                                -- @current <> common <> [existD]@ with jump
                                -- @existDs@, so children should be at
                                -- @current <> common <> [existD] <> existDs <> [n]@.
                                -- But @common <> [existD] <> existDs == hexJump@
                                -- so the paths are identical! No need to move.
                                insert
                                    mpfCol
                                    (current <> common <> [existD])
                                    (mkBranchIndirect existDs newBH)

                        -- Create the new leaf
                        insert
                            mpfCol
                            (current <> common <> [newD])
                            (mkLeafIndirect newDs valHash)

                        -- Create the parent branch at @current@
                        -- (its hash will be computed by saveBranch)
                        saveBranch current common
                        pure []
                    _ ->
                        error "insertingFaithful: unexpected key relationship"

    -- \| Walk back up the parent list, recomputing each branch hash.
    -- Parents are ordered innermost-first.
    walkBackUp :: [(HexKey, HexKey)] -> Transaction m cf d ops ()
    walkBackUp [] = pure ()
    walkBackUp ((storePath, jump) : rest) = do
        saveBranch storePath jump
        walkBackUp rest

{-# LANGUAGE StrictData #-}

module MPF.Deletion
    ( deleting
    , newMPFDeletionPath
    , MPFDeletionPath (..)
    , deletionPathToOps
    )
where

import Control.Monad (guard)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Database.KV.Transaction
    ( GCompare
    , Selector
    , Transaction
    , delete
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
    )

-- | Deletion path through the MPF trie
data MPFDeletionPath a where
    -- | Terminal: the value to delete with its remaining suffix
    MPFDPValue :: HexKey -> a -> MPFDeletionPath a
    -- | Branch: the jump, the direction taken, the sub-path, and sibling children
    MPFDPBranch
        :: HexKey
        -> HexDigit
        -> MPFDeletionPath a
        -> Map HexDigit (HexIndirect a) -- Sibling children
        -> MPFDeletionPath a
    deriving (Show, Eq)

-- | Delete a key from the MPF structure
deleting
    :: (Monad m, Ord k, GCompare d)
    => FromHexKV k v a
    -> MPFHashing a
    -> Selector d k v
    -> Selector d HexKey (HexIndirect a)
    -> k
    -> Transaction m cf d ops ()
deleting FromHexKV{fromHexK} hashing kvSel mpfSel key = do
    mpath <- newMPFDeletionPath mpfSel (fromHexK key)
    case mpath of
        Nothing -> pure ()
        Just path -> do
            delete kvSel key
            mapM_ (applyOp mpfSel) $ deletionPathToOps hashing path

-- | Apply a single deletion operation
applyOp
    :: GCompare d
    => Selector d HexKey (HexIndirect a)
    -> (HexKey, Maybe (HexIndirect a))
    -> Transaction m cf d ops ()
applyOp mpfSel (k, Nothing) = delete mpfSel k
applyOp mpfSel (k, Just i) = insert mpfSel k i

-- | Convert a deletion path to database operations
deletionPathToOps
    :: forall a
     . MPFHashing a
    -> MPFDeletionPath a
    -> [(HexKey, Maybe (HexIndirect a))]
deletionPathToOps hashing = snd . go []
  where
    go
        :: HexKey
        -> MPFDeletionPath a
        -> (Maybe (HexIndirect a), [(HexKey, Maybe (HexIndirect a))])
    go k (MPFDPValue _ _v) = (Nothing, [(k, Nothing)])
    go k (MPFDPBranch j d subpath siblings) =
        let
            (msb, ops) = go (k <> j <> [d]) subpath
        in
            case msb of
                Just i' ->
                    -- Child still exists, update the branch hash
                    let updatedSiblings = Map.insert d i' siblings
                        sparseArray = [Map.lookup (HexDigit n) updatedSiblings | n <- [0 .. 15]]
                        childHashes = map (fmap hexValue) sparseArray
                        mr = merkleRoot hashing childHashes
                        value = branchHash hashing j mr
                        i'' = mkBranchIndirect j value
                    in  (Just i'', [(k, Just i'')] <> ops)
                Nothing ->
                    -- Child was deleted
                    let remainingChildren = Map.toList siblings
                    in  case remainingChildren of
                            [] ->
                                -- No more children, delete the branch
                                (Nothing, [(k, Nothing), (k <> j <> [d], Nothing)] <> ops)
                            [(onlyD, onlyChild)] ->
                                -- Single child remaining: collapse the branch
                                -- The new node combines the branch jump + child's digit + child's jump
                                -- Preserve the child's node type (leaf or branch)
                                let newJump = j <> [onlyD] <> hexJump onlyChild
                                    collapsed =
                                        HexIndirect
                                            { hexJump = newJump
                                            , hexValue = hexValue onlyChild
                                            , hexIsLeaf = hexIsLeaf onlyChild
                                            }
                                in  ( Just collapsed
                                    , [ (k, Just collapsed)
                                      , (k <> j <> [onlyD], Nothing) -- Delete old child location
                                      , (k <> j <> [d], Nothing) -- Delete deleted child location
                                      ]
                                        <> ops
                                    )
                            _ ->
                                -- Multiple children remain, just remove the deleted one and update hash
                                let sparseArray = [Map.lookup (HexDigit n) siblings | n <- [0 .. 15]]
                                    childHashes = map (fmap hexValue) sparseArray
                                    mr = merkleRoot hashing childHashes
                                    value = branchHash hashing j mr
                                    i'' = mkBranchIndirect j value
                                in  ( Just i''
                                    , [(k, Just i''), (k <> j <> [d], Nothing)] <> ops
                                    )

-- | Build a deletion path by traversing the trie
newMPFDeletionPath
    :: forall a d ops cf m
     . (Monad m, GCompare d)
    => Selector d HexKey (HexIndirect a)
    -> HexKey
    -> Transaction m cf d ops (Maybe (MPFDeletionPath a))
newMPFDeletionPath mpfSel = runMaybeT . go []
  where
    go
        :: HexKey
        -> HexKey
        -> MaybeT (Transaction m cf d ops) (MPFDeletionPath a)
    go current remaining = do
        HexIndirect{hexJump = j, hexValue = v} <-
            MaybeT $ query mpfSel current
        let (_common, other, remaining') = compareHexKeys j remaining
        guard $ null other -- The stored jump must be a prefix of remaining
        case remaining' of
            [] -> pure $ MPFDPValue j v
            (r : remaining'') -> do
                let current' = current <> j
                -- Fetch all siblings at this branch point
                siblings <- MaybeT $ Just <$> fetchSiblings mpfSel current' r
                subpath <- go (current' <> [r]) remaining''
                pure $ MPFDPBranch j r subpath siblings

-- | Fetch all sibling nodes at a branch point (excluding the given digit)
fetchSiblings
    :: (Monad m, GCompare d)
    => Selector d HexKey (HexIndirect a)
    -> HexKey
    -> HexDigit
    -> Transaction m cf d ops (Map HexDigit (HexIndirect a))
fetchSiblings mpfSel prefix exclude = do
    let digits = [HexDigit n | n <- [0 .. 15], HexDigit n /= exclude]
    pairs <- mapM fetchOne digits
    pure $ Map.fromList [(d, i) | (d, Just i) <- pairs]
  where
    fetchOne d = do
        mi <- query mpfSel (prefix <> [d])
        pure (d, mi)

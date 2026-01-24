{-# LANGUAGE StrictData #-}

module MPF.Insertion
    ( inserting
    , mkMPFCompose
    , scanMPFCompose
    , MPFCompose (..)
    )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
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

-- | A tree structure for building insertion operations
-- Similar to CSMT's Compose but with 16-ary branching
data MPFCompose a
    = -- | A leaf with value hash - leafHash will be applied during scan
      MPFComposeLeaf (HexIndirect a)
    | -- | A branch being constructed with children
      MPFComposeBranch HexKey (Map HexDigit (MPFCompose a))
    deriving (Show, Eq)

-- | Insert a key-value pair into the MPF structure
inserting
    :: (Monad m, Ord k, GCompare d)
    => FromHexKV k v a
    -> MPFHashing a
    -> Selector d k v
    -> Selector d HexKey (HexIndirect a)
    -> k
    -> v
    -> Transaction m cf d ops ()
inserting FromHexKV{fromHexK, fromHexV} hashing kvCol mpfCol k v = do
    insert kvCol k v
    c <- mkMPFCompose mpfCol (fromHexK k) (fromHexV v)
    mapM_ (uncurry $ insert mpfCol) $ snd $ scanMPFCompose hashing c

-- | Build a compose tree by traversing the existing trie
mkMPFCompose
    :: forall a d ops cf m
     . (Monad m, GCompare d)
    => Selector d HexKey (HexIndirect a)
    -> HexKey
    -> a
    -> Transaction m cf d ops (MPFCompose a)
mkMPFCompose mpfCol key h = go key [] pure
  where
    go
        :: HexKey
        -> HexKey
        -> (MPFCompose a -> Transaction m cf d ops (MPFCompose a))
        -> Transaction m cf d ops (MPFCompose a)
    go [] _ cont = cont $ MPFComposeLeaf $ mkLeafIndirect [] h
    go target current cont = do
        mi <- query mpfCol current
        case mi of
            Nothing -> cont $ MPFComposeLeaf $ mkLeafIndirect target h
            Just HexIndirect{hexJump, hexValue, hexIsLeaf} -> do
                let (common, other, us) = compareHexKeys hexJump target
                case (other, us) of
                    ([], []) ->
                        -- Exact match: replace value
                        cont $ MPFComposeLeaf $ mkLeafIndirect common h
                    ([], d : ds) -> do
                        -- Key is longer: descend into child
                        -- Fetch existing siblings
                        siblings <- fetchSiblings mpfCol (current <> common) d
                        go ds (current <> common <> [d]) $ \c ->
                            cont $ MPFComposeBranch common $ Map.insert d c siblings
                    (d1 : os, d2 : us') -> do
                        -- Divergence: create new branch with two children
                        -- Check if existing node is a leaf or branch
                        child1 <-
                            if hexIsLeaf
                                then
                                    -- Leaf: hexValue is VALUE hash, wrap as leaf to recompute with new suffix
                                    pure $ MPFComposeLeaf $ mkLeafIndirect os hexValue
                                else do
                                    -- Branch: hexValue is BRANCH hash, need to fetch children
                                    -- and rebuild the branch structure with the new prefix
                                    children <- fetchChildTree mpfCol (current <> hexJump)
                                    pure $ MPFComposeBranch os children
                        go us' (current <> common <> [d2]) $ \c ->
                            cont
                                $ MPFComposeBranch common
                                $ Map.fromList
                                    [ (d1, child1)
                                    , (d2, c)
                                    ]
                    _ ->
                        error "mkMPFCompose: existing key is longer than target"

-- | Fetch sibling nodes at a branch point (excluding the given digit)
-- Wraps each sibling correctly based on whether it's a leaf or branch
fetchSiblings
    :: (Monad m, GCompare d)
    => Selector d HexKey (HexIndirect a)
    -> HexKey
    -> HexDigit
    -> Transaction m cf d ops (Map HexDigit (MPFCompose a))
fetchSiblings mpfCol prefix exclude = do
    let allDigits = [HexDigit n | n <- [0 .. 15], HexDigit n /= exclude]
    pairs <- mapM fetchSibling allDigits
    Map.fromList <$> sequence [wrapNode d i | (d, Just i) <- pairs]
  where
    fetchSibling d = do
        mi <- query mpfCol (prefix <> [d])
        pure (d, mi)
    wrapNode d HexIndirect{hexIsLeaf, hexJump, hexValue}
        | hexIsLeaf =
            pure (d, MPFComposeLeaf $ mkLeafIndirect hexJump hexValue)
        | otherwise = do
            -- Branch: fetch its children to rebuild structure
            children <- fetchChildTree mpfCol (prefix <> [d] <> hexJump)
            pure (d, MPFComposeBranch hexJump children)

-- | Fetch all children of a branch node and wrap them as MPFCompose
fetchChildTree
    :: (Monad m, GCompare d)
    => Selector d HexKey (HexIndirect a)
    -> HexKey
    -> Transaction m cf d ops (Map HexDigit (MPFCompose a))
fetchChildTree mpfCol prefix = do
    let allDigits = [HexDigit n | n <- [0 .. 15]]
    pairs <- mapM fetchChild allDigits
    Map.fromList <$> sequence [wrapNode d node | (d, Just node) <- pairs]
  where
    fetchChild d = do
        mi <- query mpfCol (prefix <> [d])
        pure (d, mi)
    wrapNode d HexIndirect{hexIsLeaf, hexJump, hexValue}
        | hexIsLeaf =
            pure (d, MPFComposeLeaf $ mkLeafIndirect hexJump hexValue)
        | otherwise = do
            -- Recursively fetch children for nested branches
            children <- fetchChildTree mpfCol (prefix <> [d] <> hexJump)
            pure (d, MPFComposeBranch hexJump children)

-- | Scan a compose tree and produce the resulting hash and list of inserts
scanMPFCompose
    :: MPFHashing a
    -> MPFCompose a
    -> (HexIndirect a, [(HexKey, HexIndirect a)])
scanMPFCompose MPFHashing{leafHash, merkleRoot, branchHash} = go []
  where
    go k (MPFComposeLeaf i) =
        -- NEW leaf: compute leaf hash from value hash, store value hash
        let nodeHash = leafHash (hexJump i) (hexValue i)
            stored = mkLeafIndirect (hexJump i) (hexValue i) -- Store VALUE hash
            returned = mkLeafIndirect (hexJump i) nodeHash -- Return NODE hash
        in  (returned, [(k, stored)])
    go k (MPFComposeBranch jump children) =
        let k' = k <> jump
            -- Process all children - they return (nodeHashIndirect, inserts)
            childResults = Map.mapWithKey (\d c -> go (k' <> [d]) c) children
            -- For merkle root, we need the NODE hashes (leaf or branch hashes)
            childIndirects = Map.map fst childResults
            -- Collect all inserts from children
            allInserts = concatMap (snd . snd) (Map.toList childResults)
            -- Build sparse 16-element array for merkle root using NODE hashes
            sparseArray = [Map.lookup (HexDigit n) childIndirects | n <- [0 .. 15]]
            childHashes = map (fmap hexValue) sparseArray
            -- Compute branch hash
            mr = merkleRoot childHashes
            value = branchHash jump mr
            stored = mkBranchIndirect jump value -- Store as BRANCH
            returned = mkBranchIndirect jump value -- Return same for merkle root
        in  (returned, allInserts <> [(k, stored)])

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module MPF.Insertion
    ( inserting
    , insertingBatch
    , insertingChunked
    , insertingStream
    , mkMPFCompose
    , scanMPFCompose
    , MPFCompose (..)
    )
where

import Data.List (foldl')
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

-- | Batch insert multiple key-value pairs into an empty MPF structure
-- This is much faster than sequential inserts as it builds the tree in one pass
-- using divide-and-conquer (O(n log n) vs O(n²))
insertingBatch
    :: (Monad m, Ord k, GCompare d)
    => FromHexKV k v a
    -> MPFHashing a
    -> Selector d k v
    -> Selector d HexKey (HexIndirect a)
    -> [(k, v)]
    -> Transaction m cf d ops ()
insertingBatch FromHexKV{fromHexK, fromHexV} hashing kvCol mpfCol kvs = do
    -- Insert all key-value pairs into the KV store
    mapM_ (uncurry $ insert kvCol) kvs
    -- Build the MPF tree in one pass and insert all nodes
    let hexKvs = [(fromHexK k, fromHexV v) | (k, v) <- kvs]
        compose = buildComposeFromList hexKvs
    case compose of
        Nothing -> pure () -- Empty list
        Just c -> mapM_ (uncurry $ insert mpfCol) $ snd $ scanMPFCompose hashing c

-- | Chunked insert for very large datasets (millions of items)
-- Processes items in chunks to bound memory usage. Works with any backend.
--
-- For truly large datasets (10M+), use this with the RocksDB backend which
-- persists data to disk between chunks, keeping memory bounded.
--
-- Returns the number of chunks processed.
insertingChunked
    :: (Monad m, Ord k, GCompare d)
    => FromHexKV k v a
    -> MPFHashing a
    -> Selector d k v
    -> Selector d HexKey (HexIndirect a)
    -> Int
    -- ^ Chunk size (e.g., 50000)
    -> [(k, v)]
    -> Transaction m cf d ops Int
insertingChunked fhkv hashing kvCol mpfCol chunkSize kvs = do
    let chunks = chunksOf chunkSize kvs
    go 0 chunks
  where
    go !n [] = pure n
    go !n (chunk : rest) = do
        -- Insert this chunk item by item
        mapM_ (\(k, v) -> inserting fhkv hashing kvCol mpfCol k v) chunk
        -- Continue with rest
        go (n + 1) rest

-- | Split a list into chunks of the given size
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
    let (chunk, rest) = splitAt n xs
    in  chunk : chunksOf n rest

-- | Streaming batch insert for very large datasets
-- Groups items by first hex digit and processes each group separately,
-- reducing peak memory by ~16x compared to full batch insert.
-- Still requires O(n) memory for the input list and one group at a time.
insertingStream
    :: forall m k v a d cf ops
     . (Monad m, Ord k, GCompare d)
    => FromHexKV k v a
    -> MPFHashing a
    -> Selector d k v
    -> Selector d HexKey (HexIndirect a)
    -> [(k, v)]
    -> Transaction m cf d ops ()
insertingStream FromHexKV{fromHexK, fromHexV} hashing kvCol mpfCol kvs = do
    -- Insert all key-value pairs into the KV store first
    mapM_ (uncurry $ insert kvCol) kvs

    -- Convert to hex keys and group by first digit
    let hexKvs = [(fromHexK k, fromHexV v) | (k, v) <- kvs]
        grouped = groupByFirstDigit' hexKvs

    -- Process each group independently and collect root nodes
    childResults <- mapM processGroup (Map.toList grouped)

    -- Build final root node from the 16 subtrees
    let childMap = Map.fromList [(d, ind) | (d, Just ind) <- childResults]
    if Map.null childMap
        then pure ()
        else do
            -- Create root branch with all children
            let sparseArray = [Map.lookup (HexDigit n) childMap | n <- [0 .. 15]]
                childHashes = map (fmap hexValue) sparseArray
                mr = merkleRoot hashing childHashes
                rootValue = branchHash hashing [] mr
                rootNode = mkBranchIndirect [] rootValue
            insert mpfCol [] rootNode
  where
    processGroup :: (HexDigit, [(HexKey, a)]) -> Transaction m cf d ops (HexDigit, Maybe (HexIndirect a))
    processGroup (digit, items) = do
        case buildComposeFromList items of
            Nothing -> pure (digit, Nothing)
            Just compose -> do
                let (rootInd, inserts) = scanMPFCompose hashing compose
                -- Write all nodes for this subtree with digit prefix
                mapM_ (\(k, v) -> insert mpfCol ([digit] <> k) v) inserts
                pure (digit, Just rootInd)

-- | Group by first hex digit, keeping remaining key
groupByFirstDigit' :: [(HexKey, a)] -> Map HexDigit [(HexKey, a)]
groupByFirstDigit' = foldl' addToGroup' Map.empty
  where
    addToGroup' acc ([], _) = acc
    addToGroup' acc (d : rest, v) =
        Map.insertWith (++) d [(rest, v)] acc

-- | Build an MPFCompose tree from a list of key-value pairs
-- Uses divide-and-conquer: find common prefix, group by first digit, recurse
buildComposeFromList :: [(HexKey, a)] -> Maybe (MPFCompose a)
buildComposeFromList [] = Nothing
buildComposeFromList [(key, value)] =
    -- Single element: create a leaf
    Just $ MPFComposeLeaf $ mkLeafIndirect key value
buildComposeFromList kvs =
    -- Multiple elements: find common prefix, then branch
    let prefix = commonPrefixAll (map fst kvs)
        prefixLen = length prefix
        -- Strip prefix and group by first digit
        stripped = [(drop prefixLen k, v) | (k, v) <- kvs]
        -- Group by first digit
        grouped = groupByFirstDigit stripped
        -- Recursively build children
        children = Map.mapMaybe buildComposeFromList grouped
    in  if Map.null children
            then Nothing
            else Just $ MPFComposeBranch prefix children

-- | Find the common prefix of all keys
commonPrefixAll :: [HexKey] -> HexKey
commonPrefixAll [] = []
commonPrefixAll [k] = k
commonPrefixAll (k : ks) = foldl' commonPrefix2 k ks

-- | Find common prefix of two keys
commonPrefix2 :: HexKey -> HexKey -> HexKey
commonPrefix2 [] _ = []
commonPrefix2 _ [] = []
commonPrefix2 (x : xs) (y : ys)
    | x == y = x : commonPrefix2 xs ys
    | otherwise = []

-- | Group key-value pairs by their first hex digit
groupByFirstDigit :: [(HexKey, a)] -> Map HexDigit [(HexKey, a)]
groupByFirstDigit = foldl' addToGroup Map.empty
  where
    addToGroup acc ([], _) = acc -- Should not happen after stripping common prefix
    addToGroup acc (d : rest, v) =
        Map.insertWith (++) d [(rest, v)] acc

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

{-# LANGUAGE StrictData #-}

module MPF.Proof.Insertion
    ( MPFProof (..)
    , MPFProofStep (..)
    , MerkleProofItem (..)
    , mkMPFInclusionProof
    , foldMPFProof
    , verifyMPFInclusionProof
    )
where

import Control.Monad (guard)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.List (foldl', isPrefixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Database.KV.Transaction
    ( GCompare
    , Selector
    , Transaction
    , query
    )
import MPF.Hashes (MPFHashing (..))
import MPF.Interface
    ( FromHexKV (..)
    , HexDigit (..)
    , HexIndirect (..)
    , HexKey
    )

-- | A single item in a merkle proof
-- Indicates whether this is a left or right sibling in the merkle tree
data MerkleProofItem a
    = -- | Hash of left sibling
      MPILeft a
    | -- | Hash of right sibling
      MPIRight a
    | -- | No sibling (for sparse positions)
      MPINone
    deriving (Show, Eq)

-- | Proof step types for MPF
data MPFProofStep a
    = ProofStepLeaf
        { pslPrefixLen :: Int
        -- ^ Length of common prefix
        , pslNeighborKeyPath :: HexKey
        -- ^ Neighbor's remaining key path
        , pslNeighborValueDigest :: a
        -- ^ Neighbor's value digest
        }
    | ProofStepFork
        { psfPrefixLen :: Int
        -- ^ Length of common prefix
        , psfNeighborPrefix :: HexKey
        -- ^ Neighbor branch prefix
        , psfNeighborIndex :: HexDigit
        -- ^ Neighbor's position
        , psfMerkleRoot :: a
        -- ^ Merkle root of branch
        }
    | ProofStepBranch
        { psbJump :: HexKey
        -- ^ Jump prefix from this branch to next level
        , psbPosition :: HexDigit
        -- ^ Our position (digit) at this branch
        , psbSiblingHashes :: [(HexDigit, a)]
        -- ^ Sibling NODE hashes at this branch (leafHash applied for leaves)
        }
    deriving (Show, Eq)

-- | Complete membership proof for MPF
data MPFProof a = MPFProof
    { mpfProofSteps :: [MPFProofStep a]
    , mpfProofRootPrefix :: HexKey
    , mpfProofLeafSuffix :: HexKey
    -- ^ The remaining key suffix at the leaf level
    }
    deriving (Show, Eq)

-- | Generate a membership proof for a key
mkMPFInclusionProof
    :: (Monad m, GCompare d)
    => FromHexKV k v a
    -> MPFHashing a
    -> Selector d HexKey (HexIndirect a)
    -> k
    -> Transaction m cf d ops (Maybe (MPFProof a))
mkMPFInclusionProof FromHexKV{fromHexK} hashing sel k = runMaybeT $ do
    let key = fromHexK k
    HexIndirect{hexJump = rootJump, hexIsLeaf = rootIsLeaf} <-
        MaybeT $ query sel []
    guard $ isPrefixOf rootJump key
    let remainingAfterRoot = drop (length rootJump) key
    if rootIsLeaf
        then
            -- Single leaf at root, no branch steps needed
            -- leafSuffix is the node's hexJump (used in leafHash computation)
            pure
                $ MPFProof
                    { mpfProofSteps = []
                    , mpfProofRootPrefix = [] -- No prefix for single-leaf root
                    , mpfProofLeafSuffix = rootJump -- The leaf's suffix is its hexJump
                    }
        else do
            -- For branches, we need to track the parent's jump for branchHash
            -- Root branch's jump is rootJump
            (steps, leafSuffix) <- go [] rootJump remainingAfterRoot
            pure
                $ MPFProof
                    { mpfProofSteps = reverse steps
                    , mpfProofRootPrefix = rootJump
                    , mpfProofLeafSuffix = leafSuffix
                    }
  where
    -- go currentPath currentBranchJump remainingKey
    -- Returns (steps built bottom-up, leaf's hexJump as suffix)
    go _ _ [] = pure ([], [])
    go u branchJump (x : ks) = do
        HexIndirect{hexJump = childJump, hexIsLeaf} <-
            MaybeT $ query sel (u <> branchJump <> [x])
        guard $ isPrefixOf childJump ks
        let remaining = drop (length childJump) ks
        -- Collect sibling information at this branch
        -- Compute node hashes (leafHash for leaves, branchHash for branches)
        siblings <-
            MaybeT
                $ Just <$> fetchSiblingNodeHashes hashing sel (u <> branchJump) x
        -- psbJump is the current BRANCH's jump (for branchHash), not the child's
        let step =
                ProofStepBranch
                    { psbJump = branchJump
                    , psbPosition = x
                    , psbSiblingHashes = Map.toList siblings
                    }
        if hexIsLeaf
            then
                -- Reached the leaf - suffix for hashing is the leaf's hexJump
                pure ([step], childJump)
            else do
                -- Descend into child branch, passing child's jump as the new branchJump
                (restSteps, leafSuffix) <-
                    go (u <> branchJump <> [x]) childJump remaining
                pure (step : restSteps, leafSuffix)

-- | Fetch all sibling NODE hashes at a branch point (excluding the given digit)
-- For leaf nodes: computes leafHash(suffix, valueHash)
-- For branch nodes: uses the stored branchHash directly
fetchSiblingNodeHashes
    :: (Monad m, GCompare d)
    => MPFHashing a
    -> Selector d HexKey (HexIndirect a)
    -> HexKey
    -> HexDigit
    -> Transaction m cf d ops (Map HexDigit a)
fetchSiblingNodeHashes MPFHashing{leafHash} sel prefix exclude = do
    let digits = [HexDigit n | n <- [0 .. 15], HexDigit n /= exclude]
    pairs <- mapM fetchOne digits
    pure $ Map.fromList [(d, h) | (d, Just h) <- pairs]
  where
    fetchOne d = do
        mi <- query sel (prefix <> [d])
        pure $ case mi of
            Nothing -> (d, Nothing)
            Just HexIndirect{hexJump, hexValue, hexIsLeaf}
                | hexIsLeaf ->
                    -- Leaf: hexValue is VALUE hash, compute NODE hash
                    (d, Just $ leafHash hexJump hexValue)
                | otherwise ->
                    -- Branch: hexValue is already the BRANCH hash
                    (d, Just hexValue)

-- | Fold a proof to compute the root hash
-- Starts with the value hash, computes the leaf hash, then works up through
-- branches applying merkleRoot and branchHash at each level
foldMPFProof :: MPFHashing a -> a -> MPFProof a -> a
foldMPFProof hashing valueHash MPFProof{mpfProofSteps, mpfProofLeafSuffix} =
    case mpfProofSteps of
        [] ->
            -- Single leaf at root: compute leaf hash with full suffix
            leafHash hashing mpfProofLeafSuffix valueHash
        steps ->
            -- Multiple levels: fold from leaf up to root
            -- Steps are ordered leaf-to-root, so foldl' processes in correct order:
            -- start with leaf hash, combine with leaf's parent siblings, work up
            let leafNodeHash = leafHash hashing mpfProofLeafSuffix valueHash
            in  foldl' step leafNodeHash steps
  where
    step acc proofStep =
        case proofStep of
            ProofStepBranch{psbJump, psbPosition, psbSiblingHashes} ->
                let siblingMap = Map.fromList psbSiblingHashes
                    -- Build sparse 16-element array with our hash at psbPosition
                    -- and sibling hashes at their respective positions
                    sparseArray =
                        [ if HexDigit n == psbPosition
                            then Just acc
                            else Map.lookup (HexDigit n) siblingMap
                        | n <- [0 .. 15]
                        ]
                    mr = merkleRoot hashing sparseArray
                in  branchHash hashing psbJump mr
            ProofStepLeaf{} ->
                -- Leaf step: handled at termination
                acc
            ProofStepFork{psfMerkleRoot} ->
                -- Fork step: use the provided merkle root
                psfMerkleRoot

-- | Verify a membership proof
-- Compares the computed root hash from the proof against the stored root hash
verifyMPFInclusionProof
    :: (Eq a, Monad m, GCompare d)
    => FromHexKV k v a
    -> Selector d HexKey (HexIndirect a)
    -> MPFHashing a
    -> v
    -> MPFProof a
    -> Transaction m cf d ops Bool
verifyMPFInclusionProof FromHexKV{fromHexV} sel hashing@MPFHashing{leafHash} v proof = do
    let valueHash = fromHexV v
    mv <- query sel []
    pure $ case mv of
        Just HexIndirect{hexJump, hexValue, hexIsLeaf} ->
            -- Compute the root NODE hash from what's stored
            -- Leaf: hexValue is VALUE hash, need to apply leafHash
            -- Branch: hexValue is already the BRANCH hash
            let rootNodeHash =
                    if hexIsLeaf
                        then leafHash hexJump hexValue
                        else hexValue
            in  rootNodeHash == foldMPFProof hashing valueHash proof
        Nothing -> False

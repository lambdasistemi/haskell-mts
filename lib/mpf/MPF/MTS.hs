-- | MPF implementation of the MTS interface.
--
-- Defines @MpfImpl@ phantom type with type family instances
-- and constructors that wrap MPF operations into
-- 'MerkleTreeStore'.
--
-- Four constructors are provided:
--
-- * 'mpfMerkleTreeStoreT' — prefix-scoped transactional store,
--   composable within a single atomic transaction.
-- * 'mpfMerkleTreeStore' — convenience wrapper that commits
--   each operation in its own transaction.
-- * 'mpfNamespacedMTST' — transactional namespaced store.
-- * 'mpfNamespacedMTS' — IO namespaced store.
module MPF.MTS
    ( MpfImpl
    , mpfMerkleTreeStoreT
    , mpfMerkleTreeStore
    , mpfNamespacedMTST
    , mpfNamespacedMTS
    )
where

import Data.ByteString (ByteString)
import Database.KV.Database (Database)
import Database.KV.Transaction
    ( Transaction
    , query
    , runTransactionUnguarded
    )
import MPF.Backend.Standalone
    ( MPFStandalone (..)
    , MPFStandaloneCF
    , MPFStandaloneOp
    )
import MPF.Deletion (deleteSubtree, deleting)
import MPF.Hashes (MPFHash, MPFHashing (..))
import MPF.Insertion (MPFCompose, inserting, insertingBatch)
import MPF.Interface
    ( FromHexKV (..)
    , HexIndirect (..)
    , HexKey
    )
import MPF.Proof.Completeness
    ( collectMPFLeaves
    , foldMPFCompletenessProof
    , generateMPFCompletenessProof
    )
import MPF.Proof.Insertion
    ( MPFProof
    , foldMPFProof
    , mkMPFInclusionProof
    , verifyMPFInclusionProof
    )
import MTS.Interface
    ( MerkleTreeStore (..)
    , MtsCompletenessProof
    , MtsHash
    , MtsKey
    , MtsLeaf
    , MtsPrefix
    , MtsProof
    , MtsValue
    , NamespacedMTS (..)
    , hoistMTS
    , hoistNamespacedMTS
    )

-- | Phantom type tag for the MPF implementation.
data MpfImpl

type instance MtsKey MpfImpl = ByteString
type instance MtsValue MpfImpl = ByteString
type instance MtsHash MpfImpl = MPFHash
type instance MtsProof MpfImpl = MPFProof MPFHash
type instance MtsLeaf MpfImpl = HexIndirect MPFHash
type instance MtsCompletenessProof MpfImpl = MPFCompose MPFHash
type instance MtsPrefix MpfImpl = HexKey

-- | Compute the MPF root hash from the root node.
mpfRootFromNode
    :: MPFHashing MPFHash -> HexIndirect MPFHash -> MPFHash
mpfRootFromNode hashing i =
    if hexIsLeaf i
        then leafHash hashing (hexJump i) (hexValue i)
        else hexValue i

-- | Build a transactional 'MerkleTreeStore' for MPF scoped to a
-- prefix.
--
-- Operations live in the 'Transaction' monad so multiple
-- calls can be composed into a single atomic transaction:
--
-- @
-- runTransactionUnguarded db $ do
--     mtsInsert store k1 v1
--     mtsDelete store k2
--     mtsRootHash store
-- @
mpfMerkleTreeStoreT
    :: (MonadFail m)
    => HexKey
    -- ^ Prefix (use @[]@ for root)
    -> FromHexKV ByteString ByteString MPFHash
    -> MPFHashing MPFHash
    -> MerkleTreeStore
        MpfImpl
        ( Transaction
            m
            MPFStandaloneCF
            (MPFStandalone ByteString ByteString MPFHash)
            MPFStandaloneOp
        )
mpfMerkleTreeStoreT prefix fromKV hashing =
    MerkleTreeStore
        { mtsInsert =
            inserting
                prefix
                fromKV
                hashing
                MPFStandaloneKVCol
                MPFStandaloneMPFCol
        , mtsDelete =
            deleting
                prefix
                fromKV
                hashing
                MPFStandaloneKVCol
                MPFStandaloneMPFCol
        , mtsRootHash = do
            mi <- query MPFStandaloneMPFCol prefix
            pure $ fmap (mpfRootFromNode hashing) mi
        , mtsMkProof = \k -> do
            mp <-
                mkMPFInclusionProof
                    prefix
                    fromKV
                    hashing
                    MPFStandaloneMPFCol
                    k
            case mp of
                Nothing -> pure Nothing
                Just proof -> do
                    mi <-
                        query MPFStandaloneMPFCol prefix
                    pure $ case mi of
                        Nothing -> Nothing
                        Just i ->
                            Just (mpfRootFromNode hashing i, proof)
        , mtsVerifyProof =
            verifyMPFInclusionProof
                prefix
                fromKV
                MPFStandaloneMPFCol
                hashing
        , mtsFoldProof =
            foldMPFProof hashing
        , mtsBatchInsert =
            insertingBatch
                prefix
                fromKV
                hashing
                MPFStandaloneKVCol
                MPFStandaloneMPFCol
        , mtsCollectLeaves =
            collectMPFLeaves MPFStandaloneMPFCol prefix
        , mtsMkCompletenessProof =
            generateMPFCompletenessProof MPFStandaloneMPFCol prefix
        , mtsVerifyCompletenessProof = \leaves proof -> do
            mi <- query MPFStandaloneMPFCol prefix
            let currentRoot = fmap (mpfRootFromNode hashing) mi
                computed =
                    foldMPFCompletenessProof hashing leaves proof
            pure $ case (currentRoot, computed) of
                (Just r, Just computedRoot) ->
                    computedRoot == r
                _ -> False
        }

-- | Build an IO 'MerkleTreeStore' for MPF scoped to a prefix.
--
-- Each operation commits in its own transaction. For atomic
-- multi-operation sequences, use 'mpfMerkleTreeStoreT' instead.
mpfMerkleTreeStore
    :: (MonadFail m)
    => HexKey
    -- ^ Prefix (use @[]@ for root)
    -> (forall b. m b -> IO b)
    -> Database
        m
        MPFStandaloneCF
        (MPFStandalone ByteString ByteString MPFHash)
        MPFStandaloneOp
    -> FromHexKV ByteString ByteString MPFHash
    -> MPFHashing MPFHash
    -> MerkleTreeStore MpfImpl IO
mpfMerkleTreeStore prefix run db fromKV hashing =
    hoistMTS
        (run . runTransactionUnguarded db)
        (mpfMerkleTreeStoreT prefix fromKV hashing)

-- | Build a transactional 'NamespacedMTS' for MPF.
--
-- Each namespace is a prefix-scoped 'MerkleTreeStore'.
mpfNamespacedMTST
    :: (MonadFail m)
    => FromHexKV ByteString ByteString MPFHash
    -> MPFHashing MPFHash
    -> NamespacedMTS
        MpfImpl
        ( Transaction
            m
            MPFStandaloneCF
            (MPFStandalone ByteString ByteString MPFHash)
            MPFStandaloneOp
        )
mpfNamespacedMTST fromKV hashing =
    NamespacedMTS
        { nsStore = \prefix ->
            mpfMerkleTreeStoreT prefix fromKV hashing
        , nsDelete =
            deleteSubtree MPFStandaloneMPFCol
        }

-- | Build an IO 'NamespacedMTS' for MPF.
--
-- Each namespace is a prefix-scoped 'MerkleTreeStore' that
-- commits each operation in its own transaction.
mpfNamespacedMTS
    :: (MonadFail m)
    => (forall b. m b -> IO b)
    -> Database
        m
        MPFStandaloneCF
        (MPFStandalone ByteString ByteString MPFHash)
        MPFStandaloneOp
    -> FromHexKV ByteString ByteString MPFHash
    -> MPFHashing MPFHash
    -> NamespacedMTS MpfImpl IO
mpfNamespacedMTS run db fromKV hashing =
    hoistNamespacedMTS
        (run . runTransactionUnguarded db)
        (mpfNamespacedMTST fromKV hashing)

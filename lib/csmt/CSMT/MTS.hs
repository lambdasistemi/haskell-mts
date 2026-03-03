-- | CSMT implementation of the MTS interface.
--
-- Defines @CsmtImpl@ phantom type with type family instances
-- and constructors that wrap CSMT operations into
-- 'MerkleTreeStore'.
--
-- Four constructors are provided:
--
-- * 'csmtMerkleTreeStoreT' — prefix-scoped transactional store,
--   composable within a single atomic transaction.
-- * 'csmtMerkleTreeStore' — convenience wrapper that commits
--   each operation in its own transaction.
-- * 'csmtNamespacedMTST' — transactional namespaced store.
-- * 'csmtNamespacedMTS' — IO namespaced store.
module CSMT.MTS
    ( CsmtImpl
    , csmtMerkleTreeStoreT
    , csmtMerkleTreeStore
    , csmtNamespacedMTST
    , csmtNamespacedMTS
    )
where

import CSMT.Backend.Standalone
    ( Standalone (..)
    , StandaloneCF
    , StandaloneOp
    )
import CSMT.Deletion (deleteSubtree, deleting)
import CSMT.Hashes (Hash)
import CSMT.Insertion (inserting)
import CSMT.Interface
    ( FromKV (..)
    , Hashing (..)
    , Indirect (..)
    , Key
    , root
    )
import CSMT.Proof.Completeness
    ( CompletenessProof (..)
    , collectValues
    , foldCompletenessProof
    , generateProof
    )
import CSMT.Proof.Insertion
    ( InclusionProof (..)
    , buildInclusionProof
    , computeRootHash
    , verifyInclusionProof
    )
import Data.ByteString (ByteString)
import Database.KV.Database (Database)
import Database.KV.Transaction
    ( Transaction
    , runTransactionUnguarded
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

-- | Phantom type tag for the CSMT implementation.
data CsmtImpl

type instance MtsKey CsmtImpl = ByteString
type instance MtsValue CsmtImpl = ByteString
type instance MtsHash CsmtImpl = Hash
type instance MtsProof CsmtImpl = InclusionProof Hash
type instance MtsLeaf CsmtImpl = Indirect Hash
type instance MtsCompletenessProof CsmtImpl = CompletenessProof Hash
type instance MtsPrefix CsmtImpl = Key

-- | Build a transactional 'MerkleTreeStore' for CSMT scoped to a
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
csmtMerkleTreeStoreT
    :: (Monad m)
    => Key
    -- ^ Prefix (use @[]@ for root)
    -> FromKV ByteString ByteString Hash
    -> Hashing Hash
    -> MerkleTreeStore
        CsmtImpl
        ( Transaction
            m
            StandaloneCF
            (Standalone ByteString ByteString Hash)
            StandaloneOp
        )
csmtMerkleTreeStoreT prefix fromKV hashing =
    MerkleTreeStore
        { mtsInsert =
            inserting
                prefix
                fromKV
                hashing
                StandaloneKVCol
                StandaloneCSMTCol
        , mtsDelete =
            deleting
                prefix
                fromKV
                hashing
                StandaloneKVCol
                StandaloneCSMTCol
        , mtsRootHash =
            root hashing StandaloneCSMTCol prefix
        , mtsMkProof = \k -> do
            mp <-
                buildInclusionProof
                    prefix
                    fromKV
                    StandaloneKVCol
                    StandaloneCSMTCol
                    hashing
                    k
            case mp of
                Nothing -> pure Nothing
                Just (_, proof) -> do
                    mr <- root hashing StandaloneCSMTCol prefix
                    pure $ case mr of
                        Nothing -> Nothing
                        Just r -> Just (r, proof)
        , mtsVerifyProof = \v proof ->
            pure
                $ proofValue proof == fromV fromKV v
                    && verifyInclusionProof hashing proof
        , mtsFoldProof =
            computeRootHash hashing
        , mtsBatchInsert =
            mapM_
                ( uncurry
                    ( inserting
                        prefix
                        fromKV
                        hashing
                        StandaloneKVCol
                        StandaloneCSMTCol
                    )
                )
        , mtsCollectLeaves =
            collectValues StandaloneCSMTCol prefix []
        , mtsMkCompletenessProof =
            generateProof StandaloneCSMTCol prefix []
        , mtsVerifyCompletenessProof = \leaves proof -> do
            currentRoot <-
                root hashing StandaloneCSMTCol prefix
            let computed =
                    foldCompletenessProof hashing [] leaves proof
            pure $ case (currentRoot, computed) of
                (Just r, Just computedRoot) ->
                    computedRoot == r
                _ -> False
        }

-- | Build an IO 'MerkleTreeStore' for CSMT scoped to a prefix.
--
-- Each operation commits in its own transaction. For atomic
-- multi-operation sequences, use 'csmtMerkleTreeStoreT' instead.
csmtMerkleTreeStore
    :: (MonadFail m)
    => Key
    -- ^ Prefix (use @[]@ for root)
    -> (forall b. m b -> IO b)
    -> Database
        m
        StandaloneCF
        (Standalone ByteString ByteString Hash)
        StandaloneOp
    -> FromKV ByteString ByteString Hash
    -> Hashing Hash
    -> MerkleTreeStore CsmtImpl IO
csmtMerkleTreeStore prefix run db fromKV hashing =
    hoistMTS
        (run . runTransactionUnguarded db)
        (csmtMerkleTreeStoreT prefix fromKV hashing)

-- | Build a transactional 'NamespacedMTS' for CSMT.
--
-- Each namespace is a prefix-scoped 'MerkleTreeStore'.
csmtNamespacedMTST
    :: (Monad m)
    => FromKV ByteString ByteString Hash
    -> Hashing Hash
    -> NamespacedMTS
        CsmtImpl
        ( Transaction
            m
            StandaloneCF
            (Standalone ByteString ByteString Hash)
            StandaloneOp
        )
csmtNamespacedMTST fromKV hashing =
    NamespacedMTS
        { nsStore = \prefix ->
            csmtMerkleTreeStoreT prefix fromKV hashing
        , nsDelete =
            deleteSubtree StandaloneCSMTCol
        }

-- | Build an IO 'NamespacedMTS' for CSMT.
--
-- Each namespace is a prefix-scoped 'MerkleTreeStore' that
-- commits each operation in its own transaction.
csmtNamespacedMTS
    :: (MonadFail m)
    => (forall b. m b -> IO b)
    -> Database
        m
        StandaloneCF
        (Standalone ByteString ByteString Hash)
        StandaloneOp
    -> FromKV ByteString ByteString Hash
    -> Hashing Hash
    -> NamespacedMTS CsmtImpl IO
csmtNamespacedMTS run db fromKV hashing =
    hoistNamespacedMTS
        (run . runTransactionUnguarded db)
        (csmtNamespacedMTST fromKV hashing)

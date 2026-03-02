-- | CSMT implementation of the MTS interface.
--
-- Defines @CsmtImpl@ phantom type with type family instances
-- and constructors that wrap CSMT operations into
-- 'MerkleTreeStore'.
--
-- Two constructors are provided:
--
-- * 'csmtMerkleTreeStoreT' — operations live in the
--   'Transaction' monad, composable within a single atomic
--   transaction.
-- * 'csmtMerkleTreeStore' — convenience wrapper that commits
--   each operation in its own transaction (suitable for simple
--   use cases and tests).
module CSMT.MTS
    ( CsmtImpl
    , csmtMerkleTreeStoreT
    , csmtMerkleTreeStore
    )
where

import CSMT.Backend.Standalone
    ( Standalone (..)
    , StandaloneCF
    , StandaloneOp
    )
import CSMT.Deletion (deleting)
import CSMT.Hashes (Hash)
import CSMT.Insertion (inserting)
import CSMT.Interface
    ( FromKV (..)
    , Hashing (..)
    , Indirect (..)
    , root
    )
import CSMT.Proof.Completeness
    ( CompletenessProof
    , collectValues
    , foldProof
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
    , MtsProof
    , MtsValue
    , hoistMTS
    )

-- | Phantom type tag for the CSMT implementation.
data CsmtImpl

type instance MtsKey CsmtImpl = ByteString
type instance MtsValue CsmtImpl = ByteString
type instance MtsHash CsmtImpl = Hash
type instance MtsProof CsmtImpl = InclusionProof Hash
type instance MtsLeaf CsmtImpl = Indirect Hash
type instance MtsCompletenessProof CsmtImpl = CompletenessProof

-- | Build a transactional 'MerkleTreeStore' for CSMT.
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
    => FromKV ByteString ByteString Hash
    -> Hashing Hash
    -> MerkleTreeStore
        CsmtImpl
        ( Transaction
            m
            StandaloneCF
            (Standalone ByteString ByteString Hash)
            StandaloneOp
        )
csmtMerkleTreeStoreT fromKV hashing =
    MerkleTreeStore
        { mtsInsert =
            inserting fromKV hashing StandaloneKVCol StandaloneCSMTCol
        , mtsDelete =
            deleting fromKV hashing StandaloneKVCol StandaloneCSMTCol
        , mtsRootHash =
            root hashing StandaloneCSMTCol
        , mtsMkProof = \k -> do
            mp <-
                buildInclusionProof
                    fromKV
                    StandaloneKVCol
                    StandaloneCSMTCol
                    hashing
                    k
            case mp of
                Nothing -> pure Nothing
                Just (_, proof) -> do
                    mr <- root hashing StandaloneCSMTCol
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
                        fromKV
                        hashing
                        StandaloneKVCol
                        StandaloneCSMTCol
                    )
                )
        , mtsCollectLeaves =
            collectValues StandaloneCSMTCol []
        , mtsMkCompletenessProof =
            generateProof StandaloneCSMTCol []
        , mtsVerifyCompletenessProof = \leaves proof -> do
            currentRoot <- root hashing StandaloneCSMTCol
            let computed =
                    foldProof (combineHash hashing) leaves proof
            pure $ case (currentRoot, computed) of
                (Just r, Just indirect) ->
                    rootHash hashing indirect == r
                _ -> False
        }

-- | Build an IO 'MerkleTreeStore' for CSMT.
--
-- Each operation commits in its own transaction. For atomic
-- multi-operation sequences, use 'csmtMerkleTreeStoreT' instead.
csmtMerkleTreeStore
    :: (MonadFail m)
    => (forall b. m b -> IO b)
    -> Database
        m
        StandaloneCF
        (Standalone ByteString ByteString Hash)
        StandaloneOp
    -> FromKV ByteString ByteString Hash
    -> Hashing Hash
    -> MerkleTreeStore CsmtImpl IO
csmtMerkleTreeStore run db fromKV hashing =
    hoistMTS
        (run . runTransactionUnguarded db)
        (csmtMerkleTreeStoreT fromKV hashing)

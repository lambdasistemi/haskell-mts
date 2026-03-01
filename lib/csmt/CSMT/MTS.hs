-- | CSMT implementation of the MTS interface.
--
-- Defines @CsmtImpl@ phantom type with type family instances
-- and a constructor that wraps CSMT operations into
-- 'MerkleTreeStore'.
module CSMT.MTS
    ( CsmtImpl
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
import Database.KV.Transaction (runTransactionUnguarded)
import MTS.Interface
    ( MerkleTreeStore (..)
    , MtsCompletenessProof
    , MtsHash
    , MtsKey
    , MtsLeaf
    , MtsProof
    , MtsValue
    )

-- | Phantom type tag for the CSMT implementation.
data CsmtImpl

type instance MtsKey CsmtImpl = ByteString
type instance MtsValue CsmtImpl = ByteString
type instance MtsHash CsmtImpl = Hash
type instance MtsProof CsmtImpl = InclusionProof Hash
type instance MtsLeaf CsmtImpl = Indirect Hash
type instance MtsCompletenessProof CsmtImpl = CompletenessProof

-- | Build a 'MerkleTreeStore' for CSMT.
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
    MerkleTreeStore
        { mtsInsert = \k v ->
            run
                $ runTransactionUnguarded db
                $ inserting fromKV hashing StandaloneKVCol StandaloneCSMTCol k v
        , mtsDelete = \k ->
            run
                $ runTransactionUnguarded db
                $ deleting fromKV hashing StandaloneKVCol StandaloneCSMTCol k
        , mtsRootHash =
            run
                $ runTransactionUnguarded db
                $ root hashing StandaloneCSMTCol
        , mtsMkProof = \k ->
            run
                $ runTransactionUnguarded db
                $ do
                    mp <-
                        buildInclusionProof fromKV StandaloneKVCol StandaloneCSMTCol hashing k
                    pure $ fmap snd mp
        , mtsVerifyProof = \v proof ->
            pure
                $ proofValue proof == fromV fromKV v
                    && verifyInclusionProof hashing proof
        , mtsFoldProof = \_ proof ->
            computeRootHash hashing proof
        , mtsBatchInsert = \kvs ->
            run
                $ runTransactionUnguarded db
                $ mapM_
                    ( \(k, v) -> inserting fromKV hashing StandaloneKVCol StandaloneCSMTCol k v
                    )
                    kvs
        , mtsCollectLeaves =
            run
                $ runTransactionUnguarded db
                $ collectValues StandaloneCSMTCol []
        , mtsMkCompletenessProof =
            run
                $ runTransactionUnguarded db
                $ generateProof StandaloneCSMTCol []
        , mtsVerifyCompletenessProof = \leaves proof -> do
            currentRoot <-
                run
                    $ runTransactionUnguarded db
                    $ root hashing StandaloneCSMTCol
            let computed = foldProof (combineHash hashing) leaves proof
            pure $ case (currentRoot, computed) of
                (Just r, Just indirect) ->
                    rootHash hashing indirect == r
                _ -> False
        }

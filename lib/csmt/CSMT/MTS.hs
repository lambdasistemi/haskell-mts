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

import CSMT.Backend.Standalone (Standalone (..), StandaloneCodecs)
import CSMT.Deletion (deleting)
import CSMT.Hashes (Hash)
import CSMT.Insertion (inserting)
import CSMT.Interface
    ( FromKV (..)
    , Hashing (..)
    , Indirect (..)
    , Key
    , root
    )
import CSMT.Proof.Insertion
    ( InclusionProof (..)
    , buildInclusionProof
    , computeRootHash
    , verifyInclusionProof
    )
import Data.ByteString (ByteString)
import Database.KV.Transaction (runTransactionUnguarded)
import MTS.Interface
    ( MerkleTreeStore (..)
    , MtsHash
    , MtsKey
    , MtsProof
    , MtsValue
    )

-- | Phantom type tag for the CSMT implementation.
data CsmtImpl

type instance MtsKey CsmtImpl = ByteString
type instance MtsValue CsmtImpl = ByteString
type instance MtsHash CsmtImpl = Hash
type instance MtsProof CsmtImpl = InclusionProof Hash

-- | Build a 'MerkleTreeStore' for CSMT.
csmtMerkleTreeStore
    :: (forall b. m b -> IO b)
    -> StandaloneCodecs ByteString ByteString Hash
    -> (StandaloneCodecs ByteString ByteString Hash -> db)
    -> FromKV ByteString ByteString Hash
    -> Hashing Hash
    -> MerkleTreeStore CsmtImpl IO
csmtMerkleTreeStore run codecs mkDb fromKV hashing =
    MerkleTreeStore
        { mtsInsert = \k v ->
            run
                $ runTransactionUnguarded (mkDb codecs)
                $ inserting fromKV hashing StandaloneKVCol StandaloneCSMTCol k v
        , mtsDelete = \k ->
            run
                $ runTransactionUnguarded (mkDb codecs)
                $ deleting fromKV hashing StandaloneKVCol StandaloneCSMTCol k
        , mtsRootHash =
            run
                $ runTransactionUnguarded (mkDb codecs)
                $ root hashing StandaloneCSMTCol
        , mtsMkProof = \k ->
            run
                $ runTransactionUnguarded (mkDb codecs)
                $ do
                    mp <- buildInclusionProof fromKV StandaloneKVCol StandaloneCSMTCol hashing k
                    pure $ fmap snd mp
        , mtsVerifyProof = \v proof ->
            pure
                $ proofValue proof == fromV fromKV v
                && verifyInclusionProof hashing proof
        , mtsFoldProof = \_ proof ->
            computeRootHash hashing proof
        , mtsBatchInsert = \kvs ->
            run
                $ runTransactionUnguarded (mkDb codecs)
                $ mapM_ (\(k, v) -> inserting fromKV hashing StandaloneKVCol StandaloneCSMTCol k v) kvs
        }

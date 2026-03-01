-- | Wraps CSMT operations into a 'MerkleTreeStore'.
module CSMT.MTS
    ( csmtMerkleTreeStore
    )
where

import CSMT.Backend.Standalone (Standalone (..), StandaloneCodecs)
import CSMT.Deletion (deleting)
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
import Database.KV.Transaction (runTransactionUnguarded)
import MTS.Interface (MerkleTreeStore (..))

-- | Build a 'MerkleTreeStore' from CSMT components.
--
-- The @run@ function executes a monadic action in the
-- backend context (e.g. 'runPure' for in-memory, IO for RocksDB).
csmtMerkleTreeStore
    :: (Ord k, Eq a)
    => (forall b. m b -> IO b)
    -> StandaloneCodecs k v a
    -> (StandaloneCodecs k v a -> db)
    -> FromKV k v a
    -> Hashing a
    -> MerkleTreeStore IO k v a (InclusionProof a)
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

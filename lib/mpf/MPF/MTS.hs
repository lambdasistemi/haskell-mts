-- | Wraps MPF operations into a 'MerkleTreeStore'.
module MPF.MTS
    ( mpfMerkleTreeStore
    )
where

import Database.KV.Transaction (query, runTransactionUnguarded)
import MPF.Backend.Standalone
    ( MPFStandalone (..)
    , MPFStandaloneCodecs
    )
import MPF.Deletion (deleting)
import MPF.Hashes (MPFHashing (..))
import MPF.Insertion (inserting, insertingBatch)
import MPF.Interface
    ( FromHexKV (..)
    , HexIndirect (..)
    , HexKey
    )
import MPF.Proof.Insertion
    ( MPFProof
    , foldMPFProof
    , mkMPFInclusionProof
    , verifyMPFInclusionProof
    )
import MTS.Interface (MerkleTreeStore (..))

-- | Build a 'MerkleTreeStore' from MPF components.
--
-- The @run@ function executes a monadic action in the
-- backend context (e.g. 'runMPFPure' for in-memory).
mpfMerkleTreeStore
    :: (Ord k, Eq a)
    => (forall b. m b -> IO b)
    -> MPFStandaloneCodecs k a a
    -> (MPFStandaloneCodecs k a a -> db)
    -> FromHexKV k v a
    -> MPFHashing a
    -> MerkleTreeStore IO k v a (MPFProof a)
mpfMerkleTreeStore run codecs mkDb fromKV hashing =
    MerkleTreeStore
        { mtsInsert = \k v ->
            run
                $ runTransactionUnguarded (mkDb codecs)
                $ inserting fromKV hashing MPFStandaloneKVCol MPFStandaloneMPFCol k v
        , mtsDelete = \k ->
            run
                $ runTransactionUnguarded (mkDb codecs)
                $ deleting fromKV hashing MPFStandaloneKVCol MPFStandaloneMPFCol k
        , mtsRootHash =
            run
                $ runTransactionUnguarded (mkDb codecs)
                $ do
                    mi <- query MPFStandaloneMPFCol ([] :: HexKey)
                    pure $ case mi of
                        Nothing -> Nothing
                        Just i ->
                            Just
                                $ if hexIsLeaf i
                                    then leafHash hashing (hexJump i) (hexValue i)
                                    else hexValue i
        , mtsMkProof = \k ->
            run
                $ runTransactionUnguarded (mkDb codecs)
                $ mkMPFInclusionProof fromKV hashing MPFStandaloneMPFCol k
        , mtsVerifyProof = \v proof ->
            run
                $ runTransactionUnguarded (mkDb codecs)
                $ verifyMPFInclusionProof fromKV MPFStandaloneMPFCol hashing v proof
        , mtsFoldProof = \_ proof ->
            foldMPFProof hashing (fromHexV fromKV undefined) proof
        , mtsBatchInsert = \kvs ->
            run
                $ runTransactionUnguarded (mkDb codecs)
                $ insertingBatch fromKV hashing MPFStandaloneKVCol MPFStandaloneMPFCol kvs
        }

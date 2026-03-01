-- | MPF implementation of the MTS interface.
--
-- Defines @MpfImpl@ phantom type with type family instances
-- and a constructor that wraps MPF operations into
-- 'MerkleTreeStore'.
module MPF.MTS
    ( MpfImpl
    , mpfMerkleTreeStore
    )
where

import Data.ByteString (ByteString)
import Database.KV.Transaction (query, runTransactionUnguarded)
import MPF.Backend.Standalone
    ( MPFStandalone (..)
    , MPFStandaloneCodecs
    )
import MPF.Deletion (deleting)
import MPF.Hashes (MPFHash, MPFHashing (..))
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
import MTS.Interface
    ( MerkleTreeStore (..)
    , MtsHash
    , MtsKey
    , MtsProof
    , MtsValue
    )

-- | Phantom type tag for the MPF implementation.
data MpfImpl

type instance MtsKey MpfImpl = ByteString
type instance MtsValue MpfImpl = ByteString
type instance MtsHash MpfImpl = MPFHash
type instance MtsProof MpfImpl = MPFProof MPFHash

-- | Build a 'MerkleTreeStore' for MPF.
mpfMerkleTreeStore
    :: (forall b. m b -> IO b)
    -> MPFStandaloneCodecs ByteString MPFHash MPFHash
    -> (MPFStandaloneCodecs ByteString MPFHash MPFHash -> db)
    -> FromHexKV ByteString ByteString MPFHash
    -> MPFHashing MPFHash
    -> MerkleTreeStore MpfImpl IO
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

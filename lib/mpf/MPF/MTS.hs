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
import Database.KV.Database (Database)
import Database.KV.Transaction (query, runTransactionUnguarded)
import MPF.Backend.Standalone
    ( MPFStandalone (..)
    , MPFStandaloneCF
    , MPFStandaloneOp
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
    , MtsCompletenessProof
    , MtsHash
    , MtsKey
    , MtsLeaf
    , MtsProof
    , MtsValue
    )

-- | Phantom type tag for the MPF implementation.
data MpfImpl

type instance MtsKey MpfImpl = ByteString
type instance MtsValue MpfImpl = ByteString
type instance MtsHash MpfImpl = MPFHash
type instance MtsProof MpfImpl = MPFProof MPFHash
type instance MtsLeaf MpfImpl = HexIndirect MPFHash
type instance MtsCompletenessProof MpfImpl = ()

-- | Build a 'MerkleTreeStore' for MPF.
mpfMerkleTreeStore
    :: (MonadFail m)
    => (forall b. m b -> IO b)
    -> Database
        m
        MPFStandaloneCF
        (MPFStandalone ByteString ByteString MPFHash)
        MPFStandaloneOp
    -> FromHexKV ByteString ByteString MPFHash
    -> MPFHashing MPFHash
    -> MerkleTreeStore MpfImpl IO
mpfMerkleTreeStore run db fromKV hashing =
    MerkleTreeStore
        { mtsInsert = \k v ->
            run
                $ runTransactionUnguarded db
                $ inserting fromKV hashing MPFStandaloneKVCol MPFStandaloneMPFCol k v
        , mtsDelete = \k ->
            run
                $ runTransactionUnguarded db
                $ deleting fromKV hashing MPFStandaloneKVCol MPFStandaloneMPFCol k
        , mtsRootHash =
            run
                $ runTransactionUnguarded db
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
                $ runTransactionUnguarded db
                $ mkMPFInclusionProof fromKV hashing MPFStandaloneMPFCol k
        , mtsVerifyProof = \v proof ->
            run
                $ runTransactionUnguarded db
                $ verifyMPFInclusionProof fromKV MPFStandaloneMPFCol hashing v proof
        , mtsFoldProof = \_ proof ->
            foldMPFProof hashing (fromHexV fromKV undefined) proof
        , mtsBatchInsert = \kvs ->
            run
                $ runTransactionUnguarded db
                $ insertingBatch
                    fromKV
                    hashing
                    MPFStandaloneKVCol
                    MPFStandaloneMPFCol
                    kvs
        , mtsCollectLeaves =
            fail "MPF completeness proofs not implemented"
        , mtsMkCompletenessProof =
            fail "MPF completeness proofs not implemented"
        , mtsVerifyCompletenessProof = \_ _ ->
            fail "MPF completeness proofs not implemented"
        }

-- | MPF implementation of the MTS interface.
--
-- Defines @MpfImpl@ phantom type with type family instances
-- and constructors that wrap MPF operations into
-- 'MerkleTreeStore'.
--
-- Two constructors are provided:
--
-- * 'mpfMerkleTreeStoreT' — operations live in the
--   'Transaction' monad, composable within a single atomic
--   transaction.
-- * 'mpfMerkleTreeStore' — convenience wrapper that commits
--   each operation in its own transaction (suitable for simple
--   use cases and tests).
module MPF.MTS
    ( MpfImpl
    , mpfMerkleTreeStoreT
    , mpfMerkleTreeStore
    )
where

import Control.Monad.Trans.Class (lift)
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
    , hoistMTS
    )

-- | Phantom type tag for the MPF implementation.
data MpfImpl

type instance MtsKey MpfImpl = ByteString
type instance MtsValue MpfImpl = ByteString
type instance MtsHash MpfImpl = MPFHash
type instance MtsProof MpfImpl = MPFProof MPFHash
type instance MtsLeaf MpfImpl = HexIndirect MPFHash
type instance MtsCompletenessProof MpfImpl = ()

-- | Compute the MPF root hash from the root node.
mpfRootFromNode
    :: MPFHashing MPFHash -> HexIndirect MPFHash -> MPFHash
mpfRootFromNode hashing i =
    if hexIsLeaf i
        then leafHash hashing (hexJump i) (hexValue i)
        else hexValue i

-- | Build a transactional 'MerkleTreeStore' for MPF.
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
    => FromHexKV ByteString ByteString MPFHash
    -> MPFHashing MPFHash
    -> MerkleTreeStore
        MpfImpl
        ( Transaction
            m
            MPFStandaloneCF
            (MPFStandalone ByteString ByteString MPFHash)
            MPFStandaloneOp
        )
mpfMerkleTreeStoreT fromKV hashing =
    MerkleTreeStore
        { mtsInsert =
            inserting
                fromKV
                hashing
                MPFStandaloneKVCol
                MPFStandaloneMPFCol
        , mtsDelete =
            deleting
                fromKV
                hashing
                MPFStandaloneKVCol
                MPFStandaloneMPFCol
        , mtsRootHash = do
            mi <- query MPFStandaloneMPFCol ([] :: HexKey)
            pure $ fmap (mpfRootFromNode hashing) mi
        , mtsMkProof = \k -> do
            mp <-
                mkMPFInclusionProof
                    fromKV
                    hashing
                    MPFStandaloneMPFCol
                    k
            case mp of
                Nothing -> pure Nothing
                Just proof -> do
                    mi <-
                        query MPFStandaloneMPFCol ([] :: HexKey)
                    pure $ case mi of
                        Nothing -> Nothing
                        Just i ->
                            Just (mpfRootFromNode hashing i, proof)
        , mtsVerifyProof =
            verifyMPFInclusionProof
                fromKV
                MPFStandaloneMPFCol
                hashing
        , mtsFoldProof =
            foldMPFProof hashing
        , mtsBatchInsert =
            insertingBatch
                fromKV
                hashing
                MPFStandaloneKVCol
                MPFStandaloneMPFCol
        , mtsCollectLeaves =
            lift $ fail "MPF completeness proofs not implemented"
        , mtsMkCompletenessProof =
            lift $ fail "MPF completeness proofs not implemented"
        , mtsVerifyCompletenessProof = \_ _ ->
            lift $ fail "MPF completeness proofs not implemented"
        }

-- | Build an IO 'MerkleTreeStore' for MPF.
--
-- Each operation commits in its own transaction. For atomic
-- multi-operation sequences, use 'mpfMerkleTreeStoreT' instead.
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
    hoistMTS
        (run . runTransactionUnguarded db)
        (mpfMerkleTreeStoreT fromKV hashing)

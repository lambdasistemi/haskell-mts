module MTS.PropertySpec (spec) where

import CSMT.Backend.Pure
    ( emptyInMemoryDB
    , pureDatabase
    , runPure
    )
import CSMT.Backend.Standalone (Standalone (..), StandaloneCodecs (..))
import CSMT.Deletion (deleting)
import CSMT.Hashes (Hash, byteStringToKey, fromKVHashes, hashHashing, isoHash, keyToByteString, mkHash)
import CSMT.Insertion (inserting)
import CSMT.Interface
    ( FromKV (..)
    , Hashing (..)
    , Indirect (..)
    , Key
    , keyPrism
    , root
    )
import CSMT.MTS (CsmtImpl, csmtMerkleTreeStore)
import CSMT.Proof.Insertion (InclusionProof (..), buildInclusionProof, computeRootHash, verifyInclusionProof)
import Control.Lens (iso)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import MPF.Backend.Pure
    ( emptyMPFInMemoryDB
    , mpfPureDatabase
    , runMPFPure
    )
import MPF.Backend.Standalone (MPFStandalone (..), MPFStandaloneCodecs (..))
import MPF.Hashes (MPFHash, mkMPFHash, mpfHashing, renderMPFHash)
import MPF.Interface (FromHexKV (..), HexIndirect (..), HexKey, byteStringToHexKey, hexKeyPrism)
import MPF.MTS (MpfImpl, mpfMerkleTreeStore)
import MTS.Interface (MerkleTreeStore (..))
import MTS.Properties
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck
    ( Gen
    , arbitrary
    , elements
    , listOf1
    , property
    , vectorOf
    , (==>)
    )

-- ------------------------------------------------------------------
-- CSMT store factory
-- ------------------------------------------------------------------

mkCsmtStore :: IO (MerkleTreeStore CsmtImpl IO)
mkCsmtStore = do
    ref <- newIORef emptyInMemoryDB
    let run action = do
            db <- readIORef ref
            let (a, db') = runPure db action
            writeIORef ref db'
            pure a
    pure $ csmtMerkleTreeStore run csmtCodecs pureDatabase fromKVHashes hashHashing
  where
    csmtCodecs :: StandaloneCodecs ByteString ByteString Hash
    csmtCodecs =
        StandaloneCodecs
            { keyCodec = keyPrism
            , valueCodec = isoHash
            , nodeCodec = isoHash
            }

-- ------------------------------------------------------------------
-- MPF store factory
-- ------------------------------------------------------------------

mkMpfStore :: IO (MerkleTreeStore MpfImpl IO)
mkMpfStore = do
    ref <- newIORef emptyMPFInMemoryDB
    let run action = do
            db <- readIORef ref
            let (a, db') = runMPFPure db action
            writeIORef ref db'
            pure a
    pure $ mpfMerkleTreeStore run mpfCodecs mpfPureDatabase fromHexKVBS mpfHashing
  where
    mpfCodecs :: MPFStandaloneCodecs ByteString MPFHash MPFHash
    mpfCodecs =
        MPFStandaloneCodecs
            { mpfKeyCodec = hexKeyPrism
            , mpfValueCodec = isoMPFHash
            , mpfNodeCodec = isoMPFHash
            }
    isoMPFHash = iso parseMPFHashUnsafe renderMPFHash
    parseMPFHashUnsafe bs = case parseMPFHash' bs of
        Just h -> h
        Nothing -> mkMPFHash bs
    parseMPFHash' bs
        | B.length bs == 32 = Just (mkMPFHash bs)
        | otherwise = Nothing
    fromHexKVBS :: FromHexKV ByteString ByteString MPFHash
    fromHexKVBS =
        FromHexKV
            { fromHexK = byteStringToHexKey
            , fromHexV = mkMPFHash
            , hexTreePrefix = const []
            }

-- ------------------------------------------------------------------
-- Generators
-- ------------------------------------------------------------------

genBSPair :: Gen (ByteString, ByteString)
genBSPair = do
    k <- B.pack <$> vectorOf 8 arbitrary
    v <- B.pack <$> vectorOf 8 arbitrary
    pure (k, v)

genBSPairs :: Gen [(ByteString, ByteString)]
genBSPairs = listOf1 genBSPair

genBSTriple :: Gen (ByteString, ByteString, ByteString)
genBSTriple = do
    k <- B.pack <$> vectorOf 8 arbitrary
    v <- B.pack <$> vectorOf 8 arbitrary
    v' <- B.pack <$> vectorOf 8 arbitrary
    pure (k, v, v')

-- ------------------------------------------------------------------
-- Spec
-- ------------------------------------------------------------------

spec :: Spec
spec = do
    describe "CSMT shared properties" $ do
        it "insert-verify" $ propInsertVerify mkCsmtStore genBSPair
        it "multiple insert all verify" $ propMultipleInsertAllVerify mkCsmtStore genBSPairs
        it "insertion order independence" $ propInsertionOrderIndependence mkCsmtStore mkCsmtStore genBSPairs
        it "delete removes key" $ propDeleteRemovesKey mkCsmtStore genBSPair
        it "delete preserves siblings" $ propDeletePreservesSiblings mkCsmtStore genBSPair genBSPair genBSPair
        it "insert-delete-all empty" $ propInsertDeleteAllEmpty mkCsmtStore genBSPairs
        it "empty tree no root" $ propEmptyTreeNoRoot mkCsmtStore
        it "single insert has root" $ propSingleInsertHasRoot mkCsmtStore genBSPair
        it "wrong value rejects" $ propWrongValueRejects mkCsmtStore genBSTriple

    describe "MPF shared properties" $ do
        it "insert-verify" $ propInsertVerify mkMpfStore genBSPair
        it "multiple insert all verify" $ propMultipleInsertAllVerify mkMpfStore genBSPairs
        it "insertion order independence" $ propInsertionOrderIndependence mkMpfStore mkMpfStore genBSPairs
        it "delete removes key" $ propDeleteRemovesKey mkMpfStore genBSPair
        it "delete preserves siblings" $ propDeletePreservesSiblings mkMpfStore genBSPair genBSPair genBSPair
        it "insert-delete-all empty" $ propInsertDeleteAllEmpty mkMpfStore genBSPairs
        it "empty tree no root" $ propEmptyTreeNoRoot mkMpfStore
        it "single insert has root" $ propSingleInsertHasRoot mkMpfStore genBSPair
        it "wrong value rejects" $ propWrongValueRejects mkMpfStore genBSTriple

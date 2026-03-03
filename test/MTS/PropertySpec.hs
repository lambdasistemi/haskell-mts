module MTS.PropertySpec (spec) where

import CSMT.Backend.Pure
    ( Pure
    , emptyInMemoryDB
    , pureDatabase
    , runPure
    )
import CSMT.Backend.Standalone (StandaloneCodecs (..))
import CSMT.Hashes (Hash, fromKVHashes, hashHashing, isoHash)
import CSMT.MTS (CsmtImpl, csmtMerkleTreeStore)
import Control.Lens (Iso', iso)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.IORef (newIORef, readIORef, writeIORef)
import MPF.Backend.Pure
    ( MPFPure
    , emptyMPFInMemoryDB
    , mpfPureDatabase
    , runMPFPure
    )
import MPF.Backend.Standalone (MPFStandaloneCodecs (..))
import MPF.Hashes
    ( MPFHash
    , mkMPFHash
    , mpfHashing
    , parseMPFHash
    , renderMPFHash
    )
import MPF.Interface (FromHexKV (..), byteStringToHexKey)
import MPF.MTS (MpfImpl, mpfMerkleTreeStore)
import MTS.Interface (MerkleTreeStore)
import MTS.Properties
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck
    ( Gen
    , arbitrary
    , listOf1
    , vectorOf
    )

-- ------------------------------------------------------------------
-- CSMT store factory
-- ------------------------------------------------------------------

mkCsmtStore :: IO (MerkleTreeStore CsmtImpl IO)
mkCsmtStore = do
    ref <- newIORef emptyInMemoryDB
    let run :: forall b. Pure b -> IO b
        run action = do
            db <- readIORef ref
            let (a, db') = runPure db action
            writeIORef ref db'
            pure a
    pure
        $ csmtMerkleTreeStore
            []
            run
            (pureDatabase csmtCodecs)
            fromKVHashes
            hashHashing
  where
    csmtCodecs :: StandaloneCodecs ByteString ByteString Hash
    csmtCodecs =
        StandaloneCodecs
            { keyCodec = iso id id
            , valueCodec = iso id id
            , nodeCodec = isoHash
            }

-- ------------------------------------------------------------------
-- MPF store factory
-- ------------------------------------------------------------------

mkMpfStore :: IO (MerkleTreeStore MpfImpl IO)
mkMpfStore = do
    ref <- newIORef emptyMPFInMemoryDB
    let run :: forall b. MPFPure b -> IO b
        run action = do
            db <- readIORef ref
            let (a, db') = runMPFPure db action
            writeIORef ref db'
            pure a
    pure
        $ mpfMerkleTreeStore
            []
            run
            (mpfPureDatabase mpfCodecs)
            fromHexKVBS
            mpfHashing
  where
    mpfCodecs :: MPFStandaloneCodecs ByteString ByteString MPFHash
    mpfCodecs =
        MPFStandaloneCodecs
            { mpfKeyCodec = iso id id
            , mpfValueCodec = iso id id
            , mpfNodeCodec = isoMPFHash
            }
    isoMPFHash :: Iso' ByteString MPFHash
    isoMPFHash = iso parseMPFHashUnsafe renderMPFHash
    parseMPFHashUnsafe bs = case parseMPFHash bs of
        Just h -> h
        Nothing -> mkMPFHash bs
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
        it "multiple insert all verify"
            $ propMultipleInsertAllVerify mkCsmtStore genBSPairs
        it "insertion order independence"
            $ propInsertionOrderIndependence mkCsmtStore mkCsmtStore genBSPairs
        it "delete removes key" $ propDeleteRemovesKey mkCsmtStore genBSPair
        it "delete preserves siblings"
            $ propDeletePreservesSiblings mkCsmtStore genBSPair genBSPair genBSPair
        it "insert-delete-all empty"
            $ propInsertDeleteAllEmpty mkCsmtStore genBSPairs
        it "empty tree no root" $ propEmptyTreeNoRoot mkCsmtStore
        it "single insert has root"
            $ propSingleInsertHasRoot mkCsmtStore genBSPair
        it "wrong value rejects"
            $ propWrongValueRejects mkCsmtStore genBSTriple
        it "proof anchored to root"
            $ propProofAnchoredToRoot mkCsmtStore genBSPair
        it "completeness round-trip"
            $ propCompletenessRoundTrip mkCsmtStore genBSPairs
        it "completeness empty"
            $ propCompletenessEmpty mkCsmtStore
        it "completeness after delete"
            $ propCompletenessAfterDelete mkCsmtStore genBSPairs

    describe "MPF shared properties" $ do
        it "insert-verify" $ propInsertVerify mkMpfStore genBSPair
        it "multiple insert all verify"
            $ propMultipleInsertAllVerify mkMpfStore genBSPairs
        it "insertion order independence"
            $ propInsertionOrderIndependence mkMpfStore mkMpfStore genBSPairs
        it "delete removes key" $ propDeleteRemovesKey mkMpfStore genBSPair
        it "delete preserves siblings"
            $ propDeletePreservesSiblings mkMpfStore genBSPair genBSPair genBSPair
        it "insert-delete-all empty"
            $ propInsertDeleteAllEmpty mkMpfStore genBSPairs
        it "empty tree no root" $ propEmptyTreeNoRoot mkMpfStore
        it "single insert has root"
            $ propSingleInsertHasRoot mkMpfStore genBSPair
        it "wrong value rejects"
            $ propWrongValueRejects mkMpfStore genBSTriple
        it "proof anchored to root"
            $ propProofAnchoredToRoot mkMpfStore genBSPair
        it "completeness round-trip"
            $ propCompletenessRoundTrip mkMpfStore genBSPairs
        it "completeness empty"
            $ propCompletenessEmpty mkMpfStore
        it "completeness after delete"
            $ propCompletenessAfterDelete mkMpfStore genBSPairs

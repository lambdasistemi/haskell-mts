module CSMT.FifoSpec (spec) where

import CSMT.Backend.Pure
    ( Pure
    , emptyInMemoryDB
    , pureDatabase
    , runPure
    )
import CSMT.Backend.Standalone
    ( Standalone (..)
    , StandaloneCF
    , StandaloneCodecs (..)
    , StandaloneOp
    )
import CSMT.Deletion (deleting)
import CSMT.Fifo (counterToKey)
import CSMT.Hashes
    ( Hash
    , fromKVHashes
    , hashHashing
    , isoHash
    )
import CSMT.Insertion (inserting)
import CSMT.Interface qualified as I
import CSMT.Proof.Insertion (buildInclusionProof)
import Control.Lens (simple)
import Data.ByteString (ByteString)
import Database.KV.Transaction
    ( Transaction
    , runTransactionUnguarded
    )
import Test.Hspec (Spec, describe, it, shouldBe)

-- | Codecs matching the test-vector generator.
bsCodecs :: StandaloneCodecs ByteString ByteString Hash
bsCodecs =
    StandaloneCodecs
        { keyCodec = simple
        , valueCodec = simple
        , nodeCodec = isoHash
        }

-- | Run a transaction in the pure backend.
runTx
    :: Transaction
        Pure
        StandaloneCF
        (Standalone ByteString ByteString Hash)
        StandaloneOp
        a
    -> Pure a
runTx = runTransactionUnguarded (pureDatabase bsCodecs)

-- | Insert a key-value pair.
insertK :: ByteString -> ByteString -> Pure ()
insertK k v =
    runTx
        $ inserting
            fromKVHashes
            hashHashing
            StandaloneKVCol
            StandaloneCSMTCol
            k
            v

-- | Delete a key.
deleteK :: ByteString -> Pure ()
deleteK k =
    runTx
        $ deleting
            fromKVHashes
            hashHashing
            StandaloneKVCol
            StandaloneCSMTCol
            k

-- | Get root hash.
rootK :: Pure (Maybe Hash)
rootK = runTx $ I.root hashHashing StandaloneCSMTCol

-- | Check if a key has a valid proof.
hasProof :: ByteString -> Pure Bool
hasProof k = do
    mp <-
        runTx
            $ buildInclusionProof
                fromKVHashes
                StandaloneKVCol
                StandaloneCSMTCol
                hashHashing
                k
    pure $ case mp of
        Nothing -> False
        Just _ -> True

spec :: Spec
spec = do
    describe "counterToKey encoding" $ do
        it "encodes 0 as single zero byte"
            $ counterToKey 1 0
            `shouldBe` "\x00"

        it "encodes 128 as single byte"
            $ counterToKey 1 128
            `shouldBe` "\x80"

        it "encodes 255 as single byte"
            $ counterToKey 1 255
            `shouldBe` "\xff"

        it "encodes 0 as two zero bytes"
            $ counterToKey 2 0
            `shouldBe` "\x00\x00"

        it "encodes 256 as two bytes big-endian"
            $ counterToKey 2 256
            `shouldBe` "\x01\x00"

        it "encodes 65535 as two bytes"
            $ counterToKey 2 65535
            `shouldBe` "\xff\xff"

    describe "FIFO push/pop round-trip" $ do
        it "pushes 3 values and all have proofs" $ do
            let ixB = 1
                key = counterToKey ixB
                vA = "\xaa" :: ByteString
                vB = "\xbb" :: ByteString
                vC = "\xcc" :: ByteString
                ((h0, h1, h2), _db) =
                    runPure emptyInMemoryDB $ do
                        insertK (key 0) vA
                        insertK (key 1) vB
                        insertK (key 2) vC
                        (,,)
                            <$> hasProof (key 0)
                            <*> hasProof (key 1)
                            <*> hasProof (key 2)
            h0 `shouldBe` True
            h1 `shouldBe` True
            h2 `shouldBe` True

        it "pops in FIFO order and tree empties" $ do
            let ixB = 1
                key = counterToKey ixB
                vA = "\xaa" :: ByteString
                vB = "\xbb" :: ByteString
                vC = "\xcc" :: ByteString
                (finalRoot, _db) =
                    runPure emptyInMemoryDB $ do
                        insertK (key 0) vA
                        insertK (key 1) vB
                        insertK (key 2) vC
                        deleteK (key 0)
                        deleteK (key 1)
                        deleteK (key 2)
                        rootK
            finalRoot `shouldBe` Nothing

        it "intermediate roots differ" $ do
            let ixB = 1
                key = counterToKey ixB
                vA = "\xaa" :: ByteString
                vB = "\xbb" :: ByteString
                vC = "\xcc" :: ByteString
                ((r1, r2, r3), _db) =
                    runPure emptyInMemoryDB $ do
                        insertK (key 0) vA
                        r1' <- rootK
                        insertK (key 1) vB
                        r2' <- rootK
                        insertK (key 2) vC
                        r3' <- rootK
                        pure (r1', r2', r3')
            (r1 /= r2) `shouldBe` True
            (r2 /= r3) `shouldBe` True
            (r1 /= r3) `shouldBe` True

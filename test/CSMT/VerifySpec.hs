-- |
-- Module      : CSMT.VerifySpec
-- Description : Cross-check @csmt-verify@ against @csmt@ write side
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- The @csmt-verify@ sublibrary is an independent, database-free
-- reimplementation of the Merkle path arithmetic that the full
-- @csmt@ library uses on the write side. This spec makes sure the
-- two implementations agree on:
--
--   * the CBOR wire format for inclusion proofs, and
--   * the computed root hash for any given proof.
--
-- If either of those drifts, a server-rendered proof will fail to
-- verify on the client, which is the exact failure mode we want to
-- catch here rather than in production.
module CSMT.VerifySpec (spec) where

import CSMT.Hashes (Hash, mkHash, renderHash)
import CSMT.Hashes.CBOR qualified as Write
import CSMT.Interface (Direction (..), Indirect (..))
import CSMT.Proof.Insertion
    ( InclusionProof (..)
    , ProofStep (..)
    , computeRootHash
    )
import CSMT.Verify qualified as Verify
import Data.Bits (xor)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Word (Word8)
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
    ( Gen
    , arbitrary
    , choose
    , elements
    , forAll
    , listOf
    , vectorOf
    , (===)
    )

import CSMT.Hashes qualified as CSMT

genBS :: Gen ByteString
genBS = B.pack <$> listOf (arbitrary :: Gen Word8)

genKey :: Gen [Direction]
genKey = listOf (elements [L, R])

-- | A structurally-valid inclusion proof. The total bits consumed
-- by the steps equal @length proofKey - length proofRootJump@, and
-- each 'stepConsumed' is at least 1 (as 'foldProof' requires a
-- direction at each step).
genProof :: Gen (InclusionProof Hash)
genProof = do
    totalKeyLen <- choose (0, 16 :: Int)
    rootJumpLen <- choose (0, totalKeyLen)
    let remaining = totalKeyLen - rootJumpLen
    consumed <- partitionSteps remaining
    proofKey <- vectorOf totalKeyLen (elements [L, R])
    let proofRootJump = take rootJumpLen proofKey
    proofValue <- mkHash <$> genBS
    proofSteps <- traverse mkStep consumed
    pure
        InclusionProof
            { proofKey
            , proofValue
            , proofSteps
            , proofRootJump
            }
  where
    mkStep stepConsumed = do
        siblingValue <- mkHash <$> genBS
        siblingJump <- genKey
        pure
            ProofStep
                { stepConsumed
                , stepSibling =
                    Indirect
                        { jump = siblingJump
                        , value = siblingValue
                        }
                }

    partitionSteps 0 = pure []
    partitionSteps n = do
        step <- choose (1, n)
        (step :) <$> partitionSteps (n - step)

spec :: Spec
spec = describe "csmt-verify cross-check" $ do
    prop "write-side render parses and verifies on csmt-verify"
        $ forAll genProof
        $ \proof ->
            let rootBytes =
                    renderHash
                        (computeRootHash CSMT.hashHashing proof)
                proofBytes = Write.renderProof proof
            in  Verify.verifyInclusionProof rootBytes proofBytes
                    === True

    prop "csmt-verify rejects a tampered root"
        $ forAll genProof
        $ \proof ->
            let trueRoot =
                    renderHash
                        (computeRootHash CSMT.hashHashing proof)
                badRoot = B.map (`xor` 0xff) trueRoot
                proofBytes = Write.renderProof proof
            in  Verify.verifyInclusionProof badRoot proofBytes
                    === False

    prop "rejects garbage proof bytes"
        $ forAll ((,) <$> vectorOf 32 arbitrary <*> genBS)
        $ \(rootBs, garbage) ->
            Verify.verifyInclusionProof (B.pack rootBs) garbage
                === False

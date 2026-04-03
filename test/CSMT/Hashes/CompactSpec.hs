{-# LANGUAGE OverloadedStrings #-}

module CSMT.Hashes.CompactSpec (spec) where

import CSMT.Hashes (hashHashing, mkHash)
import CSMT.Hashes.CBOR (renderProof)
import CSMT.Hashes.Compact
    ( packKey
    , parseCompactProof
    , renderCompactProof
    , unpackKey
    )
import CSMT.Hashes.Types (Hash)
import CSMT.Interface (Direction (..), Indirect (..), Key)
import CSMT.Proof.Insertion
    ( InclusionProof (..)
    , ProofStep (..)
    , computeRootHash
    )
import Data.ByteString qualified as BS
import Test.Hspec
import Test.QuickCheck

genDirection :: Gen Direction
genDirection = elements [L, R]

genKey :: Gen Key
genKey = do
    len <- choose (0, 256)
    vectorOf len genDirection

spec :: Spec
spec = describe "CSMT.Hashes.Compact" $ do
    describe "packKey / unpackKey" $ do
        it "round-trips empty key"
            $ unpackKey (packKey [])
            `shouldBe` Just []

        it "round-trips single L"
            $ unpackKey (packKey [L])
            `shouldBe` Just [L]

        it "round-trips single R"
            $ unpackKey (packKey [R])
            `shouldBe` Just [R]

        it "round-trips 8 directions"
            $ let key = [L, R, L, R, R, L, L, R]
              in  unpackKey (packKey key) `shouldBe` Just key

        it "round-trips 256 directions"
            $ property
            $ forAll (vectorOf 256 genDirection)
            $ \key ->
                unpackKey (packKey key) === Just key

        it "round-trips arbitrary keys"
            $ property
            $ forAll genKey
            $ \key ->
                unpackKey (packKey key) === Just key

        it "packs efficiently (34 bytes for 256 directions)" $ do
            let key = replicate 256 L
                packed = packKey key
            BS.length packed `shouldBe` 34

    describe "renderCompactProof / parseCompactProof" $ do
        it "compact proof is smaller than full CBOR proof" $ do
            let proof = multiStepProof
                fullSize = BS.length (renderProof proof)
                compactSize = BS.length (renderCompactProof proof)
            putStrLn $ "\n  full CBOR: " ++ show fullSize ++ " bytes"
            putStrLn $ "  compact:   " ++ show compactSize ++ " bytes"
            putStrLn
                $ "  reduction: "
                    ++ show (100 - (compactSize * 100 `div` fullSize))
                    ++ "%"
            compactSize `shouldSatisfy` (< fullSize)

        it "round-trips single-element proof preserving root hash" $ do
            let proof = singleElementProof
                compact = renderCompactProof proof
                parsed =
                    parseCompactProof
                        (proofKey proof)
                        (proofValue proof)
                        (proofRootHash proof)
                        compact
            case parsed of
                Nothing -> expectationFailure "parse failed"
                Just rt ->
                    computeRootHash hashHashing rt
                        `shouldBe` computeRootHash hashHashing proof

        it "round-trips multi-step proof preserving root hash" $ do
            let proof = multiStepProof
                compact = renderCompactProof proof
                parsed =
                    parseCompactProof
                        (proofKey proof)
                        (proofValue proof)
                        (proofRootHash proof)
                        compact
            case parsed of
                Nothing -> expectationFailure "parse failed"
                Just rt ->
                    computeRootHash hashHashing rt
                        `shouldBe` computeRootHash hashHashing proof

        it "deterministic encoding" $ do
            let proof = multiStepProof
            renderCompactProof proof `shouldBe` renderCompactProof proof

-- | Single-element CSMT proof (no steps)
singleElementProof :: InclusionProof Hash
singleElementProof =
    let key = bsToKey "hello"
        val = mkHash "world"
    in  InclusionProof
            { proofKey = key
            , proofValue = val
            , proofRootHash = val -- will be recomputed by verifier
            , proofSteps = []
            , proofRootJump = key
            }

-- | Multi-step proof with synthetic data
multiStepProof :: InclusionProof Hash
multiStepProof =
    let key = bsToKey "test-key"
        val = mkHash "test-value"
        step1 =
            ProofStep
                { stepConsumed = 5
                , stepSibling =
                    Indirect
                        { jump = [L, R, L]
                        , value = mkHash "sibling1"
                        }
                }
        step2 =
            ProofStep
                { stepConsumed = 3
                , stepSibling =
                    Indirect
                        { jump = [R, R]
                        , value = mkHash "sibling2"
                        }
                }
    in  InclusionProof
            { proofKey = key
            , proofValue = val
            , proofRootHash = mkHash "root" -- synthetic
            , proofSteps = [step1, step2]
            , proofRootJump = [L, R]
            }

bsToKey :: BS.ByteString -> Key
bsToKey = concatMap byteToDirections . BS.unpack
  where
    byteToDirections byte =
        [ if byte `div` (2 ^ i) `mod` 2 == 1 then R else L
        | i <- [7, 6 .. 0 :: Int]
        ]

module CSMT.Hashes.PlutusDataSpec (spec) where

import CSMT.Hashes (hashHashing, mkHash)
import CSMT.Hashes.Compact (renderCompactProof)
import CSMT.Hashes.PlutusData
    ( parsePlutusProof
    , renderPlutusProof
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

spec :: Spec
spec = describe "CSMT.Hashes.PlutusData" $ do
    it "round-trips single-element proof" $ do
        let proof = singleElementProof
            plutus = renderPlutusProof proof
            parsed =
                parsePlutusProof
                    (proofKey proof)
                    (proofValue proof)
                    plutus
        case parsed of
            Nothing -> expectationFailure "parse failed"
            Just rt ->
                computeRootHash hashHashing rt
                    `shouldBe` computeRootHash
                        hashHashing
                        proof

    it "round-trips multi-step proof" $ do
        let proof = multiStepProof
            plutus = renderPlutusProof proof
            parsed =
                parsePlutusProof
                    (proofKey proof)
                    (proofValue proof)
                    plutus
        case parsed of
            Nothing -> expectationFailure "parse failed"
            Just rt ->
                computeRootHash hashHashing rt
                    `shouldBe` computeRootHash
                        hashHashing
                        proof

    it "deterministic encoding" $ do
        let proof = multiStepProof
        renderPlutusProof proof
            `shouldBe` renderPlutusProof proof

    it "size is close to compact CBOR" $ do
        let proof = multiStepProof
            compactSize =
                BS.length (renderCompactProof proof)
            plutusSize =
                BS.length (renderPlutusProof proof)
        putStrLn
            $ "\n  compact CBOR: "
                ++ show compactSize
                ++ " bytes"
        putStrLn
            $ "  Plutus Data:  "
                ++ show plutusSize
                ++ " bytes"
        -- Plutus Data should be within 20% of compact
        plutusSize
            `shouldSatisfy` ( <=
                                compactSize
                                    + compactSize
                                        `div` 5
                            )

    it "starts with Constr 0 tag" $ do
        let proof = multiStepProof
            bs = renderPlutusProof proof
        -- Constr 0 = CBOR tag 121 = 0xd8 0x79
        BS.index bs 0 `shouldBe` 0xd8
        BS.index bs 1 `shouldBe` 0x79

-- | Single-element CSMT proof (no steps)
singleElementProof :: InclusionProof Hash
singleElementProof =
    let key = bsToKey "hello"
        val = mkHash "world"
    in  InclusionProof
            { proofKey = key
            , proofValue = val
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
            , proofSteps = [step1, step2]
            , proofRootJump = [L, R]
            }

bsToKey :: BS.ByteString -> Key
bsToKey = concatMap byteToDirections . BS.unpack
  where
    byteToDirections byte =
        [ if byte
            `div` (2 ^ i)
            `mod` 2
            == 1
            then R
            else L
        | i <- [7, 6 .. 0 :: Int]
        ]

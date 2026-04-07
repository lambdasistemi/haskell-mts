-- |
-- Module      : CSMT.Hashes.CBOR
-- Description : CBOR serialization for CSMT proofs
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- CBOR encoding and decoding for inclusion and exclusion proofs.
module CSMT.Hashes.CBOR
    ( renderProof
    , parseProof
    , renderExclusionProof
    , parseExclusionProof
    )
where

import CSMT.Hashes.Types (Hash (..), renderHash)
import CSMT.Interface (Direction (..), Indirect (..), Key)
import CSMT.Proof.Exclusion (ExclusionProof (..))
import CSMT.Proof.Insertion (InclusionProof (..), ProofStep (..))
import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL

-- | Encode a Direction to CBOR (L = 0, R = 1).
encodeDirection :: Direction -> CBOR.Encoding
encodeDirection L = CBOR.encodeWord 0
encodeDirection R = CBOR.encodeWord 1

-- | Decode a Direction from CBOR.
decodeDirection :: CBOR.Decoder s Direction
decodeDirection = do
    w <- CBOR.decodeWord
    case w of
        0 -> pure L
        1 -> pure R
        _ -> fail "Invalid direction"

-- | Encode a Key (list of Directions) to CBOR.
encodeKey :: Key -> CBOR.Encoding
encodeKey dirs =
    CBOR.encodeListLen (fromIntegral $ length dirs)
        <> foldMap encodeDirection dirs

-- | Decode a Key from CBOR.
decodeKey :: CBOR.Decoder s Key
decodeKey = do
    len <- CBOR.decodeListLen
    replicateM len decodeDirection

-- | Encode an Indirect to CBOR.
encodeIndirect :: Indirect Hash -> CBOR.Encoding
encodeIndirect Indirect{jump, value} =
    CBOR.encodeListLen 2
        <> encodeKey jump
        <> CBOR.encodeBytes (renderHash value)

-- | Decode an Indirect from CBOR.
decodeIndirect :: CBOR.Decoder s (Indirect Hash)
decodeIndirect = do
    _ <- CBOR.decodeListLen
    jump <- decodeKey
    value <- Hash <$> CBOR.decodeBytes
    pure Indirect{jump, value}

-- | Encode a ProofStep to CBOR.
encodeProofStep :: ProofStep Hash -> CBOR.Encoding
encodeProofStep ProofStep{stepConsumed, stepSibling} =
    CBOR.encodeListLen 2
        <> CBOR.encodeInt stepConsumed
        <> encodeIndirect stepSibling

-- | Decode a ProofStep from CBOR.
decodeProofStep :: CBOR.Decoder s (ProofStep Hash)
decodeProofStep = do
    _ <- CBOR.decodeListLen
    stepConsumed <- CBOR.decodeInt
    stepSibling <- decodeIndirect
    pure ProofStep{stepConsumed, stepSibling}

-- | Encode an InclusionProof to CBOR.
encodeProof :: InclusionProof Hash -> CBOR.Encoding
encodeProof
    InclusionProof
        { proofKey
        , proofValue
        , proofSteps
        , proofRootJump
        } =
        CBOR.encodeListLen 4
            <> encodeKey proofKey
            <> CBOR.encodeBytes (renderHash proofValue)
            <> ( CBOR.encodeListLen (fromIntegral $ length proofSteps)
                    <> foldMap encodeProofStep proofSteps
               )
            <> encodeKey proofRootJump

-- | Decode an InclusionProof from CBOR.
decodeProof :: CBOR.Decoder s (InclusionProof Hash)
decodeProof = do
    _ <- CBOR.decodeListLen
    proofKey <- decodeKey
    proofValue <- Hash <$> CBOR.decodeBytes
    stepsLen <- CBOR.decodeListLen
    proofSteps <- replicateM stepsLen decodeProofStep
    proofRootJump <- decodeKey
    pure
        InclusionProof
            { proofKey
            , proofValue
            , proofSteps
            , proofRootJump
            }

-- | Render a proof to a ByteString using CBOR.
renderProof :: InclusionProof Hash -> ByteString
renderProof = BL.toStrict . CBOR.toLazyByteString . encodeProof

-- | Parse a ByteString as a proof. Returns Nothing on parse failure.
parseProof :: ByteString -> Maybe (InclusionProof Hash)
parseProof bs =
    case CBOR.deserialiseFromBytes decodeProof (BL.fromStrict bs) of
        Left _ -> Nothing
        Right (_, pf) -> Just pf

-- -----------------------------------------------------------
-- Exclusion proof CBOR
-- -----------------------------------------------------------

-- | Encode an ExclusionProof to CBOR.
--
-- Format:
--
-- @
-- ExclusionEmpty  → CBOR array [tag=0]
-- ExclusionWitness → CBOR array [tag=1, targetKey, inclusionProof]
-- @
encodeExclusionProof :: ExclusionProof Hash -> CBOR.Encoding
encodeExclusionProof ExclusionEmpty =
    CBOR.encodeListLen 1
        <> CBOR.encodeWord 0
encodeExclusionProof
    ExclusionWitness{epTargetKey, epWitnessProof} =
        CBOR.encodeListLen 3
            <> CBOR.encodeWord 1
            <> encodeKey epTargetKey
            <> encodeProof epWitnessProof

-- | Decode an ExclusionProof from CBOR.
decodeExclusionProof :: CBOR.Decoder s (ExclusionProof Hash)
decodeExclusionProof = do
    _ <- CBOR.decodeListLen
    tag <- CBOR.decodeWord
    case tag of
        0 -> pure ExclusionEmpty
        1 -> do
            epTargetKey <- decodeKey
            epWitnessProof <- decodeProof
            pure ExclusionWitness{epTargetKey, epWitnessProof}
        _ -> fail "Invalid exclusion proof tag"

-- | Render an exclusion proof to CBOR bytes.
renderExclusionProof :: ExclusionProof Hash -> ByteString
renderExclusionProof =
    BL.toStrict . CBOR.toLazyByteString . encodeExclusionProof

-- | Parse CBOR bytes as an exclusion proof.
parseExclusionProof
    :: ByteString -> Maybe (ExclusionProof Hash)
parseExclusionProof bs =
    case CBOR.deserialiseFromBytes decodeExclusionProof (BL.fromStrict bs) of
        Left _ -> Nothing
        Right (_, pf) -> Just pf

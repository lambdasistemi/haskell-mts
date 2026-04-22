-- |
-- Module      : CSMT.Core.CBOR
-- Description : CBOR encoding/decoding for CSMT proofs
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- CBOR parsers and encoders for inclusion and exclusion proofs.
-- Shared wire format — the @csmt@ write side and the @csmt-verify@
-- verifier both round-trip through these codecs, so bytes produced
-- by the server are exactly the bytes the verifier consumes.
module CSMT.Core.CBOR
    ( renderProof
    , parseProof
    , renderExclusionProof
    , parseExclusionProof
    ) where

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL

import CSMT.Core.Exclusion
    ( ExclusionProof (..)
    )
import CSMT.Core.Hash
    ( Hash (..)
    , renderHash
    )
import CSMT.Core.Proof
    ( InclusionProof (..)
    , ProofStep (..)
    )
import CSMT.Core.Types
    ( Direction (..)
    , Indirect (..)
    , Key
    )

encodeDirection :: Direction -> CBOR.Encoding
encodeDirection L = CBOR.encodeWord 0
encodeDirection R = CBOR.encodeWord 1

decodeDirection :: CBOR.Decoder s Direction
decodeDirection = do
    w <- CBOR.decodeWord
    case w of
        0 -> pure L
        1 -> pure R
        _ -> fail "Invalid direction"

encodeKey :: Key -> CBOR.Encoding
encodeKey dirs =
    CBOR.encodeListLen (fromIntegral $ length dirs)
        <> foldMap encodeDirection dirs

decodeKey :: CBOR.Decoder s Key
decodeKey = do
    len <- CBOR.decodeListLen
    replicateM len decodeDirection

encodeIndirect :: Indirect Hash -> CBOR.Encoding
encodeIndirect Indirect{jump, value} =
    CBOR.encodeListLen 2
        <> encodeKey jump
        <> CBOR.encodeBytes (renderHash value)

decodeIndirect :: CBOR.Decoder s (Indirect Hash)
decodeIndirect = do
    _ <- CBOR.decodeListLen
    jump <- decodeKey
    value <- Hash <$> CBOR.decodeBytes
    pure Indirect{jump, value}

encodeProofStep :: ProofStep Hash -> CBOR.Encoding
encodeProofStep ProofStep{stepConsumed, stepSibling} =
    CBOR.encodeListLen 2
        <> CBOR.encodeInt stepConsumed
        <> encodeIndirect stepSibling

decodeProofStep :: CBOR.Decoder s (ProofStep Hash)
decodeProofStep = do
    _ <- CBOR.decodeListLen
    stepConsumed <- CBOR.decodeInt
    stepSibling <- decodeIndirect
    pure ProofStep{stepConsumed, stepSibling}

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
            <> ( CBOR.encodeListLen
                    (fromIntegral $ length proofSteps)
                    <> foldMap encodeProofStep proofSteps
               )
            <> encodeKey proofRootJump

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

-- | Render an inclusion proof as CBOR bytes.
renderProof :: InclusionProof Hash -> ByteString
renderProof =
    BL.toStrict . CBOR.toLazyByteString . encodeProof

-- | Parse CBOR bytes as an inclusion proof.
parseProof :: ByteString -> Maybe (InclusionProof Hash)
parseProof bs =
    case CBOR.deserialiseFromBytes decodeProof (BL.fromStrict bs) of
        Left _ -> Nothing
        Right (_, pf) -> Just pf

encodeExclusionProof :: ExclusionProof Hash -> CBOR.Encoding
encodeExclusionProof ExclusionEmpty =
    CBOR.encodeListLen 1 <> CBOR.encodeWord 0
encodeExclusionProof
    ExclusionWitness{epTargetKey, epWitnessProof} =
        CBOR.encodeListLen 3
            <> CBOR.encodeWord 1
            <> encodeKey epTargetKey
            <> encodeProof epWitnessProof

decodeExclusionProof :: CBOR.Decoder s (ExclusionProof Hash)
decodeExclusionProof = do
    _ <- CBOR.decodeListLen
    tag <- CBOR.decodeWord
    case tag of
        0 -> pure ExclusionEmpty
        1 -> do
            epTargetKey <- decodeKey
            epWitnessProof <- decodeProof
            pure
                ExclusionWitness
                    { epTargetKey
                    , epWitnessProof
                    }
        _ -> fail "Invalid exclusion proof tag"

-- | Render an exclusion proof as CBOR bytes.
renderExclusionProof :: ExclusionProof Hash -> ByteString
renderExclusionProof =
    BL.toStrict
        . CBOR.toLazyByteString
        . encodeExclusionProof

-- | Parse CBOR bytes as an exclusion proof.
parseExclusionProof
    :: ByteString -> Maybe (ExclusionProof Hash)
parseExclusionProof bs =
    case CBOR.deserialiseFromBytes
        decodeExclusionProof
        (BL.fromStrict bs) of
        Left _ -> Nothing
        Right (_, pf) -> Just pf

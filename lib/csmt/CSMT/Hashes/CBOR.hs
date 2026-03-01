-- |
-- Module      : CSMT.Hashes.CBOR
-- Description : CBOR serialization for inclusion proofs
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- CBOR encoding and decoding for 'InclusionProof' and related types.
module CSMT.Hashes.CBOR
    ( renderProof
    , parseProof
    )
where

import CSMT.Hashes.Types (Hash (..), renderHash)
import CSMT.Interface (Direction (..), Indirect (..), Key)
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
        , proofRootHash
        , proofSteps
        , proofRootJump
        } =
        CBOR.encodeListLen 5
            <> encodeKey proofKey
            <> CBOR.encodeBytes (renderHash proofValue)
            <> CBOR.encodeBytes (renderHash proofRootHash)
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
    proofRootHash <- Hash <$> CBOR.decodeBytes
    stepsLen <- CBOR.decodeListLen
    proofSteps <- replicateM stepsLen decodeProofStep
    proofRootJump <- decodeKey
    pure
        InclusionProof
            { proofKey
            , proofValue
            , proofRootHash
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

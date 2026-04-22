-- |
-- Module      : MPF.Hashes.CBOR
-- Description : CBOR serialization for MPF inclusion proofs
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- CBOR encoding and decoding for 'MPFProof' and related types.
module MPF.Hashes.CBOR
    ( renderProof
    , parseProof
    )
where

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import MPF.Hashes.Types (MPFHash (..), renderMPFHash)
import MPF.Interface (HexDigit (..), HexKey)
import MPF.Proof.Insertion (MPFProof (..), MPFProofStep (..))

-- | Encode a HexDigit to CBOR (0-15).
encodeHexDigit :: HexDigit -> CBOR.Encoding
encodeHexDigit (HexDigit d) = CBOR.encodeWord8 d

-- | Decode a HexDigit from CBOR.
decodeHexDigit :: CBOR.Decoder s HexDigit
decodeHexDigit = do
    w <- CBOR.decodeWord8
    if w < 16
        then pure (HexDigit w)
        else fail "Invalid hex digit"

-- | Encode a HexKey to CBOR.
encodeHexKey :: HexKey -> CBOR.Encoding
encodeHexKey digits =
    CBOR.encodeListLen (fromIntegral $ length digits)
        <> foldMap encodeHexDigit digits

-- | Decode a HexKey from CBOR.
decodeHexKey :: CBOR.Decoder s HexKey
decodeHexKey = do
    len <- CBOR.decodeListLen
    replicateM len decodeHexDigit

-- | Encode a proof step to CBOR.
-- Tag byte: 0=Leaf, 1=Fork, 2=Branch
encodeProofStep :: MPFProofStep MPFHash -> CBOR.Encoding
encodeProofStep ProofStepLeaf{..} =
    CBOR.encodeWord8 0
        <> encodeHexKey pslBranchJump
        <> encodeHexDigit pslOurPosition
        <> encodeHexKey pslNeighborKeyPath
        <> encodeHexDigit pslNeighborNibble
        <> encodeHexKey pslNeighborSuffix
        <> CBOR.encodeBytes (renderMPFHash pslNeighborValueDigest)
encodeProofStep ProofStepFork{..} =
    CBOR.encodeWord8 1
        <> encodeHexKey psfBranchJump
        <> encodeHexDigit psfOurPosition
        <> encodeHexKey psfNeighborPrefix
        <> encodeHexDigit psfNeighborIndex
        <> CBOR.encodeBytes (renderMPFHash psfMerkleRoot)
encodeProofStep ProofStepBranch{..} =
    CBOR.encodeWord8 2
        <> encodeHexKey psbJump
        <> encodeHexDigit psbPosition
        <> ( CBOR.encodeListLen
                (fromIntegral $ length psbSiblingHashes)
                <> foldMap encodeSibling psbSiblingHashes
           )
  where
    encodeSibling (d, h) =
        CBOR.encodeListLen 2
            <> encodeHexDigit d
            <> CBOR.encodeBytes (renderMPFHash h)

-- | Decode a proof step from CBOR.
decodeProofStep :: CBOR.Decoder s (MPFProofStep MPFHash)
decodeProofStep = do
    tag <- CBOR.decodeWord8
    case tag of
        0 -> do
            pslBranchJump <- decodeHexKey
            pslOurPosition <- decodeHexDigit
            pslNeighborKeyPath <- decodeHexKey
            pslNeighborNibble <- decodeHexDigit
            pslNeighborSuffix <- decodeHexKey
            pslNeighborValueDigest <- MPFHash <$> CBOR.decodeBytes
            pure ProofStepLeaf{..}
        1 -> do
            psfBranchJump <- decodeHexKey
            psfOurPosition <- decodeHexDigit
            psfNeighborPrefix <- decodeHexKey
            psfNeighborIndex <- decodeHexDigit
            psfMerkleRoot <- MPFHash <$> CBOR.decodeBytes
            pure ProofStepFork{..}
        2 -> do
            psbJump <- decodeHexKey
            psbPosition <- decodeHexDigit
            len <- CBOR.decodeListLen
            psbSiblingHashes <- replicateM len decodeSibling
            pure ProofStepBranch{..}
        _ -> fail "Invalid proof step tag"
  where
    decodeSibling = do
        _ <- CBOR.decodeListLen
        d <- decodeHexDigit
        h <- MPFHash <$> CBOR.decodeBytes
        pure (d, h)

-- | Encode an MPFProof to CBOR.
encodeProof :: MPFProof MPFHash -> CBOR.Encoding
encodeProof MPFProof{..} =
    CBOR.encodeListLen 4
        <> ( CBOR.encodeListLen
                (fromIntegral $ length mpfProofSteps)
                <> foldMap encodeProofStep mpfProofSteps
           )
        <> encodeHexKey mpfProofRootPrefix
        <> encodeHexKey mpfProofLeafSuffix
        <> CBOR.encodeBytes (renderMPFHash mpfProofValueHash)

-- | Decode an MPFProof from CBOR.
decodeProof :: CBOR.Decoder s (MPFProof MPFHash)
decodeProof = do
    _ <- CBOR.decodeListLen
    stepsLen <- CBOR.decodeListLen
    mpfProofSteps <- replicateM stepsLen decodeProofStep
    mpfProofRootPrefix <- decodeHexKey
    mpfProofLeafSuffix <- decodeHexKey
    mpfProofValueHash <- MPFHash <$> CBOR.decodeBytes
    pure MPFProof{..}

-- | Render a proof to a ByteString using CBOR.
renderProof :: MPFProof MPFHash -> ByteString
renderProof = BL.toStrict . CBOR.toLazyByteString . encodeProof

-- | Parse a ByteString as a proof. Returns Nothing on parse failure.
parseProof :: ByteString -> Maybe (MPFProof MPFHash)
parseProof bs =
    case CBOR.deserialiseFromBytes decodeProof (BL.fromStrict bs) of
        Left _ -> Nothing
        Right (_, pf) -> Just pf

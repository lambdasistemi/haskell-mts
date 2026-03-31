{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : CSMT.Hashes.Compact
-- Description : Compact CBOR serialization for CSMT inclusion proofs
--
-- Minimal proof encoding that strips redundant fields (key, value, root
-- hash) that the verifier already has. Directions are packed as bits
-- instead of individual words.
--
-- Format:
--
-- @
-- CBOR array: [rootJumpPacked, step1, step2, ...]
--   rootJumpPacked = bytes(2-byte-bitcount ++ packed-bits)
--   step = [int(consumed), bytes(siblingJumpPacked), bytes(siblingHash)]
-- @
module CSMT.Hashes.Compact
    ( renderCompactProof
    , parseCompactProof
    , packKey
    , unpackKey
    )
where

import CSMT.Hashes.Types (Hash (..), renderHash)
import CSMT.Interface (Direction (..), Indirect (..), Key)
import CSMT.Proof.Insertion (InclusionProof (..), ProofStep (..))
import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Data.Bits (setBit, testBit)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Word (Word8)

-- | Pack a Key (list of Directions) into a compact bytestring.
-- Format: 2-byte big-endian bit count, then bits packed MSB-first.
packKey :: Key -> ByteString
packKey dirs =
    let len = length dirs
    in  BS.pack
            [ fromIntegral (len `div` 256)
            , fromIntegral (len `mod` 256)
            ]
            <> packBits dirs

-- | Unpack a compact bytestring back to a Key.
unpackKey :: ByteString -> Maybe Key
unpackKey bs
    | BS.length bs < 2 = Nothing
    | otherwise =
        let hi = fromIntegral (BS.index bs 0)
            lo = fromIntegral (BS.index bs 1)
            len = hi * 256 + lo :: Int
            bitBytes = BS.drop 2 bs
            expectedBytes = (len + 7) `div` 8
        in  if BS.length bitBytes < expectedBytes
                then Nothing
                else Just $ unpackBits len bitBytes

-- | Pack directions into bytes, MSB first.
packBits :: [Direction] -> ByteString
packBits = BS.pack . go
  where
    go [] = []
    go dirs =
        let (chunk, rest) = splitAt 8 dirs
            byte =
                foldl
                    ( \acc (i, d) -> case d of
                        R -> setBit acc (7 - i)
                        L -> acc
                    )
                    (0 :: Word8)
                    (zip [0 ..] chunk)
        in  byte : go rest

-- | Unpack n bits from bytes.
unpackBits :: Int -> ByteString -> [Direction]
unpackBits n bs = take n $ go 0
  where
    go byteIdx
        | byteIdx >= BS.length bs = []
        | otherwise =
            let byte = BS.index bs byteIdx
                dirs = [if testBit byte (7 - i) then R else L | i <- [0 .. 7]]
            in  dirs ++ go (byteIdx + 1)

-- | Encode a compact proof step.
encodeCompactStep :: ProofStep Hash -> CBOR.Encoding
encodeCompactStep ProofStep{stepConsumed, stepSibling = Indirect{jump, value}} =
    CBOR.encodeListLen 3
        <> CBOR.encodeInt stepConsumed
        <> CBOR.encodeBytes (packKey jump)
        <> CBOR.encodeBytes (renderHash value)

-- | Decode a compact proof step.
decodeCompactStep :: CBOR.Decoder s (ProofStep Hash)
decodeCompactStep = do
    _ <- CBOR.decodeListLen
    stepConsumed <- CBOR.decodeInt
    jumpBs <- CBOR.decodeBytes
    case unpackKey jumpBs of
        Nothing -> fail "invalid packed key in proof step"
        Just jump -> do
            value <- Hash <$> CBOR.decodeBytes
            pure ProofStep{stepConsumed, stepSibling = Indirect{jump, value}}

-- | Render a compact proof. Only encodes root jump and steps.
-- The verifier supplies key, value, and root hash externally.
renderCompactProof :: InclusionProof Hash -> ByteString
renderCompactProof InclusionProof{proofSteps, proofRootJump} =
    BL.toStrict
        $ CBOR.toLazyByteString
        $ CBOR.encodeListLen (fromIntegral $ 1 + length proofSteps)
            <> CBOR.encodeBytes (packKey proofRootJump)
            <> foldMap encodeCompactStep proofSteps

-- | Parse a compact proof. The caller supplies key, value, and root
-- hash to reconstruct the full InclusionProof.
parseCompactProof
    :: Key
    -> Hash
    -> Hash
    -> ByteString
    -> Maybe (InclusionProof Hash)
parseCompactProof proofKey proofValue proofRootHash bs =
    case CBOR.deserialiseFromBytes decoder' (BL.fromStrict bs) of
        Left _ -> Nothing
        Right (_, proof) -> Just proof
  where
    decoder' :: CBOR.Decoder s (InclusionProof Hash)
    decoder' = do
        len <- CBOR.decodeListLen
        rootJumpBs <- CBOR.decodeBytes
        case unpackKey rootJumpBs of
            Nothing -> fail "invalid root jump"
            Just proofRootJump -> do
                proofSteps <- mapM (const decodeCompactStep) [2 .. len]
                pure
                    InclusionProof
                        { proofKey
                        , proofValue
                        , proofRootHash
                        , proofSteps
                        , proofRootJump
                        }

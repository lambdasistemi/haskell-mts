-- |
-- Module      : CSMT.Hashes.PlutusData
-- Description : Plutus Data encoding of CSMT inclusion proofs
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- Encode CSMT inclusion proofs as Plutus Data CBOR for on-chain
-- Aiken verification. The format strips fields the verifier
-- already has (key, value, root hash) and uses Plutus Data
-- constructors.
--
-- Format:
--
-- @
-- Proof = Constr 0 [rootJump, step1, step2, ...]
--   rootJump = B\<packed-key\>   (2-byte bitcount + packed bits)
--   stepN    = Constr 0 [I\<consumed\>, B\<sibJump\>, B\<sibHash\>]
-- @
--
-- Constructor tags follow the Plutus Data convention:
-- Constr 0 = CBOR tag 121.
-- All lists use indefinite-length encoding.
module CSMT.Hashes.PlutusData
    ( renderPlutusProof
    , parsePlutusProof
    )
where

import CSMT.Hashes.Compact (packKey, unpackKey)
import CSMT.Hashes.Types (Hash (..), renderHash)
import CSMT.Interface (Indirect (..), Key)
import CSMT.Proof.Insertion
    ( InclusionProof (..)
    , ProofStep (..)
    )
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BL
import Data.Word (Word8)

-- -----------------------------------------------------------
-- CBOR primitives (Plutus Data subset)
-- -----------------------------------------------------------

-- | CBOR tag for Constr 0 = tag 121
constr0Tag :: Builder.Builder
constr0Tag =
    Builder.word8 0xd8 <> Builder.word8 0x79

-- | Begin indefinite-length CBOR list
listBegin :: Builder.Builder
listBegin = Builder.word8 0x9f

-- | CBOR break byte
cborBreak :: Builder.Builder
cborBreak = Builder.word8 0xff

-- | Encode a non-negative integer in CBOR (major type 0)
cborUInt :: Int -> Builder.Builder
cborUInt n
    | n < 24 = Builder.word8 (fromIntegral n)
    | n < 256 =
        Builder.word8 0x18
            <> Builder.word8 (fromIntegral n)
    | n < 65536 =
        Builder.word8 0x19
            <> Builder.word8
                (fromIntegral (n `div` 256))
            <> Builder.word8
                (fromIntegral (n `mod` 256))
    | otherwise = error "cborUInt: value too large"

-- | Encode a definite-length CBOR bytestring
cborBytes :: ByteString -> Builder.Builder
cborBytes bs =
    let len = B.length bs
    in  if len < 24
            then
                Builder.word8
                    (0x40 + fromIntegral len)
                    <> Builder.byteString bs
            else
                if len < 256
                    then
                        Builder.word8 0x58
                            <> Builder.word8
                                (fromIntegral len)
                            <> Builder.byteString bs
                    else
                        Builder.word8 0x59
                            <> Builder.word8
                                ( fromIntegral
                                    (len `div` 256)
                                )
                            <> Builder.word8
                                ( fromIntegral
                                    (len `mod` 256)
                                )
                            <> Builder.byteString bs

-- -----------------------------------------------------------
-- Encoding
-- -----------------------------------------------------------

-- | Encode a proof step as Plutus Data.
--
-- @Constr 0 [consumed, sibJump, sibHash]@
encodeStep :: ProofStep Hash -> Builder.Builder
encodeStep
    ProofStep
        { stepConsumed
        , stepSibling =
            Indirect{jump, value}
        } =
        constr0Tag
            <> listBegin
            <> cborUInt stepConsumed
            <> cborBytes (packKey jump)
            <> cborBytes (renderHash value)
            <> cborBreak

-- | Render a CSMT inclusion proof as Plutus Data CBOR.
--
-- Strips key, value, and root hash (the verifier has them).
-- The caller supplies those when parsing back.
renderPlutusProof :: InclusionProof Hash -> ByteString
renderPlutusProof
    InclusionProof{proofSteps, proofRootJump} =
        BL.toStrict
            $ Builder.toLazyByteString
            $ constr0Tag
                <> listBegin
                <> cborBytes (packKey proofRootJump)
                <> foldMap encodeStep proofSteps
                <> cborBreak

-- -----------------------------------------------------------
-- Decoding
-- -----------------------------------------------------------

-- | Simple parser type
type Parser a = ByteString -> Maybe (a, ByteString)

-- | Parse a single byte
parseByte :: Parser Word8
parseByte bs = case B.uncons bs of
    Just (w, rest) -> Just (w, rest)
    Nothing -> Nothing

-- | Expect a specific byte
expectByte :: Word8 -> Parser ()
expectByte expected bs = case parseByte bs of
    Just (w, rest) | w == expected -> Just ((), rest)
    _ -> Nothing

-- | Parse CBOR unsigned integer
parseUInt :: Parser Int
parseUInt bs = case parseByte bs of
    Just (w, rest)
        | w < 24 -> Just (fromIntegral w, rest)
        | w == 0x18 -> case parseByte rest of
            Just (v, rest') ->
                Just (fromIntegral v, rest')
            Nothing -> Nothing
        | w == 0x19 -> do
            (hi, rest1) <- parseByte rest
            (lo, rest2) <- parseByte rest1
            Just
                ( fromIntegral hi * 256
                    + fromIntegral lo
                , rest2
                )
    _ -> Nothing

-- | Parse definite-length CBOR bytestring
parseDefBytes :: Parser ByteString
parseDefBytes bs = case parseByte bs of
    Just (w, rest)
        | w >= 0x40 && w <= 0x57 ->
            takeN (fromIntegral (w - 0x40)) rest
        | w == 0x58 -> case parseByte rest of
            Just (len, rest') ->
                takeN (fromIntegral len) rest'
            Nothing -> Nothing
        | w == 0x59 -> do
            (hi, rest1) <- parseByte rest
            (lo, rest2) <- parseByte rest1
            let len =
                    fromIntegral hi * 256
                        + fromIntegral lo
            takeN len rest2
    _ -> Nothing

-- | Take N bytes
takeN :: Int -> Parser ByteString
takeN n bs
    | B.length bs >= n =
        Just (B.take n bs, B.drop n bs)
    | otherwise = Nothing

-- | Parse Constr 0 tag (0xd8 0x79)
parseConstr0 :: Parser ()
parseConstr0 bs = do
    ((), bs1) <- expectByte 0xd8 bs
    expectByte 0x79 bs1

-- | Parse list begin (0x9f)
parseListBegin :: Parser ()
parseListBegin = expectByte 0x9f

-- | Parse break (0xff)
parseBreak :: Parser ()
parseBreak = expectByte 0xff

-- | Parse a proof step
parseStep :: Parser (ProofStep Hash)
parseStep bs = do
    ((), bs1) <- parseConstr0 bs
    ((), bs2) <- parseListBegin bs1
    (stepConsumed, bs3) <- parseUInt bs2
    (jumpBs, bs4) <- parseDefBytes bs3
    jump <- unpackKey jumpBs
    (hashBs, bs5) <- parseDefBytes bs4
    ((), bs6) <- parseBreak bs5
    Just
        ( ProofStep
            { stepConsumed
            , stepSibling =
                Indirect{jump, value = Hash hashBs}
            }
        , bs6
        )

-- | Parse a Plutus Data CSMT proof. The caller supplies key
-- and value to reconstruct the full proof.
parsePlutusProof
    :: Key
    -> Hash
    -> ByteString
    -> Maybe (InclusionProof Hash)
parsePlutusProof proofKey proofValue bs = do
    ((), bs1) <- parseConstr0 bs
    ((), bs2) <- parseListBegin bs1
    (rootJumpBs, bs3) <- parseDefBytes bs2
    proofRootJump <- unpackKey rootJumpBs
    (proofSteps, bs4) <- collectSteps [] bs3
    ((), bs5) <- parseBreak bs4
    if B.null bs5
        then
            Just
                InclusionProof
                    { proofKey
                    , proofValue
                    , proofSteps
                    , proofRootJump
                    }
        else Nothing
  where
    collectSteps acc bs' = case B.uncons bs' of
        Just (0xff, _) -> Just (reverse acc, bs')
        _ -> do
            (step, rest) <- parseStep bs'
            collectSteps (step : acc) rest

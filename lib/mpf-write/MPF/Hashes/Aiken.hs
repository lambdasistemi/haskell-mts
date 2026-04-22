-- |
-- Module      : MPF.Hashes.Aiken
-- Description : Aiken-compatible CBOR serialization for MPF proofs
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- Plutus Data encoding of MPF proof steps, matching
-- the on-chain Aiken format byte-for-byte.
--
-- Constructor tags (Plutus Data):
--
-- * Branch = Constr 0 = CBOR tag 121
-- * Fork   = Constr 1 = CBOR tag 122
-- * Leaf   = Constr 2 = CBOR tag 123
-- * Neighbor (inside Fork) = Constr 0 = CBOR tag 121
--
-- All lists use indefinite-length encoding.
-- Branch neighbors use indefinite-length bytestring (2 × 64-byte chunks).
module MPF.Hashes.Aiken
    ( renderAikenProof
    , parseAikenProof
    )
where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Word (Word8)
import MPF.Hashes (merkleProof, packHexKey)
import MPF.Hashes.Types (MPFHash (..), renderMPFHash)
import MPF.Interface (HexDigit (..), HexKey)
import MPF.Proof.Insertion (MPFProofStep (..))

-- -------------------------------------------------------------------
-- Low-level CBOR primitives (Plutus Data subset)
-- -------------------------------------------------------------------

-- | CBOR tag for Constr N: tags 121-127 map to Constr 0-6
cborTag :: Word -> Builder.Builder
cborTag n
    | n < 24 = Builder.word8 (0xc0 + fromIntegral n)
    | n < 256 = Builder.word8 0xd8 <> Builder.word8 (fromIntegral n)
    | otherwise =
        Builder.word8 0xd9
            <> Builder.word8 (fromIntegral (n `div` 256))
            <> Builder.word8 (fromIntegral (n `mod` 256))

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
    | n < 256 = Builder.word8 0x18 <> Builder.word8 (fromIntegral n)
    | n < 65536 =
        Builder.word8 0x19
            <> Builder.word8 (fromIntegral (n `div` 256))
            <> Builder.word8 (fromIntegral (n `mod` 256))
    | otherwise = error "cborUInt: value too large"

-- | Encode a definite-length CBOR bytestring
cborBytes :: ByteString -> Builder.Builder
cborBytes bs =
    let len = B.length bs
    in  if len < 24
            then Builder.word8 (0x40 + fromIntegral len) <> Builder.byteString bs
            else
                if len < 256
                    then
                        Builder.word8 0x58
                            <> Builder.word8 (fromIntegral len)
                            <> Builder.byteString bs
                    else
                        Builder.word8 0x59
                            <> Builder.word8 (fromIntegral (len `div` 256))
                            <> Builder.word8 (fromIntegral (len `mod` 256))
                            <> Builder.byteString bs

-- | Encode an indefinite-length CBOR bytestring from chunks
cborBytesIndef :: [ByteString] -> Builder.Builder
cborBytesIndef chunks =
    Builder.word8 0x5f
        <> foldMap cborBytes chunks
        <> cborBreak

-- -------------------------------------------------------------------
-- Aiken proof step encoding
-- -------------------------------------------------------------------

-- | Encode a Branch step.
--
-- @skip@ = length of the branch jump prefix.
-- @neighbors@ = 128 bytes (4 × 32-byte merkle proof hashes),
-- split into two 64-byte chunks in an indefinite bytestring.
encodeBranch
    :: HexKey -> HexDigit -> [(HexDigit, MPFHash)] -> Builder.Builder
encodeBranch branchJump (HexDigit pos) siblingHashes =
    cborTag 121
        <> listBegin
        <> cborUInt (length branchJump)
        <> cborBytesIndef [B.take 64 neighbors, B.drop 64 neighbors]
        <> cborBreak
  where
    sibMap = Map.fromList siblingHashes
    sparseChildren =
        [ if n == pos
            then Nothing
            else Map.lookup (HexDigit n) sibMap
        | n <- [0 .. 15]
        ]
    neighbors =
        B.concat
            $ map renderMPFHash
            $ merkleProof sparseChildren (fromIntegral pos)

-- | Encode a Fork step.
--
-- @skip@ = length of the fork's branch jump prefix.
-- Inner Neighbor: nibble (int), prefix (packed nibbles), root (32 bytes).
encodeFork
    :: HexKey -> HexDigit -> HexKey -> MPFHash -> Builder.Builder
encodeFork branchJump (HexDigit nibble) neighborPrefix neighborRoot =
    cborTag 122
        <> listBegin
        <> cborUInt (length branchJump)
        <> cborTag 121
        <> listBegin
        <> cborUInt (fromIntegral nibble)
        <> cborBytes (packHexKey neighborPrefix)
        <> cborBytes (renderMPFHash neighborRoot)
        <> cborBreak
        <> cborBreak

-- | Encode a Leaf step.
--
-- @skip@ = length of the leaf's branch jump prefix.
-- @key@ = 32-byte full key path (hash of neighbor key).
-- @value@ = 32-byte neighbor value hash.
encodeLeaf
    :: HexKey -> ByteString -> MPFHash -> Builder.Builder
encodeLeaf branchJump neighborKeyBS neighborValueDigest =
    cborTag 123
        <> listBegin
        <> cborUInt (length branchJump)
        <> cborBytes neighborKeyBS
        <> cborBytes (renderMPFHash neighborValueDigest)
        <> cborBreak

-- | Encode a proof step to Aiken CBOR format.
encodeAikenStep :: MPFProofStep MPFHash -> Builder.Builder
encodeAikenStep ProofStepBranch{..} =
    encodeBranch psbJump psbPosition psbSiblingHashes
encodeAikenStep ProofStepFork{..} =
    encodeFork
        psfBranchJump
        psfNeighborIndex
        psfNeighborPrefix
        psfMerkleRoot
encodeAikenStep ProofStepLeaf{..} =
    encodeLeaf
        pslBranchJump
        (packFullKeyPath pslNeighborKeyPath)
        pslNeighborValueDigest
  where
    -- The neighbor key path is the full path through the trie.
    -- For Aiken encoding, this gets packed into 32 bytes
    -- (64 nibbles = 32 bytes for a blake2b-256 hash).
    packFullKeyPath :: HexKey -> ByteString
    packFullKeyPath = packHexKey

-- | Render a list of proof steps to Aiken-compatible CBOR.
--
-- The top-level proof is just the list of steps
-- (no rootPrefix/leafSuffix/valueHash — those are passed
-- separately on-chain).
--
-- Steps are reversed because 'MPFProof' stores them bottom-up
-- (leaf→root) while the Aiken format is top-down (root→leaf).
renderAikenProof :: [MPFProofStep MPFHash] -> ByteString
renderAikenProof steps =
    BL.toStrict
        $ Builder.toLazyByteString
        $ listBegin
            <> foldMap encodeAikenStep (reverse steps)
            <> cborBreak

-- -------------------------------------------------------------------
-- Aiken proof step decoding
-- -------------------------------------------------------------------

-- | Parse Aiken-compatible CBOR proof bytes.
parseAikenProof :: ByteString -> Maybe [MPFProofStep MPFHash]
parseAikenProof bs = case parseBytes bs of
    Just (steps, rest) | B.null rest -> Just steps
    _ -> Nothing

-- | Simple CBOR parser state: remaining bytes
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

-- | Parse a CBOR unsigned integer (major type 0)
parseUInt :: Parser Int
parseUInt bs = case parseByte bs of
    Just (w, rest)
        | w < 24 -> Just (fromIntegral w, rest)
        | w == 0x18 -> case parseByte rest of
            Just (v, rest') -> Just (fromIntegral v, rest')
            Nothing -> Nothing
        | w == 0x19 -> do
            (bytes, rest') <- takeN 2 rest
            let hi = B.index bytes 0
                lo = B.index bytes 1
            Just (fromIntegral hi * 256 + fromIntegral lo, rest')
    _ -> Nothing

-- | Parse definite-length CBOR bytestring
parseDefBytes :: Parser ByteString
parseDefBytes bs = case parseByte bs of
    Just (w, rest)
        | w >= 0x40 && w <= 0x57 ->
            takeN (fromIntegral (w - 0x40)) rest
        | w == 0x58 -> case parseByte rest of
            Just (len, rest') -> takeN (fromIntegral len) rest'
            Nothing -> Nothing
        | w == 0x59 -> do
            (bytes, rest') <- takeN 2 rest
            let hi = B.index bytes 0
                lo = B.index bytes 1
                len = fromIntegral hi * 256 + fromIntegral lo
            takeN len rest'
    _ -> Nothing

-- | Parse indefinite-length CBOR bytestring (0x5f chunks... 0xff)
parseIndefBytes :: Parser ByteString
parseIndefBytes bs = case expectByte 0x5f bs of
    Just ((), rest) -> collectChunks [] rest
    Nothing -> Nothing
  where
    collectChunks acc bs' = case parseByte bs' of
        Just (0xff, rest) -> Just (B.concat (reverse acc), rest)
        _ -> case parseDefBytes bs' of
            Just (chunk, rest) -> collectChunks (chunk : acc) rest
            Nothing -> Nothing

-- | Parse CBOR bytes (either definite or indefinite)
parseCBORBytes :: Parser ByteString
parseCBORBytes bs = case B.uncons bs of
    Just (0x5f, _) -> parseIndefBytes bs
    _ -> parseDefBytes bs

-- | Take N bytes
takeN :: Int -> Parser ByteString
takeN n bs
    | B.length bs >= n = Just (B.take n bs, B.drop n bs)
    | otherwise = Nothing

-- | Parse CBOR tag
parseTag :: Parser Word
parseTag bs = case parseByte bs of
    Just (0xd8, rest) -> case parseByte rest of
        Just (v, rest') -> Just (fromIntegral v, rest')
        Nothing -> Nothing
    Just (0xd9, rest) -> do
        (bytes, rest') <- takeN 2 rest
        let hi = B.index bytes 0
            lo = B.index bytes 1
        Just (fromIntegral hi * 256 + fromIntegral lo, rest')
    _ -> Nothing

-- | Parse indefinite list begin
parseListBegin :: Parser ()
parseListBegin = expectByte 0x9f

-- | Parse break byte
parseBreak :: Parser ()
parseBreak = expectByte 0xff

-- | Unpack a full key path from packed bytes to HexKey
unpackFullKeyPath :: ByteString -> HexKey
unpackFullKeyPath = concatMap byteToNibbles . B.unpack
  where
    byteToNibbles :: Word8 -> [HexDigit]
    byteToNibbles b =
        [ HexDigit (b `div` 16)
        , HexDigit (b `mod` 16)
        ]

-- | Unpack nibble-packed prefix to HexKey
unpackNibblePrefix :: ByteString -> HexKey
unpackNibblePrefix = unpackFullKeyPath

-- | Parse a Branch step (after tag 121 and list begin)
parseBranchStep :: Parser (MPFProofStep MPFHash)
parseBranchStep bs = do
    (skip, bs1) <- parseUInt bs
    (neighborsBS, bs2) <- parseCBORBytes bs1
    ((), bs3) <- parseBreak bs2
    let branchJump = replicate skip (HexDigit 0)
        -- Decode the 128-byte neighbors into 4 × 32-byte hashes
        proofHashes = splitHashes neighborsBS
        -- Reconstruct sibling hashes from proof hashes.
        -- The proof hashes are the 4 levels of the merkle proof,
        -- but we can't fully reconstruct the 16-element sparse
        -- array from just 4 hashes. Store them as-is for now.
        -- For round-tripping we store the raw neighbors.
        siblingHashes = zipWith (\i h -> (HexDigit i, h)) [0 ..] proofHashes
    Just
        ( ProofStepBranch
            { psbJump = branchJump
            , psbPosition = HexDigit 0
            , psbSiblingHashes = siblingHashes
            }
        , bs3
        )

-- | Split 128 bytes into 4 × 32-byte hashes
splitHashes :: ByteString -> [MPFHash]
splitHashes bs
    | B.length bs >= 128 =
        [ MPFHash (B.take 32 bs)
        , MPFHash (B.take 32 (B.drop 32 bs))
        , MPFHash (B.take 32 (B.drop 64 bs))
        , MPFHash (B.take 32 (B.drop 96 bs))
        ]
    | otherwise = []

-- | Parse a Fork step (after tag 122 and list begin)
parseForkStep :: Parser (MPFProofStep MPFHash)
parseForkStep bs = do
    (skip, bs1) <- parseUInt bs
    -- Parse Neighbor (tag 121)
    (tag, bs2) <- parseTag bs1
    when (tag /= 121) Nothing
    ((), bs3) <- parseListBegin bs2
    (nibble, bs4) <- parseUInt bs3
    (prefixBS, bs5) <- parseCBORBytes bs4
    (rootBS, bs6) <- parseCBORBytes bs5
    ((), bs7) <- parseBreak bs6
    ((), bs8) <- parseBreak bs7
    let branchJump = replicate skip (HexDigit 0)
    Just
        ( ProofStepFork
            { psfBranchJump = branchJump
            , psfOurPosition = HexDigit 0
            , psfNeighborPrefix = unpackNibblePrefix prefixBS
            , psfNeighborIndex = HexDigit (fromIntegral nibble)
            , psfMerkleRoot = MPFHash rootBS
            }
        , bs8
        )

-- | Parse a Leaf step (after tag 123 and list begin)
parseLeafStep :: Parser (MPFProofStep MPFHash)
parseLeafStep bs = do
    (skip, bs1) <- parseUInt bs
    (keyBS, bs2) <- parseCBORBytes bs1
    (valueBS, bs3) <- parseCBORBytes bs2
    ((), bs4) <- parseBreak bs3
    let branchJump = replicate skip (HexDigit 0)
    Just
        ( ProofStepLeaf
            { pslBranchJump = branchJump
            , pslOurPosition = HexDigit 0
            , pslNeighborKeyPath = unpackFullKeyPath keyBS
            , pslNeighborNibble = HexDigit 0
            , pslNeighborSuffix = []
            , pslNeighborValueDigest = MPFHash valueBS
            }
        , bs4
        )

-- | Parse a single proof step
parseStep :: Parser (MPFProofStep MPFHash)
parseStep bs = do
    (tag, bs1) <- parseTag bs
    ((), bs2) <- parseListBegin bs1
    case tag of
        121 -> parseBranchStep bs2
        122 -> parseForkStep bs2
        123 -> parseLeafStep bs2
        _ -> Nothing

-- | Parse the full proof (indefinite list of steps)
parseBytes :: Parser [MPFProofStep MPFHash]
parseBytes bs = do
    ((), bs1) <- parseListBegin bs
    collectSteps [] bs1
  where
    collectSteps acc bs' = case parseByte bs' of
        Just (0xff, rest) -> Just (reverse acc, rest)
        _ -> do
            (step, rest) <- parseStep bs'
            collectSteps (step : acc) rest

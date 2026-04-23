-- |
-- Module      : MPF.Verify
-- Description : Pure verification for Aiken-compatible MPF proofs
-- Copyright   : (c) Paolo Veronelli, 2026
-- License     : Apache-2.0
--
-- Verifies the exact CBOR proof-step encoding used by the Aiken MPF
-- implementation. Unlike the lossy compatibility parser in
-- "MPF.Hashes.Aiken", this module keeps enough information to fold the
-- proof bytes back to a root hash for both inclusion and exclusion.
module MPF.Verify
    ( verifyAikenInclusionProof
    , verifyAikenExclusionProof
    ) where

import Control.Monad (guard, when)
import Data.Bits (testBit)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.List (foldl')
import Data.Word (Word8)
import MPF.Hashes
    ( MPFHash (..)
    , MPFHashing (..)
    , byteStringToHexKey'
    , mkMPFHash
    , mpfHashing
    , nullHash
    , parseMPFHash
    , renderMPFHash
    )
import MPF.Interface (HexDigit (..), HexKey)

data AikenNeighbor = AikenNeighbor
    { anNibble :: HexDigit
    , anPrefix :: HexKey
    , anRoot :: MPFHash
    }

data AikenProofStep
    = AikenBranch
        { apsSkip :: Int
        , apsNeighbors :: [MPFHash]
        }
    | AikenFork
        { apsSkip :: Int
        , apsNeighbor :: AikenNeighbor
        }
    | AikenLeaf
        { apsSkip :: Int
        , apsKey :: HexKey
        , apsValue :: MPFHash
        }

verifyAikenInclusionProof
    :: ByteString -> ByteString -> ByteString -> ByteString -> Bool
verifyAikenInclusionProof rootBs keyBs valueBs proofBs =
    case (parseMPFHash rootBs, parseExactAikenProof proofBs) of
        (Just trustedRoot, Just proofSteps) ->
            case foldAikenProof
                True
                (pathFor keyBs)
                (Just (mkMPFHash valueBs))
                proofSteps of
                Just (Just computedRoot) -> computedRoot == trustedRoot
                _ -> False
        _ -> False

verifyAikenExclusionProof
    :: ByteString -> ByteString -> ByteString -> Bool
verifyAikenExclusionProof rootBs keyBs proofBs =
    case parseExactAikenProof proofBs of
        Just proofSteps ->
            case foldAikenProof False (pathFor keyBs) Nothing proofSteps of
                Just Nothing -> rootBs == renderMPFHash nullHash
                Just (Just computedRoot) ->
                    parseMPFHash rootBs == Just computedRoot
                _ -> False
        Nothing -> False

pathFor :: ByteString -> HexKey
pathFor = byteStringToHexKey' . renderMPFHash . mkMPFHash

foldAikenProof
    :: Bool
    -> HexKey
    -> Maybe MPFHash
    -> [AikenProofStep]
    -> Maybe (Maybe MPFHash)
foldAikenProof including path valueDigest =
    go 0
  where
    go cursor [] =
        if including
            then do
                digest <- valueDigest
                pure
                    $ Just
                    $ leafHash mpfHashing (drop cursor path) digest
            else pure Nothing
    go cursor (proofStep : rest) = do
        let skip = apsSkip proofStep
            nibbleIx = cursor + skip
            isLast = null rest
        prefix <- sliceHex cursor nibbleIx path
        acc <- go (nibbleIx + 1) rest
        case proofStep of
            AikenBranch{apsNeighbors} -> do
                ourNibble <- indexHex nibbleIx path
                let merkle =
                        rebuildMerkleRoot
                            (hexDigitToInt ourNibble)
                            acc
                            apsNeighbors
                pure
                    $ Just
                    $ branchHash mpfHashing prefix merkle
            AikenFork{apsNeighbor = AikenNeighbor{anNibble, anPrefix, anRoot}}
                | not including && isLast ->
                    pure
                        $ Just
                        $ branchHash
                            mpfHashing
                            (prefix <> [anNibble] <> anPrefix)
                            anRoot
                | otherwise -> do
                    ourNibble <- indexHex nibbleIx path
                    guard (ourNibble /= anNibble)
                    let neighborHash =
                            branchHash mpfHashing anPrefix anRoot
                        sparseChildren =
                            [ if HexDigit n == ourNibble
                                then acc
                                else
                                    if HexDigit n == anNibble
                                        then Just neighborHash
                                        else Nothing
                            | n <- [0 .. 15]
                            ]
                    pure
                        $ Just
                        $ branchHash
                            mpfHashing
                            prefix
                            (merkleRoot mpfHashing sparseChildren)
            AikenLeaf{apsKey, apsValue} -> do
                witnessPrefix <- sliceHex 0 cursor apsKey
                guard (witnessPrefix == take cursor path)
                targetNibble <- indexHex nibbleIx path
                neighborNibble <- indexHex nibbleIx apsKey
                guard (neighborNibble /= targetNibble)
                if not including && isLast
                    then
                        pure
                            $ Just
                            $ leafHash
                                mpfHashing
                                (drop cursor apsKey)
                                apsValue
                    else do
                        let neighborSuffix = drop (nibbleIx + 1) apsKey
                            neighborHash =
                                leafHash
                                    mpfHashing
                                    neighborSuffix
                                    apsValue
                            sparseChildren =
                                [ if HexDigit n == targetNibble
                                    then acc
                                    else
                                        if HexDigit n == neighborNibble
                                            then Just neighborHash
                                            else Nothing
                                | n <- [0 .. 15]
                                ]
                        pure
                            $ Just
                            $ branchHash
                                mpfHashing
                                prefix
                                (merkleRoot mpfHashing sparseChildren)

hexDigitToInt :: HexDigit -> Int
hexDigitToInt (HexDigit w) = fromIntegral w

rebuildMerkleRoot
    :: Int -> Maybe MPFHash -> [MPFHash] -> MPFHash
rebuildMerkleRoot position acc =
    foldl' step (maybe nullHash id acc) . zip [0 :: Int ..] . reverse
  where
    step current (depth, siblingHash)
        | testBit position depth =
            mkMPFHash (renderMPFHash siblingHash <> renderMPFHash current)
        | otherwise =
            mkMPFHash (renderMPFHash current <> renderMPFHash siblingHash)

sliceHex :: Int -> Int -> HexKey -> Maybe HexKey
sliceHex start end key
    | start < 0 || end < start = Nothing
    | otherwise =
        let prefix = take (end - start) (drop start key)
        in  if length prefix == end - start then Just prefix else Nothing

indexHex :: Int -> HexKey -> Maybe HexDigit
indexHex ix key
    | ix < 0 = Nothing
    | otherwise = case drop ix key of
        d : _ -> Just d
        [] -> Nothing

parseExactAikenProof :: ByteString -> Maybe [AikenProofStep]
parseExactAikenProof bs = case parseBytes bs of
    Just (steps, rest) | B.null rest -> Just steps
    _ -> Nothing

type Parser a = ByteString -> Maybe (a, ByteString)

parseByte :: Parser Word8
parseByte bs = case B.uncons bs of
    Just (w, rest) -> Just (w, rest)
    Nothing -> Nothing

expectByte :: Word8 -> Parser ()
expectByte expected bs = case parseByte bs of
    Just (w, rest) | w == expected -> Just ((), rest)
    _ -> Nothing

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

parseIndefBytes :: Parser ByteString
parseIndefBytes bs = case expectByte 0x5f bs of
    Just ((), rest) -> collectChunks [] rest
    Nothing -> Nothing
  where
    collectChunks acc bs' = case parseByte bs' of
        Just (0xff, rest) -> Just (B.concat (reverse acc), rest)
        _ -> do
            (chunk, rest) <- parseDefBytes bs'
            collectChunks (chunk : acc) rest

parseCBORBytes :: Parser ByteString
parseCBORBytes bs = case parseDefBytes bs of
    Just out -> Just out
    Nothing -> parseIndefBytes bs

parseTag :: Parser Int
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

parseListBegin :: Parser ()
parseListBegin = expectByte 0x9f

parseBreak :: Parser ()
parseBreak = expectByte 0xff

takeN :: Int -> Parser ByteString
takeN n bs
    | B.length bs >= n = Just (B.take n bs, B.drop n bs)
    | otherwise = Nothing

parseBranchStep :: Parser AikenProofStep
parseBranchStep bs = do
    (skip, bs1) <- parseUInt bs
    (neighborsBS, bs2) <- parseCBORBytes bs1
    ((), bs3) <- parseBreak bs2
    guard (B.length neighborsBS == 128)
    let proofHashes = splitHashes neighborsBS
    pure (AikenBranch skip proofHashes, bs3)

parseForkStep :: Parser AikenProofStep
parseForkStep bs = do
    (skip, bs1) <- parseUInt bs
    (tag, bs2) <- parseTag bs1
    when (tag /= 121) Nothing
    ((), bs3) <- parseListBegin bs2
    (nibble, bs4) <- parseUInt bs3
    (prefixBS, bs5) <- parseCBORBytes bs4
    (rootBS, bs6) <- parseCBORBytes bs5
    ((), bs7) <- parseBreak bs6
    ((), bs8) <- parseBreak bs7
    guard (nibble >= 0 && nibble < 16)
    neighborPrefix <- unpackNibblePrefix prefixBS
    neighborRoot <- MPFHash <$> takeExact 32 rootBS
    pure
        ( AikenFork
            { apsSkip = skip
            , apsNeighbor =
                AikenNeighbor
                    { anNibble = HexDigit (fromIntegral nibble)
                    , anPrefix = neighborPrefix
                    , anRoot = neighborRoot
                    }
            }
        , bs8
        )

parseLeafStep :: Parser AikenProofStep
parseLeafStep bs = do
    (skip, bs1) <- parseUInt bs
    (keyBS, bs2) <- parseCBORBytes bs1
    (valueBS, bs3) <- parseCBORBytes bs2
    ((), bs4) <- parseBreak bs3
    keyBytes <- takeExact 32 keyBS
    let keyPath = byteStringToHexKey' keyBytes
    valueHash <- MPFHash <$> takeExact 32 valueBS
    pure
        ( AikenLeaf
            { apsSkip = skip
            , apsKey = keyPath
            , apsValue = valueHash
            }
        , bs4
        )

parseStep :: Parser AikenProofStep
parseStep bs = do
    (tag, bs1) <- parseTag bs
    ((), bs2) <- parseListBegin bs1
    case tag of
        121 -> parseBranchStep bs2
        122 -> parseForkStep bs2
        123 -> parseLeafStep bs2
        _ -> Nothing

parseBytes :: Parser [AikenProofStep]
parseBytes bs = do
    ((), bs1) <- parseListBegin bs
    collectSteps [] bs1
  where
    collectSteps acc bs' = case parseByte bs' of
        Just (0xff, rest) -> Just (reverse acc, rest)
        _ -> do
            (step, rest) <- parseStep bs'
            collectSteps (step : acc) rest

splitHashes :: ByteString -> [MPFHash]
splitHashes bs =
    [ MPFHash (B.take 32 bs)
    , MPFHash (B.take 32 (B.drop 32 bs))
    , MPFHash (B.take 32 (B.drop 64 bs))
    , MPFHash (B.take 32 (B.drop 96 bs))
    ]

takeExact :: Int -> ByteString -> Maybe ByteString
takeExact n bs
    | B.length bs == n = Just bs
    | otherwise = Nothing

unpackNibblePrefix :: ByteString -> Maybe HexKey
unpackNibblePrefix =
    mapM toNibble . B.unpack
  where
    toNibble w
        | w < 16 = Just (HexDigit w)
        | otherwise = Nothing

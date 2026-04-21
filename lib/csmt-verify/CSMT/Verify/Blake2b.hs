{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : CSMT.Verify.Blake2b
-- Description : Pure-Haskell Blake2b-256 for proof verification
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- A self-contained Blake2b-256 implementation following RFC 7693.
-- Exists so that the @csmt-verify@ sublibrary has no C dependencies
-- and compiles on the GHC WASM backend without the @crypton@ /
-- @memory@ / @ram@ recipe. Only the fixed-size (32-byte, unkeyed)
-- variant is exposed — it is all that CSMT proof verification needs.
--
-- Performance is not a goal. Proofs are short, verification is
-- called a handful of times per request, and correctness is
-- cross-checked against the C implementation by the test suite.
module CSMT.Verify.Blake2b
    ( blake2b256
    ) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Array.ST
    ( STUArray
    , newArray_
    , newListArray
    , readArray
    , writeArray
    )
import Data.Array.Unboxed (UArray, listArray, (!))
import Data.Bits (complement, rotateR, shiftL, shiftR, xor, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Word (Word64, Word8)

-- | Blake2b-256 hash of a byte string. Output is always 32 bytes.
blake2b256 :: ByteString -> ByteString
blake2b256 = blake2b 32

-- | Blake2b with a configurable digest length (1..64 bytes).
blake2b :: Int -> ByteString -> ByteString
blake2b outLen input = runST $ do
    h <- newListArray (0, 7) initialH
    let blocks = allBlocks input
        n = length blocks
    forM_ (zip [1 ..] blocks) $ \(i, (t, blk)) ->
        compress h blk t (i == n)
    extract h
  where
    param0 :: Word64
    param0 = 0x01010000 .|. fromIntegral outLen

    initialH :: [Word64]
    initialH =
        zipWith xor ivList (param0 : replicate 7 0)

    extract :: STUArray s Int Word64 -> ST s ByteString
    extract h = do
        ws <- mapM (readArray h) [0 .. 7]
        pure
            $ B.take outLen
            $ B.concat (map word64LE ws)

-- Returns a list of @(tCounter, 128-byte block)@ pairs. The counter
-- is the cumulative number of input bytes processed through the end
-- of the block (zeros added by padding do not count). Empty input
-- yields a single all-zero block with counter 0.
allBlocks :: ByteString -> [(Word64, ByteString)]
allBlocks bs
    | B.null bs = [(0, B.replicate 128 0)]
    | otherwise = go 0 bs
  where
    go :: Word64 -> ByteString -> [(Word64, ByteString)]
    go !acc xs
        | B.length xs <= 128 =
            let acc' = acc + fromIntegral (B.length xs)
                pad = B.replicate (128 - B.length xs) 0
            in  [(acc', xs <> pad)]
        | otherwise =
            let (hd, tl) = B.splitAt 128 xs
                acc' = acc + 128
            in  (acc', hd) : go acc' tl

-- The Blake2b compression function (RFC 7693 §3.2). Mutates 'h' in
-- place.
compress
    :: STUArray s Int Word64
    -> ByteString
    -- ^ 128-byte block
    -> Word64
    -- ^ counter t (low half; high half is always 0 for our inputs)
    -> Bool
    -- ^ final-block flag
    -> ST s ()
compress h blk t isLast = do
    v <- newArray_ (0, 15)
    forM_ [0 .. 7] $ \i -> readArray h i >>= writeArray v i
    forM_ [0 .. 7] $ \i -> writeArray v (i + 8) (ivList !! i)
    do
        v12 <- readArray v 12
        writeArray v 12 (v12 `xor` t)
    when isLast $ do
        v14 <- readArray v 14
        writeArray v 14 (complement v14)
    let m = parseMsgBlock blk
    forM_ [0 .. 11] $ \r -> do
        let s i = sigma ! (r, i)
        mix v 0 4 8 12 (m !! s 0) (m !! s 1)
        mix v 1 5 9 13 (m !! s 2) (m !! s 3)
        mix v 2 6 10 14 (m !! s 4) (m !! s 5)
        mix v 3 7 11 15 (m !! s 6) (m !! s 7)
        mix v 0 5 10 15 (m !! s 8) (m !! s 9)
        mix v 1 6 11 12 (m !! s 10) (m !! s 11)
        mix v 2 7 8 13 (m !! s 12) (m !! s 13)
        mix v 3 4 9 14 (m !! s 14) (m !! s 15)
    forM_ [0 .. 7] $ \i -> do
        hi <- readArray h i
        vi <- readArray v i
        vi8 <- readArray v (i + 8)
        writeArray h i (hi `xor` vi `xor` vi8)

-- The Blake2b \"G\" mixing function.
mix
    :: STUArray s Int Word64
    -> Int
    -> Int
    -> Int
    -> Int
    -> Word64
    -> Word64
    -> ST s ()
mix v a b c d x y = do
    va <- readArray v a
    vb <- readArray v b
    vc <- readArray v c
    vd <- readArray v d
    let a1 = va + vb + x
        d1 = rotateR (vd `xor` a1) 32
        c1 = vc + d1
        b1 = rotateR (vb `xor` c1) 24
        a2 = a1 + b1 + y
        d2 = rotateR (d1 `xor` a2) 16
        c2 = c1 + d2
        b2 = rotateR (b1 `xor` c2) 63
    writeArray v a a2
    writeArray v b b2
    writeArray v c c2
    writeArray v d d2

-- Blake2b initialization vector (IV): the fractional parts of the
-- square roots of the first 8 primes.
ivList :: [Word64]
ivList =
    [ 0x6a09e667f3bcc908
    , 0xbb67ae8584caa73b
    , 0x3c6ef372fe94f82b
    , 0xa54ff53a5f1d36f1
    , 0x510e527fade682d1
    , 0x9b05688c2b3e6c1f
    , 0x1f83d9abfb41bd6b
    , 0x5be0cd19137e2179
    ]

-- Sigma permutation table for 12 rounds × 16 indices (RFC 7693 §2.7).
sigma :: UArray (Int, Int) Int
sigma =
    listArray
        ((0, 0), (11, 15))
        $ concat
            [ [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
            , [14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3]
            , [11, 8, 12, 0, 5, 2, 15, 13, 10, 14, 3, 6, 7, 1, 9, 4]
            , [7, 9, 3, 1, 13, 12, 11, 14, 2, 6, 5, 10, 4, 0, 15, 8]
            , [9, 0, 5, 7, 2, 4, 10, 15, 14, 1, 11, 12, 6, 8, 3, 13]
            , [2, 12, 6, 10, 0, 11, 8, 3, 4, 13, 7, 5, 15, 14, 1, 9]
            , [12, 5, 1, 15, 14, 13, 4, 10, 0, 7, 6, 3, 9, 2, 8, 11]
            , [13, 11, 7, 14, 12, 1, 3, 9, 5, 0, 15, 4, 8, 6, 2, 10]
            , [6, 15, 14, 9, 11, 3, 0, 8, 12, 2, 13, 7, 1, 4, 10, 5]
            , [10, 2, 8, 4, 7, 6, 1, 5, 15, 11, 9, 14, 3, 12, 13, 0]
            , [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
            , [14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3]
            ]

parseMsgBlock :: ByteString -> [Word64]
parseMsgBlock bs = [readWord64LE (B.drop (i * 8) bs) | i <- [0 .. 15]]

readWord64LE :: ByteString -> Word64
readWord64LE bs =
    foldr
        (\i acc -> acc `shiftL` 8 .|. fromIntegral (B.index bs i))
        0
        [0 .. 7]

word64LE :: Word64 -> ByteString
word64LE w =
    B.pack [fromIntegral (w `shiftR` (i * 8)) :: Word8 | i <- [0 .. 7]]

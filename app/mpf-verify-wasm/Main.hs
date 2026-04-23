-- |
-- Module      : Main
-- Description : WASM verifier for MPF Aiken proofs
-- Copyright   : (c) Paolo Veronelli, 2026
-- License     : Apache-2.0
--
-- Input protocol on stdin:
--
--   * 1 byte   — opcode: 0 = inclusion, 1 = exclusion
--   * 32 bytes — trusted root hash
--   * klen key
--   * vlen value
--   * plen proof
--
-- MPF proof CBOR carries only the proof-step list, so the verifier
-- must also receive the queried key and, for inclusion mode, the value.
module Main (main) where

import Data.ByteString qualified as B
import Data.Serialize
    ( Get
    , getByteString
    , getWord32be
    , getWord8
    , runGet
    )
import Data.Word (Word8)
import MPF.Verify
    ( verifyAikenExclusionProof
    , verifyAikenInclusionProof
    )
import System.Exit (exitFailure, exitSuccess)
import System.IO (stdin)

main :: IO ()
main = do
    input <- B.hGetContents stdin
    let ok = either (const False) verifyPayload (runGet parseInput input)
    if ok then exitSuccess else exitFailure

parseInput
    :: Get (Word8, B.ByteString, B.ByteString, B.ByteString, B.ByteString)
parseInput = do
    opcode <- getWord8
    rootBs <- getByteString 32
    keyBs <- getLenBytes
    valueBs <- getLenBytes
    proofBs <- getLenBytes
    pure (opcode, rootBs, keyBs, valueBs, proofBs)

getLenBytes :: Get B.ByteString
getLenBytes = do
    n <- fromIntegral <$> getWord32be
    getByteString n

verifyPayload
    :: (Word8, B.ByteString, B.ByteString, B.ByteString, B.ByteString)
    -> Bool
verifyPayload (opcode, rootBs, keyBs, valueBs, proofBs) = case opcode of
    0 -> verifyAikenInclusionProof rootBs keyBs valueBs proofBs
    1 -> verifyAikenExclusionProof rootBs keyBs proofBs
    _ -> False

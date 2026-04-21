-- |
-- Module      : Main
-- Description : WASM entry point for csmt-verify
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- Minimal stdio-based verifier for the CSMT proof-carrying API.
-- Intended to be compiled by @wasm32-wasi-cabal@ to a single
-- @csmt-verify.wasm@ module that any WASI-capable runtime (including
-- JS hosts via @wasi-js@) can drive.
--
-- Input protocol on stdin:
--
--   * 1 byte  — opcode: 0 = inclusion proof, 1 = exclusion proof
--   * 32 bytes — trusted root hash
--   * N bytes  — CBOR-encoded proof (as produced by the @csmt@ write
--                side's @Write.renderProof@ / @Write.renderExclusion@)
--
-- Exit code: 0 if the proof verifies against the root, 1 otherwise.
-- Any malformed input (short read, unknown opcode, bad CBOR) is
-- treated as a verification failure and returns exit code 1.
--
-- This deliberately small surface exists so that a native WASM host
-- (or a browser via @wasi-js@) can verify CSMT proofs with no C
-- dependencies and no ceremony around buffer allocation. A richer
-- JSON/FFI surface can be layered on later; for now a byte-level
-- shim is enough to prove that the sublibrary cross-compiles cleanly.
module Main (main) where

import CSMT.Verify
    ( verifyExclusionProof
    , verifyInclusionProof
    )
import Data.ByteString qualified as B
import System.Exit (exitFailure, exitSuccess)
import System.IO (stdin)

main :: IO ()
main = do
    input <- B.hGetContents stdin
    let ok = dispatch input
    if ok then exitSuccess else exitFailure

dispatch :: B.ByteString -> Bool
dispatch bs
    | B.length bs < 33 = False
    | otherwise =
        let opcode = B.head bs
            rootBs = B.take 32 (B.drop 1 bs)
            proofBs = B.drop 33 bs
        in  case opcode of
                0 -> verifyInclusionProof rootBs proofBs
                1 -> verifyExclusionProof rootBs proofBs
                _ -> False

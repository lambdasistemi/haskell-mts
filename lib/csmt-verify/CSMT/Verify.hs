-- |
-- Module      : CSMT.Verify
-- Description : CSMT inclusion/exclusion proof verification
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- Top-level entry point for the @csmt-verify@ sublibrary: a
-- database-free, WASM-friendly subset of the CSMT machinery that is
-- just enough to verify an inclusion or exclusion proof carried on
-- the wire. Callers that already hold raw proof and root bytes can
-- use 'verifyInclusionProof' / 'verifyExclusionProof'; callers that
-- need the structured types can import the submodules directly.
module CSMT.Verify
    ( verifyInclusionProof
    , verifyExclusionProof
    ) where

import Data.ByteString (ByteString)

import CSMT.Verify.CBOR (parseExclusionProof, parseProof)
import CSMT.Verify.Exclusion qualified as Exclusion
import CSMT.Verify.Hash (hashHashing, parseHash)
import CSMT.Verify.Proof qualified as Proof

-- | Verify an inclusion proof from a serialized 'ByteString' against
-- a trusted root hash. Mirrors @CSMT.Hashes.verifyInclusionProof@ on
-- the write side.
verifyInclusionProof :: ByteString -> ByteString -> Bool
verifyInclusionProof trustedRootBs proofBs =
    case (parseHash trustedRootBs, parseProof proofBs) of
        (Just trustedRoot, Just proof) ->
            Proof.verifyInclusionProof hashHashing trustedRoot proof
        _ -> False

-- | Verify an exclusion proof from a serialized 'ByteString' against
-- a trusted root hash.
verifyExclusionProof :: ByteString -> ByteString -> Bool
verifyExclusionProof trustedRootBs proofBs =
    case (parseHash trustedRootBs, parseExclusionProof proofBs) of
        (Just trustedRoot, Just proof) ->
            Exclusion.verifyExclusionProof hashHashing trustedRoot proof
        _ -> False

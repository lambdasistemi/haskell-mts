-- |
-- Module      : CSMT.Verify
-- Description : CSMT inclusion/exclusion/completeness proof verification
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- Top-level entry point for the @csmt-verify@ sublibrary: a
-- database-free, WASM-friendly subset of the CSMT machinery that is
-- just enough to verify an inclusion, exclusion, or completeness
-- proof carried on the wire. Wires the pure-Haskell Blake2b-256
-- from 'CSMT.Verify.Blake2b' into the backend-agnostic 'Hashing'
-- record from @csmt-core@, and feeds raw bytes through the shared
-- verification logic.
module CSMT.Verify
    ( verifyInclusionProof
    , verifyExclusionProof
    , verifyCompletenessProof

      -- * Re-exports for completeness-proof callers
    , CompletenessProof (..)
    , parseCompletenessProof
    ) where

import Data.ByteString (ByteString)

import CSMT.Core.CBOR
    ( parseCompletenessProof
    , parseExclusionProof
    , parseProof
    )
import CSMT.Core.Completeness
    ( CompletenessProof (..)
    , foldCompletenessProof
    )
import CSMT.Core.Exclusion qualified as Exclusion
import CSMT.Core.Hash (Hash (..), hashingWith, parseHash)
import CSMT.Core.Proof qualified as Proof
import CSMT.Core.Types (Hashing, Indirect, Key)
import CSMT.Verify.Blake2b (blake2b256)

-- | 'Hashing' record backed by the pure-Haskell Blake2b-256 in
-- 'CSMT.Verify.Blake2b'. Mirrors @CSMT.Hashes.hashHashing@ on the
-- write side byte-for-byte — both route through the same
-- 'CSMT.Core.Hash.hashingWith' combinator on top of the same
-- Blake2b-256 output.
hashHashing :: Hashing Hash
hashHashing = hashingWith (Hash . blake2b256)

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

-- | Verify a completeness proof from a serialized 'ByteString'
-- against a trusted root hash. The verifier supplies the prefix
-- and the leaves it received; the proof bytes carry the merge
-- operations and inclusion steps. Returns 'False' (not an
-- exception) on malformed bytes or any verification mismatch.
verifyCompletenessProof
    :: ByteString
    -- ^ Trusted root bytes
    -> Key
    -- ^ Prefix this proof covers
    -> [Indirect Hash]
    -- ^ Leaves under the prefix
    -> ByteString
    -- ^ CompletenessProof CBOR bytes
    -> Bool
verifyCompletenessProof trustedRootBs prefixKey leaves proofBs =
    case (parseHash trustedRootBs, parseCompletenessProof proofBs) of
        (Just trustedRoot, Just proof) ->
            case foldCompletenessProof
                hashHashing
                prefixKey
                leaves
                proof of
                Just computed -> computed == trustedRoot
                Nothing -> False
        _ -> False

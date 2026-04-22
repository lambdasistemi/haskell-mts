-- |
-- Module      : CSMT.Hashes.CBOR
-- Description : Re-export of shared CBOR proof codecs
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- Thin facade over 'CSMT.Core.CBOR' so legacy importers of
-- @CSMT.Hashes.CBOR@ continue to resolve. The wire format is
-- defined once in @csmt-core@.
module CSMT.Hashes.CBOR
    ( renderProof
    , parseProof
    , renderExclusionProof
    , parseExclusionProof
    )
where

import CSMT.Core.CBOR
    ( parseExclusionProof
    , parseProof
    , renderExclusionProof
    , renderProof
    )

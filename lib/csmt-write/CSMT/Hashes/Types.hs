-- |
-- Module      : CSMT.Hashes.Types
-- Description : Re-export of the shared 'Hash' type
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- Thin facade over 'CSMT.Core.Hash.Hash' so legacy importers of
-- @CSMT.Hashes.Types@ continue to resolve. The write side and the
-- WASM-safe verifier now share the same underlying 32-byte hash
-- wrapper from @csmt-core@.
module CSMT.Hashes.Types
    ( Hash (..)
    , renderHash
    )
where

import CSMT.Core.Hash (Hash (..), renderHash)

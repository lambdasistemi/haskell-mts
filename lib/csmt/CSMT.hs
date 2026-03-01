{-# LANGUAGE StrictData #-}

-- |
-- Module      : CSMT
-- Description : Compact Sparse Merkle Tree implementation
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- A Haskell library implementing a Compact Sparse Merkle Tree (CSMT) data
-- structure with support for persistent storage backends. It provides efficient
-- insertion, deletion, and proof generation functionalities.
--
-- This module re-exports the main API:
--
-- * "CSMT.Interface" - Core tree operations (insert, delete, query)
-- * "CSMT.Insertion" - Low-level insertion algorithm
-- * "CSMT.Proof.Insertion" - Merkle proof generation and verification
-- * "CSMT.Backend.Standalone" - Self-contained backend with preimage storage
module CSMT
    ( module CSMT.Interface
    , module CSMT.Insertion
    , module CSMT.Proof.Insertion
    , module CSMT.Backend.Standalone
    )
where

import CSMT.Backend.Standalone
import CSMT.Insertion
import CSMT.Interface
import CSMT.Proof.Insertion

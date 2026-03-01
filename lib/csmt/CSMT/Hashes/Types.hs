-- |
-- Module      : CSMT.Hashes.Types
-- Description : Hash type for Blake2b-256 based CSMT
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- Core 'Hash' type definition for Blake2b-256 based CSMTs.
module CSMT.Hashes.Types
    ( Hash (..)
    , renderHash
    )
where

import Data.ByteArray (ByteArray, ByteArrayAccess)
import Data.ByteArray.Encoding (Base (Base64), convertToBase)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC

-- | A 32-byte Blake2b-256 hash value.
newtype Hash = Hash ByteString
    deriving
        (Eq, Ord, Semigroup, Monoid, ByteArrayAccess, ByteArray)

instance Show Hash where
    show (Hash h) = BC.unpack $ "Hash " <> convertToBase Base64 h

-- | Extract the raw ByteString from a Hash.
renderHash :: Hash -> ByteString
renderHash (Hash h) = h

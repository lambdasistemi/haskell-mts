-- |
-- Module      : MPF.Hashes.Types
-- Description : Hash type for Blake2b-256 based MPF
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- Core 'MPFHash' type definition for Blake2b-256 based MPF.
module MPF.Hashes.Types
    ( MPFHash (..)
    , renderMPFHash
    )
where

import Data.ByteString (ByteString)

-- | MPF Hash value (32 bytes Blake2b-256)
newtype MPFHash = MPFHash ByteString
    deriving (Eq, Ord, Semigroup, Monoid)

instance Show MPFHash where
    show (MPFHash h) = "MPFHash " ++ show h

-- | Extract the raw bytes of a hash
renderMPFHash :: MPFHash -> ByteString
renderMPFHash (MPFHash h) = h

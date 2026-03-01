{-# LANGUAGE StrictData #-}

-- |
-- Module      : CSMT.Backend.Standalone
-- Description : Standalone backend types for CSMT
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- Types and functions for a standalone backend, where we do not need to access
-- other key-value stores. This is the backend used in the standalone application.
--
-- For more complex library clients that need to integrate with existing
-- storage systems, these types may not suffice - consider building custom
-- column definitions.
module CSMT.Backend.Standalone
    ( StandaloneOp
    , StandaloneCF (..)
    , Standalone (..)
    , StandaloneCodecs (..)
    , mkStandaloneOp
    )
where

import CSMT.Interface (Indirect (..), Key)
import Control.Lens (Prism', type (:~:) (..))
import Data.ByteString (ByteString)
import Database.KV.Transaction
    ( GCompare (..)
    , GEq (..)
    , GOrdering (..)
    , KV
    )

-- | Column family identifiers for the standalone backend.
data StandaloneCF = StandaloneKV | StandaloneCSMT

-- | A database operation: column family, key, and optional value (Nothing = delete).
type StandaloneOp = (StandaloneCF, ByteString, Maybe ByteString)

-- | Construct a standalone operation tuple.
mkStandaloneOp
    :: StandaloneCF -> ByteString -> Maybe ByteString -> StandaloneOp
mkStandaloneOp = (,,)

-- |
-- GADT defining the two column types in the standalone backend:
--
-- * 'StandaloneKVCol' - The key-value storage column
-- * 'StandaloneCSMTCol' - The CSMT node storage column
data Standalone k v a x where
    -- | Column for user key-value pairs
    StandaloneKVCol :: Standalone k v a (KV k v)
    -- | Column for CSMT tree nodes
    StandaloneCSMTCol :: Standalone k v a (KV Key (Indirect a))

instance GEq (Standalone k v a) where
    geq StandaloneKVCol StandaloneKVCol = Just Refl
    geq StandaloneCSMTCol StandaloneCSMTCol = Just Refl
    geq _ _ = Nothing

instance GCompare (Standalone k v a) where
    gcompare StandaloneKVCol StandaloneKVCol = GEQ
    gcompare StandaloneKVCol StandaloneCSMTCol = GLT
    gcompare StandaloneCSMTCol StandaloneKVCol = GGT
    gcompare StandaloneCSMTCol StandaloneCSMTCol = GEQ

-- |
-- Codecs for serializing keys, values, and hash nodes to ByteStrings.
--
-- These prisms handle the conversion between typed values and their
-- binary representation for storage.
data StandaloneCodecs k v a = StandaloneCodecs
    { keyCodec :: Prism' ByteString k
    -- ^ Prism for encoding/decoding keys
    , valueCodec :: Prism' ByteString v
    -- ^ Prism for encoding/decoding values
    , nodeCodec :: Prism' ByteString a
    -- ^ Prism for encoding/decoding hash values
    }

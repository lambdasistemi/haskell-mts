-- |
-- Module      : Data.Serialize.Extra
-- Description : Convenience functions for cereal serialization
-- Copyright   : (c) Paolo Veronelli, 2026
-- License     : Apache-2.0
--
-- Helper functions for working with cereal's 'PutM' and 'Get' monads.
module Data.Serialize.Extra
    ( evalPutM
    , unsafeEvalGet
    , evalGetM
    )
where

import Data.ByteString (ByteString)
import Data.Serialize
    ( Get
    , PutM
    , runGet
    , runPutM
    )

-- | Run a 'PutM' action and return only the serialized bytes, discarding any result.
evalPutM :: PutM a -> ByteString
evalPutM putM = snd $ runPutM putM

-- | Run a 'Get' parser, throwing an error on parse failure.
-- Use only when parse failure indicates a programming error.
unsafeEvalGet :: Get a -> ByteString -> a
unsafeEvalGet get bs = case runGet get bs of
    Right a -> a
    Left err -> error $ "unsafeEvalGet: parse error: " ++ err

-- | Run a 'Get' parser, returning 'Nothing' on parse failure.
evalGetM :: Get a -> ByteString -> Maybe a
evalGetM get bs = case runGet get bs of
    Right a -> Just a
    Left _err -> Nothing

-- |
-- Module      : CSMT.Interface
-- Description : DB-aware wiring on top of the shared CSMT type algebra
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- Re-exports the backend-free CSMT type algebra from
-- 'CSMT.Core.Types' and layers the database-aware pieces on top:
--
--   * 'FromKV' — projection from external key-value pairs into
--     tree keys and hash values.
--   * 'root'   — transactional query for the current root hash.
--   * 'csmtCodecs' / 'keyPrism' — lens-based codecs used by the
--     rocksdb-kv-transactions layer.
--
-- All pure types, serializers, and the 'Hashing' record live in
-- @csmt-core@ so the verifier side can re-use them without
-- pulling database dependencies.
module CSMT.Interface
    ( -- * Keys
      Direction (..)
    , Key
    , keyPrism
    , compareKeys
    , oppositeDirection

      -- * Interface Types
    , Indirect (..)
    , Hashing (..)
    , FromKV (..)

      -- * Serialization Helpers
    , fromBool
    , toBool
    , root
    , putKey
    , getKey
    , putIndirect
    , getIndirect
    , putDirection
    , getDirection
    , getSizedByteString
    , putSizedByteString
    , addWithDirection
    , prefix
    , csmtCodecs
    )
where

import Control.Lens (Iso', Prism', preview, prism', review, (<&>))
import Data.ByteString (ByteString)
import Data.Serialize.Extra (evalGetM, evalPutM, unsafeEvalGet)
import Database.KV.Transaction
    ( Codecs (..)
    , GCompare
    , KV
    , Selector
    , Transaction
    , query
    )

import CSMT.Core.Types
    ( Direction (..)
    , Hashing (..)
    , Indirect (..)
    , Key
    , addWithDirection
    , compareKeys
    , fromBool
    , getDirection
    , getIndirect
    , getKey
    , getSizedByteString
    , oppositeDirection
    , prefix
    , putDirection
    , putIndirect
    , putKey
    , putSizedByteString
    , toBool
    )

-- | Conversion functions for mapping external key-value types to
-- internal tree keys and hash values.
data FromKV k v a
    = FromKV
    { isoK :: Iso' k Key
    -- ^ Bidirectional conversion between external keys and tree paths
    , fromV :: v -> a
    -- ^ Convert an external value to a hash
    , treePrefix :: v -> Key
    -- ^ Prefix prepended to tree key (for secondary indexing)
    }

-- | Query the root hash of the CSMT. Returns 'Nothing' if the tree is empty.
root
    :: (Monad m, GCompare d)
    => Hashing a
    -> Selector d Key (Indirect a)
    -> Key
    -- ^ Prefix (use @[]@ for root)
    -> Transaction m cf d ops (Maybe a)
root hsh sel pfx = do
    mi <- query sel pfx
    pure $ case mi of
        Nothing -> Nothing
        Just i -> Just $ rootHash hsh i

indirectPrism :: Prism' ByteString a -> Prism' ByteString (Indirect a)
indirectPrism prismA =
    prism'
        (evalPutM . putIndirect . fmap (review prismA))
        ( unsafeEvalGet $ do
            Indirect k x <- getIndirect
            pure $ preview prismA x <&> \a ->
                Indirect{jump = k, value = a}
        )

-- | Prism for encoding/decoding keys to/from ByteStrings.
keyPrism :: Prism' ByteString Key
keyPrism = prism' (evalPutM . putKey) (evalGetM getKey)

-- | Build codecs for CSMT key-value storage given a hash prism.
csmtCodecs :: Prism' ByteString a -> Codecs (KV Key (Indirect a))
csmtCodecs prismA =
    Codecs
        { keyCodec = keyPrism
        , valueCodec = indirectPrism prismA
        }

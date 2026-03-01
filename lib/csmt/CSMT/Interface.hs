{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : CSMT.Interface
-- Description : Core types and serialization for Compact Sparse Merkle Trees
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- This module defines the fundamental types used throughout the CSMT library:
--
-- * 'Key' - A path through the tree represented as a list of 'Direction's
-- * 'Indirect' - A reference to a value with an optional jump path
-- * 'Hashing' - Hash combination functions for building the Merkle structure
--
-- It also provides serialization helpers for encoding keys and indirect
-- references to ByteStrings for storage.
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
import Data.Bits (Bits (..))
import Data.ByteArray (convert)
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.List (unfoldr)
import Data.Serialize
    ( Get
    , PutM
    , getByteString
    , getWord16be
    , getWord8
    , putByteString
    , putWord16be
    , putWord8
    )
import Data.Serialize.Extra (evalGetM, evalPutM, unsafeEvalGet)
import Database.KV.Transaction
    ( Codecs (..)
    , GCompare
    , KV
    , Selector
    , Transaction
    , query
    )

-- |
-- A direction in the binary tree - either Left or Right.
-- Keys are represented as paths through the tree using these directions.
data Direction = L | R deriving (Show, Eq, Ord)

-- | Convert a 'Bool' to a 'Direction'. @True@ maps to 'R', @False@ to 'L'.
fromBool :: Bool -> Direction
fromBool True = R
fromBool False = L

-- | Convert a 'Direction' to its 'Bool' representation. 'L' maps to @False@, 'R' to @True@.
toBool :: Direction -> Bool
toBool L = False
toBool R = True

-- | Get the oppositeDirection direction
oppositeDirection :: Direction -> Direction
oppositeDirection L = R
oppositeDirection R = L

-- |
-- A key is a path through the binary tree, represented as a list of directions.
-- Each bit of the original key maps to a direction: 0 = L, 1 = R.
type Key = [Direction]

-- |
-- An indirect reference to a value stored at a given position relative to a node.
--
-- * If 'jump' is empty, the value is stored at the current node
-- * If 'jump' is non-empty, the value is stored at a descendant node
--   reachable by following the jump path
--
-- This allows for path compression in sparse trees - instead of storing
-- empty nodes, we store a jump path directly to the value.
data Indirect a = Indirect
    { jump :: Key
    , value :: a
    }
    deriving (Show, Eq, Functor, Ord)

-- | Prepend a key prefix to an indirect reference's jump path.
prefix :: Key -> Indirect a -> Indirect a
prefix q Indirect{jump, value} = Indirect{jump = q ++ jump, value}

-- |
-- Conversion functions for mapping external key-value types to internal
-- tree keys and hash values.
data FromKV k v a
    = FromKV
    { isoK :: Iso' k Key
    -- ^ Bidirectional conversion between external keys and tree paths
    , fromV :: v -> a
    -- ^ Convert an external value to a hash
    , treePrefix :: v -> Key
    -- ^ Prefix prepended to tree key (for secondary indexing)
    }

-- | Compare two keys and return their common prefix and the remaining suffixes
-- of each key after the common prefix.
compareKeys :: Key -> Key -> (Key, Key, Key)
compareKeys [] ys = ([], [], ys)
compareKeys xs [] = ([], xs, [])
compareKeys (x : xs) (y : ys)
    | x == y =
        let (j, o, r) = compareKeys xs ys
        in  (x : j, o, r)
    | otherwise = ([], x : xs, y : ys)

-- | Query the root hash of the CSMT. Returns 'Nothing' if the tree is empty.
root
    :: (Monad m, GCompare d)
    => Hashing a
    -> Selector d Key (Indirect a)
    -> Transaction m cf d ops (Maybe a)
root hsh sel = do
    mi <- query sel []
    pure $ case mi of
        Nothing -> Nothing
        Just i -> Just $ rootHash hsh i

bigendian :: [Int]
bigendian = [7, 6 .. 0]

-- | Serialize a 'Direction' to a single byte (0 for 'L', 1 for 'R').
putDirection :: Direction -> PutM ()
putDirection d = do
    putWord8 $ if toBool d then 1 else 0

-- | Deserialize a 'Direction' from a single byte.
getDirection :: Get Direction
getDirection = do
    b <- getWord8
    case b of
        0 -> return L
        1 -> return R
        _ -> fail "Invalid direction byte"

-- | Serialize a 'Key' to bytes: 2-byte length followed by bit-packed directions.
putKey :: Key -> PutM ()
putKey k = do
    let bytes = BA.pack $ unfoldr unconsDirection k
    putWord16be $ fromIntegral $ length k
    putByteString bytes
  where
    unconsDirection :: (Num a, Bits a) => Key -> Maybe (a, Key)
    unconsDirection [] = Nothing
    unconsDirection ds =
        let (byteBits, rest) = splitAt 8 ds
            byte = foldl setBitFromDir 0 (zip bigendian byteBits)
        in  Just (byte, rest)

    setBitFromDir :: Bits b => b -> (Int, Direction) -> b
    setBitFromDir b (i, dir)
        | toBool dir = setBit b i
        | otherwise = b

-- | Deserialize a 'Key' from bytes.
getKey :: Get Key
getKey = do
    len <- getWord16be
    let (l, r) = len `divMod` 8
        lr = if r == 0 then l else l + 1
    ba <- getByteString (fromIntegral lr)
    return
        $ take (fromIntegral len)
        $ concatMap byteToDirections (BA.unpack ba)
  where
    byteToDirections :: Bits b => b -> Key
    byteToDirections byte = [if testBit byte i then R else L | i <- bigendian]

-- | Serialize a byte array with a 2-byte length prefix.
putSizedByteString :: BA.ByteArrayAccess a => a -> PutM ()
putSizedByteString bs = do
    let len = fromIntegral $ BA.length bs
    putWord16be len
    putByteString $ convert bs

-- | Deserialize a length-prefixed byte array.
getSizedByteString :: BA.ByteArray a => Get a
getSizedByteString = do
    len <- getWord16be
    bs <- getByteString (fromIntegral len)
    return $ convert bs

-- | Serialize an Indirect to a ByteString
putIndirect
    :: BA.ByteArrayAccess a => Indirect a -> PutM ()
putIndirect Indirect{jump, value} = do
    putKey jump
    putSizedByteString value

-- | Deserialize a ByteString back to an Indirect
getIndirect :: BA.ByteArray a => Get (Indirect a)
getIndirect = Indirect <$> getKey <*> getSizedByteString

-- |
-- Hash combination functions for building the Merkle tree structure.
--
-- These functions define how hashes are computed at each node:
--
-- * 'rootHash' - Compute the hash of a leaf/root node from its indirect value
-- * 'combineHash' - Combine two child hashes into a parent hash
data Hashing a = Hashing
    { rootHash :: Indirect a -> a
    -- ^ Hash a single indirect value (for leaf nodes)
    , combineHash :: Indirect a -> Indirect a -> a
    -- ^ Combine left and right child hashes into parent hash
    }

-- | Combine two indirect values with the given direction determining order.
addWithDirection
    :: Hashing a -> Direction -> Indirect a -> Indirect a -> a
addWithDirection Hashing{combineHash} L left right = combineHash left right
addWithDirection Hashing{combineHash} R left right = combineHash right left

indirectPrism :: Prism' ByteString a -> Prism' ByteString (Indirect a)
indirectPrism prismA =
    prism'
        (evalPutM . putIndirect . fmap (review prismA))
        ( unsafeEvalGet $ do
            -- TODO: unsafe ?
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

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : CSMT.Verify.Core
-- Description : Pure CSMT types and serialization for proof verification
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- Pure subset of the CSMT type algebra that a verifier needs: the
-- path 'Direction', the 'Key' / 'Indirect' carriers, the 'Hashing'
-- record that combines sibling hashes, and cereal-based byte
-- serializers for them. No database effects, no C FFI. This is the
-- dependency-light core of the @csmt-verify@ sublibrary — only
-- @base@, @bytestring@, and @cereal@ are needed to build it.
module CSMT.Verify.Core
    ( -- * Keys
      Direction (..)
    , Key
    , oppositeDirection
    , compareKeys
    , fromBool
    , toBool

      -- * Indirect values
    , Indirect (..)
    , prefix

      -- * Hashing algebra
    , Hashing (..)
    , addWithDirection

      -- * Byte serializers
    , putKey
    , getKey
    , putDirection
    , getDirection
    , putIndirect
    , getIndirect
    , putSizedByteString
    , getSizedByteString
    ) where

import Data.Bits (Bits (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
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
import Data.Word (Word8)

-- | A direction in the binary tree — either Left or Right.
data Direction = L | R deriving (Show, Eq, Ord)

-- | Convert a 'Bool' to a 'Direction'. @True@ maps to 'R', @False@
-- to 'L'.
fromBool :: Bool -> Direction
fromBool True = R
fromBool False = L

-- | Convert a 'Direction' to its 'Bool' representation.
toBool :: Direction -> Bool
toBool L = False
toBool R = True

-- | Flip a 'Direction'.
oppositeDirection :: Direction -> Direction
oppositeDirection L = R
oppositeDirection R = L

-- | A key is a path through the binary tree, represented as a list
-- of directions. Each bit of the original key maps to a direction:
-- 0 = L, 1 = R.
type Key = [Direction]

-- | An indirect reference to a value stored at a given position
-- relative to a node. If 'jump' is empty, the value is at the
-- current node; otherwise the value is at a descendant reachable
-- by following the jump.
data Indirect a = Indirect
    { jump :: Key
    , value :: a
    }
    deriving (Show, Eq, Functor, Ord)

-- | Prepend a key prefix to an indirect reference's jump path.
prefix :: Key -> Indirect a -> Indirect a
prefix q Indirect{jump, value} = Indirect{jump = q ++ jump, value}

-- | Compare two keys and return their common prefix and the
-- remaining suffixes of each key after the common prefix.
compareKeys :: Key -> Key -> (Key, Key, Key)
compareKeys [] ys = ([], [], ys)
compareKeys xs [] = ([], xs, [])
compareKeys (x : xs) (y : ys)
    | x == y =
        let (j, o, r) = compareKeys xs ys
        in  (x : j, o, r)
    | otherwise = ([], x : xs, y : ys)

-- | Hash combination functions for building the Merkle tree
-- structure. 'rootHash' hashes a single indirect value (leaf node).
-- 'combineHash' combines left and right child hashes into parent.
data Hashing a = Hashing
    { rootHash :: Indirect a -> a
    , combineHash :: Indirect a -> Indirect a -> a
    }

-- | Combine two indirect values with the given direction
-- determining order (so the caller's 'Direction' on the path
-- decides which sibling is on the left and which on the right).
addWithDirection
    :: Hashing a -> Direction -> Indirect a -> Indirect a -> a
addWithDirection Hashing{combineHash} L left right = combineHash left right
addWithDirection Hashing{combineHash} R left right = combineHash right left

bigendian :: [Int]
bigendian = [7, 6 .. 0]

-- | Serialize a 'Direction' to a single byte (0 for 'L', 1 for 'R').
putDirection :: Direction -> PutM ()
putDirection d = putWord8 $ if toBool d then 1 else 0

-- | Deserialize a 'Direction' from a single byte.
getDirection :: Get Direction
getDirection = do
    b <- getWord8
    case b of
        0 -> return L
        1 -> return R
        _ -> fail "Invalid direction byte"

-- | Serialize a 'Key' to bytes: 2-byte length followed by
-- bit-packed directions.
putKey :: Key -> PutM ()
putKey k = do
    let bytes = B.pack $ unfoldr unconsDirection k
    putWord16be $ fromIntegral $ length k
    putByteString bytes
  where
    unconsDirection :: Key -> Maybe (Word8, Key)
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
    bs <- getByteString (fromIntegral lr)
    return
        $ take (fromIntegral len)
        $ concatMap byteToDirections (B.unpack bs)
  where
    byteToDirections :: Word8 -> Key
    byteToDirections byte =
        [if testBit byte i then R else L | i <- bigendian]

-- | Serialize a 'ByteString' with a 2-byte length prefix.
putSizedByteString :: ByteString -> PutM ()
putSizedByteString bs = do
    let len = fromIntegral $ B.length bs
    putWord16be len
    putByteString bs

-- | Deserialize a length-prefixed 'ByteString'.
getSizedByteString :: Get ByteString
getSizedByteString = do
    len <- getWord16be
    getByteString (fromIntegral len)

-- | Serialize an 'Indirect' 'ByteString' to bytes.
putIndirect :: Indirect ByteString -> PutM ()
putIndirect Indirect{jump, value} = do
    putKey jump
    putSizedByteString value

-- | Deserialize an 'Indirect' 'ByteString' from bytes.
getIndirect :: Get (Indirect ByteString)
getIndirect = Indirect <$> getKey <*> getSizedByteString

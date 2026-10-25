-- |
-- Module      : Trees
-- Description : Seeded key-value sets for the Aiken test vectors
-- Copyright   : (c) Paolo Veronelli, 2026
-- License     : Apache-2.0
--
-- The keys and values behind the random-tree and measurement
-- vectors. Every byte comes from a SplitMix64 generator written
-- here, so the vectors depend on the seeds and on nothing else:
-- no wall clock, and no package whose random stream could change
-- with a version bump.
module Trees
    ( Shape (..)
    , TreeSpec (..)
    , Tree (..)
    , randomTreeSpecs
    , growTree
    , shapeName
    , measurementSizes
    , Measurement (..)
    , measurement
    )
where

import Control.Monad (replicateM)
import Control.Monad.Trans.State.Strict
    ( State
    , evalState
    , state
    )
import Data.Bits (bit, complement, shiftR, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64, Word8)

-- -----------------------------------------------------------
-- SplitMix64
-- -----------------------------------------------------------

type Rand = State Word64

-- | Next 64 bits of the SplitMix64 stream.
word :: Rand Word64
word = state $ \s ->
    let s' = s + 0x9e3779b97f4a7c15
        z1 = (s' `xor` (s' `shiftR` 30)) * 0xbf58476d1ce4e5b9
        z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
    in  (z2 `xor` (z2 `shiftR` 31), s')

byte :: Rand Word8
byte = fromIntegral . (`shiftR` 56) <$> word

bytesOf :: Int -> Rand ByteString
bytesOf n = B.pack <$> replicateM n byte

-- | A number in @[0, n)@.
below :: Int -> Rand Int
below n = (\w -> fromIntegral (w `mod` fromIntegral n)) <$> word

-- | A value of one to 32 bytes.
valueOf :: Rand ByteString
valueOf = below 32 >>= bytesOf . (+ 1)

-- | A key from the source that is not in the given set. The caller
-- sizes the source's key space so that one exists.
freshKey :: Set ByteString -> Rand ByteString -> Rand ByteString
freshKey seen source = do
    k <- source
    if k `Set.member` seen then freshKey seen source else pure k

-- | @n@ keys from the source, none in the given set and no two
-- equal.
distinctKeys
    :: Int -> Set ByteString -> Rand ByteString -> Rand [ByteString]
distinctKeys 0 _ _ = pure []
distinctKeys n seen source = do
    k <- freshKey seen source
    (k :) <$> distinctKeys (n - 1) (Set.insert k seen) source

withValues :: [ByteString] -> Rand [(ByteString, ByteString)]
withValues = traverse $ \k -> (,) k <$> valueOf

-- -----------------------------------------------------------
-- Random trees
-- -----------------------------------------------------------

-- | How the keys of one tree relate to each other.
data Shape
    = -- | Independent random bytes.
      Uniform
    | -- | All keys start with the same bytes, three quarters of
      -- the key.
      SharedPrefix
    | -- | All keys are equal except in the last bits of the last
      -- byte.
      LastBits
    deriving stock (Show, Eq)

-- | Name of a shape as it appears in test names.
shapeName :: Shape -> String
shapeName = \case
    Uniform -> "uniform"
    SharedPrefix -> "prefix"
    LastBits -> "lastbits"

data TreeSpec = TreeSpec
    { specShape :: Shape
    , specKeyLength :: Int
    , specSize :: Int
    , specSeed :: Word64
    }
    deriving stock (Show, Eq)

-- | Members in the order they are inserted, and keys of the same
-- length that are not members.
data Tree = Tree
    { treeMembers :: [(ByteString, ByteString)]
    , treeAbsent :: [(ByteString, ByteString)]
    }

-- | Absent keys sampled per tree.
absentSamples :: Int
absentSamples = 3

-- | The trees of the random-tree section: three shapes over the
-- key lengths and sizes that apply to them, each with its own
-- seed.
randomTreeSpecs :: [TreeSpec]
randomTreeSpecs =
    zipWith
        (\i (shape, len, size) -> TreeSpec shape len size (seed i))
        [1 ..]
        [ (shape, len, size)
        | (shape, lens, sizes) <- plan
        , len <- lens
        , size <- sizes
        ]
  where
    seed i = 20261025 + 1009 * i
    plan =
        [ (Uniform, [1, 2, 4, 32], [1, 2, 3, 7, 16, 64])
        , (SharedPrefix, [2, 4, 32], [2, 3, 12, 33])
        , (LastBits, [1, 2, 4, 32], [2, 3, 12, 33])
        ]

-- | Draw the members and the absent keys of a tree.
growTree :: TreeSpec -> Tree
growTree TreeSpec{specShape, specKeyLength, specSize, specSeed} =
    flip evalState specSeed $ do
        source <- keySource specShape specKeyLength specSize
        members <- distinctKeys specSize Set.empty source
        absent <-
            distinctKeys
                absentSamples
                (Set.fromList members)
                source
        Tree <$> withValues members <*> withValues absent

-- | Draws keys of one shape. What the keys of a tree share is
-- fixed before the first key is drawn.
keySource :: Shape -> Int -> Int -> Rand (Rand ByteString)
keySource Uniform len _ = pure $ bytesOf len
keySource SharedPrefix len _ = do
    let shared = len - max 1 (len `div` 4)
    prefix <- bytesOf shared
    pure $ (prefix <>) <$> bytesOf (len - shared)
keySource LastBits len size = do
    stem <- bytesOf (len - 1)
    fixedBits <- byte
    let bits = divergingBits size
        mask = bit bits - 1 :: Word8
    pure $ do
        free <- byte
        pure
            $ stem
                <> B.singleton
                    ( (fixedBits .&. complement mask)
                        .|. (free .&. mask)
                    )

-- | Low bits of the last byte that vary, enough for the members
-- and the absent keys and leaving at least as many free slots as
-- members.
divergingBits :: Int -> Int
divergingBits size =
    until
        (\b -> bit b >= max (2 * size) (size + absentSamples))
        (+ 1)
        2

-- -----------------------------------------------------------
-- Measurement trees
-- -----------------------------------------------------------

-- | Number of keys of the measurement trees.
measurementSizes :: [Int]
measurementSizes = [1, 10, 1000, 100000]

-- | A tree of 32-byte keys and values, one member to prove
-- present and one absent key to prove absent and insert.
data Measurement = Measurement
    { measureMembers :: [(ByteString, ByteString)]
    , measureSample :: (ByteString, ByteString)
    , measureAbsent :: (ByteString, ByteString)
    }

-- | The measurement tree with @n@ keys, @n >= 1@.
measurement :: Int -> Measurement
measurement n = flip evalState (0xa17e5 + fromIntegral n) $ do
    keys <- distinctKeys n Set.empty (bytesOf 32)
    absent <- freshKey (Set.fromList keys) (bytesOf 32)
    i <- below n
    members <- traverse (\k -> (,) k <$> bytesOf 32) keys
    value <- bytesOf 32
    pure
        Measurement
            { measureMembers = members
            , measureSample = members !! i
            , measureAbsent = (absent, value)
            }

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}

module MPF.Interface
    ( -- * Hex Digits
      HexDigit (..)
    , mkHexDigit
    , allHexDigits

      -- * Hex Keys
    , HexKey
    , hexKeyPrism
    , compareHexKeys
    , byteStringToHexKey
    , hexKeyToByteString

      -- * Interface Types
    , HexIndirect (..)
    , mkLeafIndirect
    , mkBranchIndirect
    , prefixHex
    , FromHexKV (..)

      -- * Serialization Helpers
    , putHexDigit
    , getHexDigit
    , putHexKey
    , getHexKey
    , putHexIndirect
    , getHexIndirect
    , putSizedByteString
    , getSizedByteString
    , mpfCodecs
    )
where

import Control.Lens (Prism', preview, prism', review, (<&>))
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteArray (convert)
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
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
import Data.Word (Word8)
import Database.KV.Transaction
    ( Codecs (..)
    , KV
    )

-- | A hex digit (nibble), values 0-15
newtype HexDigit = HexDigit {unHexDigit :: Word8}
    deriving (Show, Eq, Ord)

-- | Smart constructor for HexDigit
mkHexDigit :: Word8 -> Maybe HexDigit
mkHexDigit n
    | n < 16 = Just (HexDigit n)
    | otherwise = Nothing

-- | All 16 hex digits
allHexDigits :: [HexDigit]
allHexDigits = [HexDigit n | n <- [0 .. 15]]

-- | A hex-based key (list of nibbles)
type HexKey = [HexDigit]

-- | An indirect reference to a value stored at a given HexKey from a node
-- hexIsLeaf distinguishes leaf nodes (which store value hashes) from
-- branch nodes (which store branch hashes)
data HexIndirect a = HexIndirect
    { hexJump :: HexKey
    , hexValue :: a
    , hexIsLeaf :: Bool
    }
    deriving (Show, Eq, Functor, Ord)

-- | Create a leaf indirect (stores value hash)
mkLeafIndirect :: HexKey -> a -> HexIndirect a
mkLeafIndirect hexJump hexValue = HexIndirect{hexJump, hexValue, hexIsLeaf = True}

-- | Create a branch indirect (stores branch hash)
mkBranchIndirect :: HexKey -> a -> HexIndirect a
mkBranchIndirect hexJump hexValue = HexIndirect{hexJump, hexValue, hexIsLeaf = False}

-- | Prefix a HexIndirect with additional key segments
prefixHex :: HexKey -> HexIndirect a -> HexIndirect a
prefixHex q HexIndirect{hexJump, hexValue, hexIsLeaf} = HexIndirect{hexJump = q ++ hexJump, hexValue, hexIsLeaf}

-- | Conversion from user key-value types to MPF types
data FromHexKV k v a = FromHexKV
    { fromHexK :: k -> HexKey
    , fromHexV :: v -> a
    }

-- | Compare two hex keys and return their common prefix and the remaining suffixes
compareHexKeys :: HexKey -> HexKey -> (HexKey, HexKey, HexKey)
compareHexKeys [] ys = ([], [], ys)
compareHexKeys xs [] = ([], xs, [])
compareHexKeys (x : xs) (y : ys)
    | x == y =
        let (common, other, remaining) = compareHexKeys xs ys
        in  (x : common, other, remaining)
    | otherwise = ([], x : xs, y : ys)

-- | Convert a ByteString to a HexKey (2 nibbles per byte)
byteStringToHexKey :: ByteString -> HexKey
byteStringToHexKey = concatMap byteToNibbles . B.unpack
  where
    byteToNibbles :: Word8 -> [HexDigit]
    byteToNibbles b =
        [ HexDigit (b `shiftR` 4) -- high nibble
        , HexDigit (b .&. 0x0f) -- low nibble
        ]

-- | Convert a HexKey back to a ByteString
-- For odd length keys, the last nibble is stored in the high nibble with low nibble = 0
-- This ensures correct round-tripping with byteStringToHexKey and take
hexKeyToByteString :: HexKey -> ByteString
hexKeyToByteString = B.pack . pairNibbles
  where
    pairNibbles :: [HexDigit] -> [Word8]
    pairNibbles [] = []
    pairNibbles [HexDigit hi] = [hi `shiftL` 4] -- odd: single nibble in HIGH position
    pairNibbles (HexDigit hi : HexDigit lo : rest) =
        ((hi `shiftL` 4) .|. lo) : pairNibbles rest

-- | Serialize a HexDigit
putHexDigit :: HexDigit -> PutM ()
putHexDigit (HexDigit d) = putWord8 d

-- | Deserialize a HexDigit
getHexDigit :: Get HexDigit
getHexDigit = do
    d <- getWord8
    case mkHexDigit d of
        Just hd -> return hd
        Nothing -> fail $ "Invalid hex digit: " ++ show d

-- | Serialize a HexKey with length prefix
-- Format: Word16be (number of nibbles) + packed bytes (2 nibbles per byte, padded if odd)
putHexKey :: HexKey -> PutM ()
putHexKey k = do
    putWord16be $ fromIntegral $ length k
    putByteString $ hexKeyToByteString k

-- | Deserialize a HexKey
getHexKey :: Get HexKey
getHexKey = do
    len <- getWord16be
    let byteLen = (fromIntegral len + 1) `div` 2 -- ceiling division
    bs <- getByteString byteLen
    return $ take (fromIntegral len) $ byteStringToHexKey bs

-- | Serialize a sized ByteString with length prefix
putSizedByteString :: BA.ByteArrayAccess a => a -> PutM ()
putSizedByteString bs = do
    let len = fromIntegral $ BA.length bs
    putWord16be len
    putByteString $ convert bs

-- | Deserialize a sized ByteString
getSizedByteString :: BA.ByteArray a => Get a
getSizedByteString = do
    len <- getWord16be
    bs <- getByteString (fromIntegral len)
    return $ convert bs

-- | Serialize a HexIndirect
putHexIndirect :: BA.ByteArrayAccess a => HexIndirect a -> PutM ()
putHexIndirect HexIndirect{hexJump, hexValue, hexIsLeaf} = do
    putHexKey hexJump
    putSizedByteString hexValue
    putWord8 (if hexIsLeaf then 1 else 0)

-- | Deserialize a HexIndirect
getHexIndirect :: BA.ByteArray a => Get (HexIndirect a)
getHexIndirect = do
    hexJump <- getHexKey
    hexValue <- getSizedByteString
    isLeafByte <- getWord8
    let hexIsLeaf = isLeafByte /= 0
    pure HexIndirect{hexJump, hexValue, hexIsLeaf}

-- | Prism for HexKey serialization
hexKeyPrism :: Prism' ByteString HexKey
hexKeyPrism = prism' (evalPutM . putHexKey) (evalGetM getHexKey)

-- | Prism for HexIndirect serialization
hexIndirectPrism
    :: Prism' ByteString a -> Prism' ByteString (HexIndirect a)
hexIndirectPrism prismA =
    prism'
        (evalPutM . putHexIndirect . fmap (review prismA))
        ( unsafeEvalGet $ do
            HexIndirect{hexJump = k, hexValue = x, hexIsLeaf = isLeaf} <-
                getHexIndirect
            pure $ preview prismA x <&> \a ->
                HexIndirect{hexJump = k, hexValue = a, hexIsLeaf = isLeaf}
        )

-- | Codecs for MPF key-value storage
mpfCodecs :: Prism' ByteString a -> Codecs (KV HexKey (HexIndirect a))
mpfCodecs prismA =
    Codecs
        { keyCodec = hexKeyPrism
        , valueCodec = hexIndirectPrism prismA
        }

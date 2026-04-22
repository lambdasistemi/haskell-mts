-- |
-- Module      : MPF.Blake2bSpec
-- Description : MPF root parity between pure Blake2b and crypton
-- Copyright   : (c) Paolo Veronelli, 2026
-- License     : Apache-2.0
--
-- Cross-checks the MPF write path after routing it through the
-- pure-Haskell Blake2b implementation used by @csmt-verify@. The
-- load-bearing property is root parity: building the same trie with
-- @crypton@ and with the pure implementation must produce byte-identical
-- roots, otherwise the browser/WASM port is invalid.
module MPF.Blake2bSpec (spec) where

import Control.Lens (Prism', prism')
import Control.Monad (forM_)
import Crypto.Hash (Blake2b_256, Digest, hash)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Maybe (fromMaybe)
import Database.KV.Transaction (query, runTransactionUnguarded)
import MPF.Backend.Pure
    ( emptyMPFInMemoryDB
    , mpfPureDatabase
    , runMPFPure
    )
import MPF.Backend.Standalone
    ( MPFStandalone (..)
    , MPFStandaloneCodecs (..)
    )
import MPF.Hashes
    ( MPFHash (..)
    , MPFHashing (..)
    , isoMPFHash
    , mkMPFHash
    , nibbleBytes
    , nullHash
    , packHexKey
    , renderMPFHash
    )
import MPF.Insertion (inserting)
import MPF.Interface
    ( FromHexKV (..)
    , HexDigit (..)
    , byteStringToHexKey
    , hexIsLeaf
    , hexJump
    , hexValue
    )
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
    ( Gen
    , arbitrary
    , choose
    , forAll
    , vectorOf
    , (===)
    )

spec :: Spec
spec = describe "MPF Blake2b parity" $ do
    prop "pure Blake2b and crypton produce identical MPF roots"
        $ forAll genKVs
        $ \kvs ->
            rootWith pureHash kvs === rootWith cryptonHash kvs

pureHash :: ByteString -> MPFHash
pureHash = mkMPFHash

cryptonHash :: ByteString -> MPFHash
cryptonHash bs =
    MPFHash $ convert (hash bs :: Digest Blake2b_256)

rootWith
    :: (ByteString -> MPFHash)
    -> [(ByteString, ByteString)]
    -> Maybe ByteString
rootWith mkHash kvs =
    fmap renderMPFHash
        . fst
        . runMPFPure emptyMPFInMemoryDB
        . runTransactionUnguarded (mpfPureDatabase byteCodecs)
        $ do
            forM_ kvs $ \(k, v) ->
                inserting
                    []
                    (fromHexKVByteString mkHash)
                    (hashingWith mkHash)
                    MPFStandaloneKVCol
                    MPFStandaloneMPFCol
                    k
                    v
            mRoot <- query MPFStandaloneMPFCol []
            pure $ case mRoot of
                Nothing -> Nothing
                Just node ->
                    let hashing = hashingWith mkHash
                    in  Just
                            $ if hexIsLeaf node
                                then
                                    leafHash
                                        hashing
                                        (hexJump node)
                                        (hexValue node)
                                else hexValue node

fromHexKVByteString
    :: (ByteString -> MPFHash)
    -> FromHexKV ByteString ByteString MPFHash
fromHexKVByteString mkHash =
    FromHexKV
        { fromHexK = byteStringToHexKey . renderMPFHash . mkHash
        , fromHexV = mkHash
        , hexTreePrefix = const []
        }

hashingWith :: (ByteString -> MPFHash) -> MPFHashing MPFHash
hashingWith mkHash =
    MPFHashing
        { leafHash = \suffix valueDigest ->
            mkHash
                $ leafHead suffix
                    <> leafTail suffix
                    <> renderMPFHash valueDigest
        , merkleRoot = computeMerkleRootWith mkHash
        , branchHash = \prefix merkle ->
            mkHash $ nibbleBytes prefix <> renderMPFHash merkle
        }

computeMerkleRootWith
    :: (ByteString -> MPFHash)
    -> [Maybe MPFHash]
    -> MPFHash
computeMerkleRootWith mkHash children =
    pairwiseReduce
        $ map (fromMaybe zeroHash)
        $ take 16 (children ++ repeat Nothing)
  where
    zeroHash = nullHash

    pairwiseReduce [] = nullHash
    pairwiseReduce [h] = h
    pairwiseReduce hs = pairwiseReduce (pairUp hs)

    pairUp [] = []
    pairUp [h] = [h]
    pairUp (a : b : rest) =
        mkHash (renderMPFHash a <> renderMPFHash b) : pairUp rest

leafHead :: [HexDigit] -> ByteString
leafHead [] = B.singleton 0xff
leafHead (d : ds)
    | even (length (d : ds)) = B.singleton 0xff
    | otherwise = case d of
        HexDigit nibble -> B.pack [0x00, nibble]

leafTail :: [HexDigit] -> ByteString
leafTail [] = B.empty
leafTail suffix@(_ : ds)
    | even (length suffix) = packHexKey suffix
    | otherwise = packHexKey ds

byteCodecs :: MPFStandaloneCodecs ByteString ByteString MPFHash
byteCodecs =
    MPFStandaloneCodecs
        { mpfKeyCodec = bytesCodec
        , mpfValueCodec = bytesCodec
        , mpfNodeCodec = isoMPFHash
        }

bytesCodec :: Prism' ByteString ByteString
bytesCodec = prism' id Just

genKVs :: Gen [(ByteString, ByteString)]
genKVs = do
    n <- choose (0, 24)
    vectorOf n $ do
        kLen <- choose (0, 96)
        vLen <- choose (0, 96)
        k <- B.pack <$> vectorOf kLen arbitrary
        v <- B.pack <$> vectorOf vLen arbitrary
        pure (k, v)

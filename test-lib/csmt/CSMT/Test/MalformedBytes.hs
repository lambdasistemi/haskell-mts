-- |
-- Module      : CSMT.Test.MalformedBytes
-- Description : Damaged and arbitrary proof bytes
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- Generators of bytes for the decoders of proofs. The bytes are the
-- rendering of an honest proof, of a proof damaged in its fields, or
-- noise, and then damaged as bytes (a byte replaced, inserted,
-- deleted, a tail cut, bytes appended, a small number written in a
-- longer form) or as CBOR (a number replaced, a list shortened,
-- lengthened or made indefinite, an item of another type). The
-- damaged bytes mostly start as a proof so that they reach the
-- decoder's deeper items.
module CSMT.Test.MalformedBytes
    ( genInclusionBytes
    , genExclusionBytes
    , genCompletenessBytes
    , genNoise
    , damageBytes
    , shrinkBytes
    , describeBytes
    )
where

import CSMT.Core.CBOR
    ( renderCompletenessProof
    , renderExclusionProof
    , renderProof
    )
import CSMT.Core.Completeness (CompletenessProof (..))
import CSMT.Proof.Insertion (InclusionProof (..))
import CSMT.Test.Malformed
    ( Tree (..)
    , genHeldStatement
    , genWild
    )
import CSMT.Test.MalformedExclusion
    ( exclusionOf
    , genAbsentTarget
    , genHeldExclusion
    , genWildExclusion
    )
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Term (Term (..), decodeTerm, encodeTerm)
import Codec.CBOR.Write (toLazyByteString)
import Control.Monad (foldM)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Word (Word8)
import Test.QuickCheck
    ( Gen
    , arbitrary
    , choose
    , elements
    , frequency
    , listOf
    , oneof
    , shrinkIntegral
    , shrinkList
    , vectorOf
    )

-- | Bytes of an inclusion proof: honest, with its fields damaged, or
-- a statement the tree holds with its structure damaged; then
-- possibly damaged as bytes.
genInclusionBytes :: Tree -> Tree -> Gen ByteString
genInclusionBytes t o =
    frequency
        [ (4, renderProof <$> elements (Map.elems (treeProofs t)))
        , (2, renderProof <$> genWild t o)
        , (2, renderProof <$> genHeldStatement t o)
        ]
        >>= damageBytes

-- | Bytes of an exclusion proof: honest, with its fields damaged, or
-- for a key the tree holds; then possibly damaged as bytes.
genExclusionBytes :: Tree -> Tree -> Gen ByteString
genExclusionBytes t o =
    frequency
        [
            ( 4
            , do
                k <- genAbsentTarget t
                pure
                    $ maybe
                        B.empty
                        renderExclusionProof
                        (exclusionOf t k)
            )
        , (2, renderExclusionProof <$> genWildExclusion t o)
        , (2, renderExclusionProof <$> genHeldExclusion t o)
        ]
        >>= damageBytes

-- | Bytes of a completeness proof: the proof of an absent prefix
-- around a damaged exclusion proof, or merge operations and steps of
-- any size; then possibly damaged as bytes.
genCompletenessBytes :: Tree -> Tree -> Gen ByteString
genCompletenessBytes t o =
    oneof
        [ renderCompletenessProof . CompletenessEmpty
            <$> genWildExclusion t o
        , do
            n <- choose (0, 6)
            ops <- vectorOf n ((,) <$> genIndex <*> genIndex)
            steps <- proofSteps <$> genWild t o
            pure
                $ renderCompletenessProof
                $ CompletenessWitness ops steps
        ]
        >>= damageBytes
  where
    genIndex =
        frequency
            [ (4, choose (0, 4))
            , (1, choose (-3, -1))
            , (1, elements [minBound, maxBound])
            ]

-- | Bytes that are not a proof: random bytes, or the headers of
-- CBOR lists, numbers and byte strings, some of them claiming far
-- more than the bytes that follow hold.
genNoise :: Gen ByteString
genNoise =
    oneof
        [ do
            n <- choose (0, 80)
            B.pack <$> vectorOf n arbitrary
        , B.concat <$> listOf (elements fragments)
        ]
  where
    fragments =
        B.pack
            <$> [ [0x84]
                , [0x83, 0x01]
                , [0x82]
                , [0x81, 0x00]
                , [0x80]
                , [0x00]
                , [0x01]
                , [0x02]
                , [0x18, 0x00]
                , [0x40]
                , [0x58, 0x20]
                , [0x5f]
                , [0x9f]
                , [0xff]
                , [0x9b, 0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]
                , [0x9b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]
                , [0x9a, 0xff, 0xff, 0xff, 0xff]
                , [0x5b, 0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]
                , [0x3b, 0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]
                , [0x1b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]
                ]

-- | The bytes damaged zero to three times, as bytes or as CBOR.
damageBytes :: ByteString -> Gen ByteString
damageBytes bytes = do
    n <-
        frequency
            [(3, pure 0), (4, pure 1), (2, pure 2), (1, pure 3)]
    foldM
        (\b _ -> oneof [damageAsBytes b, damageAsTerm b])
        bytes
        [1 .. n :: Int]

interesting :: [Word8]
interesting =
    [ 0x00
    , 0x01
    , 0x02
    , 0x17
    , 0x18
    , 0x19
    , 0x1a
    , 0x1b
    , 0x1f
    , 0x20
    , 0x40
    , 0x58
    , 0x59
    , 0x5f
    , 0x80
    , 0x81
    , 0x82
    , 0x83
    , 0x84
    , 0x85
    , 0x9f
    , 0xa0
    , 0xbf
    , 0xc0
    , 0xf6
    , 0xff
    ]

genByte :: Gen Word8
genByte = oneof [elements interesting, arbitrary]

damageAsBytes :: ByteString -> Gen ByteString
damageAsBytes b
    | B.null b = B.pack <$> (choose (1, 4) >>= (`vectorOf` genByte))
    | otherwise = do
        i <- choose (0, B.length b - 1)
        j <- choose (0, B.length b - 1)
        extra <- choose (1, 4) >>= (`vectorOf` genByte)
        w <- genByte
        let (before, after) = B.splitAt i b
        oneof
            [ pure (B.concat [before, B.singleton w, B.drop 1 after])
            , pure (B.concat [before, B.pack extra, after])
            , pure (B.concat [before, B.drop 1 after])
            , pure before
            , pure (b <> B.pack extra)
            , pure (B.pack extra <> b)
            , pure (B.concat [before, B.take (j - i + 1) after, after])
            , pure
                ( B.concat
                    [ before
                    , B.pack [0x18]
                    , B.take 1 after
                    , B.drop 1 after
                    ]
                )
            ]

damageAsTerm :: ByteString -> Gen ByteString
damageAsTerm b =
    case deserialiseFromBytes decodeTerm (BL.fromStrict b) of
        Right (_, term) ->
            BL.toStrict . toLazyByteString . encodeTerm
                <$> mutateTerm term
        Left _ -> damageAsBytes b

mutateTerm :: Term -> Gen Term
mutateTerm term =
    case term of
        TList xs | not (null xs) -> oneof [replaceHere, inside TList xs]
        TListI xs | not (null xs) -> oneof [replaceHere, inside TListI xs]
        _ -> replaceHere
  where
    inside wrap xs = do
        i <- choose (0, length xs - 1)
        x' <- mutateTerm (xs !! i)
        pure
            ( wrap
                [if n == i then x' else x | (n, x) <- zip [0 ..] xs]
            )
    replaceHere =
        oneof
            $ pure TNull
                : pure (TInt 2)
                : case term of
                    TInt n ->
                        [ pure (TInt (n + 1))
                        , pure (TInt (negate n))
                        , pure (TInt (n - 1))
                        , pure (TInteger (2 ^ (64 :: Int)))
                        , pure (TInteger (negate (2 ^ (64 :: Int))))
                        , pure (TBytes B.empty)
                        , pure (TList [TInt n])
                        ]
                    TBytes x ->
                        [ pure (TBytes (x <> B.singleton 0))
                        , pure (TBytes (B.drop 1 x))
                        , pure (TBytes B.empty)
                        , pure (TBytesI (BL.fromStrict x))
                        , pure (TInt 0)
                        ]
                    TList xs -> listVariants TList xs
                    TListI xs -> listVariants TList xs
                    _ -> []
    listVariants wrap xs =
        [ pure (wrap (drop 1 xs))
        , pure (wrap (take (length xs - 1) xs))
        , pure (wrap (xs <> [TInt 0]))
        , pure (wrap (xs <> xs))
        , pure (wrap (reverse xs))
        , pure (wrap [])
        , pure (TListI xs)
        , pure (TTagged 24 (wrap xs))
        ]

-- | Shorter bytes, with smaller bytes.
shrinkBytes :: ByteString -> [ByteString]
shrinkBytes = fmap B.pack . shrinkList shrinkIntegral . B.unpack

-- | The bytes in hexadecimal.
describeBytes :: ByteString -> String
describeBytes = concatMap hex . B.unpack
  where
    hex w = [digit (w `div` 16), digit (w `mod` 16)]
    digit d
        | d < 10 = toEnum (fromEnum '0' + fromIntegral d)
        | otherwise = toEnum (fromEnum 'a' + fromIntegral d - 10)

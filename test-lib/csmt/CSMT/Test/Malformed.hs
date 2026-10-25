-- |
-- Module      : CSMT.Test.Malformed
-- Description : Random trees and damaged inclusion proofs
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- Generators for properties about proofs that are not what the
-- tree produced. Every tree is built by the real insertion code,
-- every honest proof comes from 'buildInclusionProof', and every
-- generated proof starts as an honest one (of the key itself, of a
-- neighbour, or of another tree) with some of its fields damaged,
-- so that the damaged proof stays close enough to the real ones to
-- reach the arithmetic of the verifier.
--
-- All keys of one tree have the same length, one of 1, 2, 4 or 32
-- bytes; keys of a tree often share long prefixes or differ only in
-- their last bits.
module CSMT.Test.Malformed
    ( -- * Trees
      Tree (..)
    , genTree
    , genTreeOfKeyBits
    , mkTree
    , honestProof

      -- * Damaged proofs
    , genHeldStatement
    , genWrongValue
    , genAbsentKey
    , genOtherLengthKey
    , genWild
    , shrinkProof

      -- * Building blocks for other generators
    , genHash
    , genBits
    , genNear
    , flipBitAt
    , insertAt
    , damageStructure

      -- * Counterexample rendering
    , describeTree
    , describeProof
    , renderKey
    )
where

import CSMT.Hashes (Hash, hashHashing, mkHash)
import CSMT.Interface
    ( Direction (..)
    , Indirect (..)
    , Key
    , oppositeDirection
    )
import CSMT.Proof.Insertion
    ( InclusionProof (..)
    , ProofStep (..)
    )
import CSMT.Test.Lib
    ( evalPureFromEmptyDB
    , getRootHashM
    , hashCodecs
    , identityFromKV
    , insertMHash
    , proofM
    )
import Control.Monad (foldM, forM)
import Data.ByteString qualified as B
import Data.List (intercalate, nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Test.QuickCheck
    ( Gen
    , arbitrary
    , choose
    , elements
    , frequency
    , oneof
    , shrinkIntegral
    , shrinkList
    , suchThat
    , vectorOf
    )

-- | A tree built by the insertion code, with what the producer
-- says about it.
data Tree = Tree
    { treeKeyBits :: Int
    -- ^ the length of every key of the tree
    , treeLeaves :: Map Key Hash
    -- ^ the key-value pairs the tree holds
    , treeRoot :: Hash
    -- ^ the root hash, read back from the tree
    , treeProofs :: Map Key (InclusionProof Hash)
    -- ^ 'buildInclusionProof' of every held key
    }
    deriving (Show)

-- | The proof the producer built for a held key.
honestProof :: Tree -> Key -> Maybe (InclusionProof Hash)
honestProof Tree{treeProofs} k = Map.lookup k treeProofs

-- | Build the tree holding exactly these pairs, with the real
-- insertion and proof-building code.
mkTree :: Int -> [(Key, Hash)] -> Tree
mkTree treeKeyBits leaves = evalPureFromEmptyDB $ do
    mapM_ (uncurry insertMHash) leaves
    mroot <- getRootHashM
    proofs <- forM leaves $ \(k, _) -> do
        mp <- proofM hashCodecs identityFromKV hashHashing k
        pure
            ( k
            , maybe
                (error "mkTree: no proof for an inserted key")
                snd
                mp
            )
    pure
        Tree
            { treeKeyBits
            , treeLeaves = Map.fromList leaves
            , treeRoot =
                fromMaybe (error "mkTree: empty tree") mroot
            , treeProofs = Map.fromList proofs
            }

keyBitLengths :: [Int]
keyBitLengths = [8, 16, 32, 256]

genHash :: Gen Hash
genHash = mkHash . B.pack <$> vectorOf 8 arbitrary

genBits :: Int -> Gen Key
genBits n = vectorOf n (elements [L, R])

flipBitAt :: Int -> Key -> Key
flipBitAt i k = case splitAt i k of
    (before, d : after) -> before <> (oppositeDirection d : after)
    _ -> k

insertAt :: Int -> Key -> Key -> Key
insertAt i extra k = let (a, b) = splitAt i k in a <> extra <> b

-- | A key of the same length, often a neighbour of the given one:
-- sharing a long prefix, or differing only in the last bits.
genNear :: Int -> Key -> Gen Key
genNear bits k =
    oneof
        [ genBits bits
        , (`flipBitAt` k) <$> choose (0, bits - 1)
        , (`flipBitAt` k) <$> choose (bits `div` 2, bits - 1)
        , (`flipBitAt` k) <$> choose (max 0 (bits - 3), bits - 1)
        , do
            p <- choose (0, bits - 1)
            rest <- genBits (bits - p - 1)
            pure $ case drop p k of
                d : _ -> take p k <> (oppositeDirection d : rest)
                [] -> k
        ]

genKeys :: Int -> Int -> Gen [Key]
genKeys bits n = do
    k0 <- genBits bits
    nub
        <$> foldM
            ( \acc _ ->
                (: acc) <$> (elements acc >>= genNear bits)
            )
            [k0]
            [2 .. n]

-- | A random tree: one key, two keys, or up to ten.
genTree :: Gen Tree
genTree = elements keyBitLengths >>= genTreeOfKeyBits

-- | A random tree with keys of the given length in bits.
genTreeOfKeyBits :: Int -> Gen Tree
genTreeOfKeyBits bits = do
    n <- frequency [(2, pure 1), (3, pure 2), (5, choose (3, 10))]
    keys <- genKeys bits n
    mkTree bits <$> traverse (\k -> (k,) <$> genHash) keys

type Damage =
    Tree -> Tree -> InclusionProof Hash -> Gen (InclusionProof Hash)

applyDamages
    :: [Damage]
    -> Tree
    -> Tree
    -> InclusionProof Hash
    -> Gen (InclusionProof Hash)
applyDamages damages t o p = do
    n <-
        frequency
            [(3, pure 0), (4, pure 1), (2, pure 2), (1, pure 3)]
    foldM
        (\q _ -> elements damages >>= \damage -> damage t o q)
        p
        [1 .. n :: Int]

-- | A proof with a few of its structural fields damaged: root jump,
-- steps, step sizes and siblings.
damageStructure
    :: Tree
    -> Tree
    -> InclusionProof Hash
    -> Gen (InclusionProof Hash)
damageStructure = applyDamages structureDamages

structureDamages :: [Damage]
structureDamages =
    [ damageRootJump
    , damageSteps
    , damageInsertBadStep
    , damageConsumed
    , damageOversizeLastStep
    , damageSibling
    ]

allDamages :: [Damage]
allDamages =
    structureDamages
        <> [damageKey, damageValue, damageEveryConsumed]

genConsumed :: Int -> Gen Int
genConsumed bits =
    frequency
        [ (3, choose (1, 4))
        , (2, choose (1, bits))
        , (1, pure 0)
        , (2, choose (-10, -1))
        , (1, elements [minBound, maxBound, minBound + 1])
        , (2, choose (bits + 1, bits + 9))
        ]

genIndirect :: Int -> Gen (Indirect Hash)
genIndirect bits = Indirect <$> (choose (0, bits + 2) >>= genBits) <*> genHash

genStep :: Int -> Gen (ProofStep Hash)
genStep bits = ProofStep <$> genConsumed bits <*> genIndirect bits

damageRootJump :: Damage
damageRootJump t _ p@InclusionProof{proofKey, proofRootJump} =
    oneof
        [ pure p{proofRootJump = []}
        , (\j -> p{proofRootJump = take j proofKey})
            <$> choose (0, length proofKey)
        , (\i -> p{proofRootJump = flipBitAt i proofRootJump})
            <$> choose (0, max 0 (length proofRootJump - 1))
        , (\ex -> p{proofRootJump = proofRootJump <> ex})
            <$> (choose (1, 3) >>= genBits)
        , (\r -> p{proofRootJump = r})
            <$> (choose (0, treeKeyBits t + 2) >>= genBits)
        , pure p{proofRootJump = proofKey}
        , pure p{proofRootJump = take (length proofRootJump - 1) proofRootJump}
        ]

damageSteps :: Damage
damageSteps t o p@InclusionProof{proofSteps = steps} =
    oneof
        [ pure p{proofSteps = []}
        , (\n -> p{proofSteps = take n steps})
            <$> choose (0, length steps)
        , (\i -> p{proofSteps = removeAt i steps})
            <$> index
        , (\i -> p{proofSteps = duplicateAt i steps})
            <$> index
        , (\i j -> p{proofSteps = swapAt i j steps})
            <$> index
            <*> index
        , pure p{proofSteps = reverse steps}
        , (\s -> p{proofSteps = steps <> [s]})
            <$> genStep (treeKeyBits t)
        , (\s -> p{proofSteps = s : steps})
            <$> genStep (treeKeyBits t)
        , (\InclusionProof{proofSteps = other} -> p{proofSteps = other})
            <$> elements (Map.elems (treeProofs o))
        ]
  where
    index = choose (0, max 0 (length steps - 1))
    removeAt i xs = take i xs <> drop (i + 1) xs
    duplicateAt i xs = case drop i xs of
        x : _ -> take (i + 1) xs <> (x : drop (i + 1) xs)
        [] -> xs
    swapAt i j xs = case (drop i xs, drop j xs) of
        (x : _, y : _) ->
            [ if n == i then y else if n == j then x else z
            | (n, z) <- zip [0 ..] xs
            ]
        _ -> xs

mapStepAt
    :: Int
    -> (ProofStep Hash -> ProofStep Hash)
    -> [ProofStep Hash]
    -> [ProofStep Hash]
mapStepAt i f steps =
    [if n == i then f s else s | (n, s) <- zip [0 ..] steps]

damageConsumed :: Damage
damageConsumed t _ p@InclusionProof{proofSteps}
    | null proofSteps = pure p
    | otherwise = do
        i <- choose (0, length proofSteps - 1)
        let c = stepConsumed (proofSteps !! i)
        c' <-
            oneof
                [ genConsumed (treeKeyBits t)
                , (c +) <$> elements [-2, -1, 1, 2, treeKeyBits t]
                , pure 0
                , pure (negate c)
                ]
        pure
            p{proofSteps = mapStepAt i (\s -> s{stepConsumed = c'}) proofSteps}

-- | A step of a size no step of the key can have, or one reaching
-- past the key, inserted at a random place.
damageInsertBadStep :: Damage
damageInsertBadStep t _ p@InclusionProof{proofSteps = steps} = do
    i <- choose (0, length steps)
    size <-
        elements
            [0, 0, -1, -3, minBound, maxBound, 1, 2, treeKeyBits t + 1]
    sibling <- genIndirect (treeKeyBits t)
    pure
        p
            { proofSteps =
                take i steps <> [ProofStep size sibling] <> drop i steps
            }

-- | The step next to the root made longer than what is left of the
-- key.
damageOversizeLastStep :: Damage
damageOversizeLastStep _ _ p@InclusionProof{proofSteps = steps}
    | null steps = pure p
    | otherwise = do
        j <- choose (1, 9)
        pure
            p
                { proofSteps =
                    mapStepAt
                        (length steps - 1)
                        (\s -> s{stepConsumed = stepConsumed s + j})
                        steps
                }

damageEveryConsumed :: Damage
damageEveryConsumed t _ p@InclusionProof{proofSteps} = do
    steps <- forM proofSteps $ \s -> do
        c <- genConsumed (treeKeyBits t)
        pure s{stepConsumed = c}
    pure p{proofSteps = steps}

damageSibling :: Damage
damageSibling t o p@InclusionProof{proofSteps}
    | null proofSteps = pure p
    | otherwise = do
        i <- choose (0, length proofSteps - 1)
        let Indirect{jump, value} = stepSibling (proofSteps !! i)
            bits = treeKeyBits t
        sibling <-
            oneof
                [ (`Indirect` value) <$> (choose (0, bits + 2) >>= genBits)
                , pure (Indirect (flipBitAt 0 jump) value)
                , pure (Indirect (take (length jump - 1) jump) value)
                , (\d -> Indirect (jump <> [d]) value)
                    <$> elements [L, R]
                , Indirect jump <$> genHash
                , Indirect jump
                    <$> elements (Map.elems (treeLeaves o))
                ]
        pure
            p
                { proofSteps =
                    mapStepAt i (\s -> s{stepSibling = sibling}) proofSteps
                }

damageKey :: Damage
damageKey t o p@InclusionProof{proofKey, proofRootJump} =
    oneof
        [ flipped <$> choose (0, lastIndex)
        , flipped <$> choose (0, max 0 (length proofRootJump - 1))
        , flipped <$> choose (max 0 (lastIndex - 2), lastIndex)
        , setKey <$> elements (Map.keys (treeLeaves t))
        , setKey <$> elements (Map.keys (treeLeaves o))
        , setKey <$> genBits (treeKeyBits t)
        , (\j -> setKey (take j proofKey)) <$> choose (0, lastIndex)
        , (\ex -> setKey (proofKey <> ex))
            <$> (choose (1, 9) >>= genBits)
        , (\ex -> setKey (insertAt (length proofRootJump) ex proofKey))
            <$> (choose (1, 4) >>= genBits)
        , pure (setKey (drop 1 proofKey))
        ]
  where
    lastIndex = max 0 (length proofKey - 1)
    setKey k = p{proofKey = k}
    flipped i = setKey (flipBitAt i proofKey)

damageValue :: Damage
damageValue t o p =
    (\v -> p{proofValue = v})
        <$> oneof
            [ elements (Map.elems (treeLeaves t))
            , elements (Map.elems (treeLeaves o))
            , genHash
            ]

genBase :: Tree -> Tree -> Gen (InclusionProof Hash)
genBase t o =
    oneof
        [ elements (Map.elems (treeProofs t))
        , elements (Map.elems (treeProofs o))
        ]

-- | The base proof for a statement about the key: the key's own
-- proof if it is held, else the proof of a neighbour.
baseFor :: Tree -> Tree -> Key -> Gen (InclusionProof Hash)
baseFor t o k =
    maybe (genBase t o) (\p -> oneof [pure p, genBase t o])
        $ honestProof t k

-- | A proof of a statement the tree holds: the structure is
-- damaged, key and value are the held pair.
genHeldStatement
    :: Tree -> Tree -> Gen (InclusionProof Hash)
genHeldStatement t o = do
    (k, v) <- elements (Map.toList (treeLeaves t))
    base <- baseFor t o k
    applyDamages structureDamages t o base{proofKey = k, proofValue = v}

-- | A proof for a held key with a value the tree does not hold
-- for it.
genWrongValue :: Tree -> Tree -> Gen (InclusionProof Hash)
genWrongValue t o = do
    (k, v) <- elements (Map.toList (treeLeaves t))
    base <- baseFor t o k
    v' <-
        oneof
            [ elements (Map.elems (treeLeaves t))
            , elements (Map.elems (treeLeaves o))
            , genHash
            ]
            `suchThat` (/= v)
    applyDamages structureDamages t o base{proofKey = k, proofValue = v'}

genForeignValue :: Tree -> Tree -> Gen Hash
genForeignValue t o =
    oneof
        [ elements (Map.elems (treeLeaves t))
        , elements (Map.elems (treeLeaves o))
        , genHash
        ]

-- | A proof for a key of the right length that the tree does not
-- hold, usually a neighbour of a held key.
genAbsentKey :: Tree -> Tree -> Gen (InclusionProof Hash)
genAbsentKey t o = do
    k0 <- elements (Map.keys (treeLeaves t))
    k <-
        oneof
            [ genNear (treeKeyBits t) k0
            , elements (Map.keys (treeLeaves o))
            ]
            `suchThat` (\k' -> length k' == treeKeyBits t && Map.notMember k' (treeLeaves t))
    v <- genForeignValue t o
    base <- baseFor t o k0
    applyDamages structureDamages t o base{proofKey = k, proofValue = v}

-- | A proof for a key whose length differs from the keys of the
-- tree. The value is never a node hash of the tree: leaf and
-- branch hashes are not domain-separated, so a branch hash given as
-- the value of a key of another length is outside what a verifier
-- can refuse.
genOtherLengthKey
    :: Tree -> Tree -> Gen (InclusionProof Hash)
genOtherLengthKey t o = do
    k0 <- elements (Map.keys (treeLeaves t))
    base <- baseFor t o k0
    let bits = treeKeyBits t
        rootJumpLength = length (proofRootJump base)
    k <-
        oneof
            [ (`take` k0) <$> choose (0, bits - 1)
            , (k0 <>) <$> (choose (1, 9) >>= genBits)
            , (\ex -> insertAt rootJumpLength ex k0)
                <$> (choose (1, 4) >>= genBits)
            , (\i ex -> insertAt i ex k0)
                <$> choose (0, bits)
                <*> (choose (1, 4) >>= genBits)
            , choose (0, bits + 9) >>= genBits
            , pure (drop 1 k0)
            ]
            `suchThat` ((/= bits) . length)
    v <- genForeignValue t o
    applyDamages structureDamages t o base{proofKey = k, proofValue = v}

-- | A proof with any field damaged, to be given to the verifier
-- without any expectation about its verdict.
genWild :: Tree -> Tree -> Gen (InclusionProof Hash)
genWild t o = do
    base <- genBase t o
    n <- choose (1, 5 :: Int)
    foldM
        (\q _ -> elements allDamages >>= \damage -> damage t o q)
        base
        [1 .. n]

-- | Smaller proofs for a failing one: fewer steps, shorter jumps,
-- smaller step sizes. The statement (key and value) is kept, so a
-- shrunk counterexample stays in the class it was generated for.
shrinkProof :: InclusionProof Hash -> [InclusionProof Hash]
shrinkProof p@InclusionProof{proofSteps, proofRootJump} =
    [p{proofSteps = s} | s <- shrinkList shrinkStep proofSteps]
        <> [p{proofRootJump = j} | j <- shrinkList (const []) proofRootJump]
  where
    shrinkStep s@ProofStep{stepConsumed, stepSibling} =
        [s{stepConsumed = c} | c <- shrinkIntegral stepConsumed]
            <> [ s{stepSibling = stepSibling{jump = j}}
               | j <- shrinkList (const []) (jump stepSibling)
               ]

renderKey :: Key -> String
renderKey = concatMap $ \case
    L -> "0"
    R -> "1"

-- | The pairs of a tree and its root, keys as bit strings.
describeTree :: Tree -> String
describeTree Tree{treeLeaves, treeRoot} =
    "tree root "
        <> show treeRoot
        <> " holds "
        <> intercalate
            ", "
            [renderKey k <> " -> " <> show v | (k, v) <- Map.toList treeLeaves]

-- | A proof with keys as bit strings.
describeProof :: InclusionProof Hash -> String
describeProof InclusionProof{proofKey, proofValue, proofSteps, proofRootJump} =
    "key "
        <> renderKey proofKey
        <> " value "
        <> show proofValue
        <> " root jump "
        <> renderKey proofRootJump
        <> " steps "
        <> show
            [ (stepConsumed, renderKey jump, value)
            | ProofStep{stepConsumed, stepSibling = Indirect{jump, value}} <-
                proofSteps
            ]

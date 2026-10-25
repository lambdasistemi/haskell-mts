-- |
-- Module      : CSMT.Test.MalformedCompleteness
-- Description : Random trees and damaged completeness statements
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- Generators for properties about completeness proofs that are not
-- what the tree produced. The trees are those of
-- "CSMT.Test.Malformed", every honest proof comes from
-- 'generateProof', and what the tree holds under a prefix is read
-- from the pairs the tree was built from, not from the producer.
--
-- A 'Statement' is what a verifier is given: a prefix, the leaves
-- said to be all those under it, and a proof. Every generated
-- statement starts as an honest one and has some of its parts
-- damaged: leaves dropped, added (from the tree outside the prefix,
-- from another tree or invented), duplicated, reordered or changed;
-- the prefix shortened, lengthened or moved to a sibling; the merge
-- operations given indices out of range, repeated, missing or extra;
-- the inclusion steps damaged as the steps of an inclusion proof
-- are; the proof replaced by the proof of another prefix or by one
-- of the other kind. Damages to the leaves often keep the merge
-- operations consistent with the damaged list, so that the proof
-- stays close enough to an honest one to reach the arithmetic of
-- the verifier.
module CSMT.Test.MalformedCompleteness
    ( -- * Statements
      Statement (..)
    , leavesUnder
    , completenessOf
    , collectedUnder
    , honestStatement

      -- * Prefixes
    , genPrefix
    , genOtherPrefix

      -- * Damaged statements
    , genDamagedStatement
    , genWildStatement
    , damageSteps
    , shrinkStatement

      -- * Counterexample rendering
    , describeStatement
    )
where

import CSMT.Backend.Pure (pureDatabase)
import CSMT.Backend.Standalone (Standalone (StandaloneCSMTCol))
import CSMT.Core.Completeness (CompletenessProof (..))
import CSMT.Hashes (Hash)
import CSMT.Interface
    ( Direction (..)
    , Indirect (..)
    , Key
    )
import CSMT.Proof.Completeness (collectValues, generateProof)
import CSMT.Proof.Insertion
    ( InclusionProof (..)
    , ProofStep (..)
    )
import CSMT.Test.Lib
    ( evalPureFromEmptyDB
    , hashCodecs
    , insertMHash
    )
import CSMT.Test.Malformed
    ( Tree (..)
    , damageStructure
    , flipBitAt
    , genBits
    , genHash
    , genNear
    , renderKey
    )
import CSMT.Test.MalformedExclusion
    ( describeExclusion
    , exclusionOf
    , genWildExclusion
    )
import Control.Monad (foldM)
import Data.Bifunctor (bimap)
import Data.List (elemIndex, isPrefixOf)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Database.KV.Transaction (runTransactionUnguarded)
import Test.QuickCheck
    ( Gen
    , choose
    , elements
    , frequency
    , oneof
    , shrinkIntegral
    , shrinkList
    , shuffle
    )

-- | What a verifier is given.
data Statement = Statement
    { stPrefix :: Key
    -- ^ the prefix the verifier asked about
    , stLeaves :: [Indirect Hash]
    -- ^ the leaves said to be all those under the prefix
    , stProof :: CompletenessProof Hash
    }
    deriving (Show)

-- | What the tree holds under the prefix, in ascending key order,
-- read from the pairs it was built from.
leavesUnder :: Tree -> Key -> [Indirect Hash]
leavesUnder Tree{treeLeaves} p =
    [ Indirect k v
    | (k, v) <- Map.toAscList treeLeaves
    , p `isPrefixOf` k
    ]

-- | The proof the producer builds for a prefix. The tree is built
-- again from its pairs, in key order.
completenessOf :: Tree -> Key -> Maybe (CompletenessProof Hash)
completenessOf Tree{treeLeaves} p = evalPureFromEmptyDB $ do
    mapM_ (uncurry insertMHash) (Map.toList treeLeaves)
    runTransactionUnguarded (pureDatabase hashCodecs)
        $ generateProof StandaloneCSMTCol [] p

-- | The leaves the producer collects under a prefix.
collectedUnder :: Tree -> Key -> [Indirect Hash]
collectedUnder Tree{treeLeaves} p = evalPureFromEmptyDB $ do
    mapM_ (uncurry insertMHash) (Map.toList treeLeaves)
    runTransactionUnguarded (pureDatabase hashCodecs)
        $ collectValues StandaloneCSMTCol [] p

-- | The statement of a prefix as the producer gives it: the leaves
-- under the prefix and the proof built for it.
honestStatement :: Tree -> Key -> Maybe Statement
honestStatement t p =
    Statement p (leavesUnder t p) <$> completenessOf t p

-- | The honest statement of a prefix, or the leaves under it with
-- an empty proof where the producer builds none.
honestOrBare :: Tree -> Key -> Statement
honestOrBare t p =
    fromMaybe
        (Statement p (leavesUnder t p) (CompletenessWitness [] []))
        (honestStatement t p)

-- | A prefix of the tree's keys, never longer than they are: the
-- empty prefix, the beginning of a held key of any length, a whole
-- key, and the beginning of a neighbour of a key, which often
-- holds no leaf.
genPrefix :: Tree -> Gen Key
genPrefix Tree{treeKeyBits, treeLeaves} = do
    k <- elements (Map.keys treeLeaves)
    frequency
        [ (2, pure [])
        , (4, (`take` k) <$> choose (0, treeKeyBits))
        , (3, (`take` k) <$> choose (max 0 (treeKeyBits - 4), treeKeyBits))
        , (1, pure k)
        ,
            ( 4
            , do
                n <- choose (1, treeKeyBits)
                take n <$> genNear treeKeyBits k
            )
        ]

-- | A prefix other than the given one, in the ways a prefix is
-- mistaken: shorter, longer, moved to a sibling, or unrelated.
genOtherPrefix :: Tree -> Tree -> Key -> Gen Key
genOtherPrefix t o p =
    oneof
        [ (`take` p) <$> choose (0, length p)
        , pure []
        , (p <>) <$> (choose (1, 3) >>= genBits)
        , longerAlongAKey
        , (`flipBitAt` p) <$> choose (0, max 0 (length p - 1))
        , pure (take (length p - 1) p)
        , genPrefix t
        , genPrefix o
        ]
  where
    longerAlongAKey =
        case [k | k <- Map.keys (treeLeaves t), p `isPrefixOf` k] of
            [] -> (p <>) <$> (choose (1, 3) >>= genBits)
            ks -> do
                k <- elements ks
                (`take` k) <$> choose (length p, length k)

type Damage = Tree -> Tree -> Statement -> Gen Statement

-- | The damages, by name, that 'genDamagedStatement' picks from.
damages :: [(String, Damage)]
damages =
    [ ("a leaf dropped", damageDropLeaf)
    , ("a leaf added", damageAddLeaf)
    , ("a leaf changed", damageChangeLeaf)
    , ("leaves reordered", damageReorder)
    , ("the prefix moved", damagePrefix)
    , ("merge operations damaged", damageOps)
    , ("inclusion steps damaged", damageSteps)
    , ("the proof replaced", damageProof)
    ]

onOps
    :: ([(Int, Int)] -> [(Int, Int)])
    -> CompletenessProof Hash
    -> CompletenessProof Hash
onOps f (CompletenessWitness ops steps) =
    CompletenessWitness (f ops) steps
onOps _ proof = proof

onIndices :: (Int -> Int) -> [(Int, Int)] -> [(Int, Int)]
onIndices f = fmap (bimap f f)

insertAtIndex :: Int -> a -> [a] -> [a]
insertAtIndex i x xs = let (a, b) = splitAt i xs in a <> (x : b)

removeAtIndex :: Int -> [a] -> [a]
removeAtIndex i xs = take i xs <> drop (i + 1) xs

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex i x xs = take i xs <> (x : drop (i + 1) xs)

genForeignValue :: Tree -> Tree -> Gen Hash
genForeignValue t o =
    oneof
        [ elements (Map.elems (treeLeaves t))
        , elements (Map.elems (treeLeaves o))
        , genHash
        ]

-- | A leaf the tree does not hold under the prefix: one held outside
-- it, one of another tree, one invented under it with a key of the
-- tree's length, or a copy of one of the leaves given.
genForeignLeaf
    :: Tree -> Tree -> Key -> [Indirect Hash] -> Gen (Indirect Hash)
genForeignLeaf t o p leaves =
    frequency
        [ (3, outside)
        , (3, invented)
        , (2, fromOther)
        , (2, copied)
        , (1, otherValue)
        ]
  where
    outside =
        case [ Indirect k v
             | (k, v) <- Map.toList (treeLeaves t)
             , not (p `isPrefixOf` k)
             ] of
            [] -> invented
            xs -> elements xs
    invented = do
        ex <- genBits (max 0 (treeKeyBits t - length p))
        Indirect (p <> ex) <$> genHash
    fromOther = do
        (k, v) <- elements (Map.toList (treeLeaves o))
        pure (Indirect k v)
    copied = case leaves of
        [] -> invented
        _ -> elements leaves
    otherValue = case leaves of
        [] -> invented
        _ -> do
            Indirect k _ <- elements leaves
            Indirect k <$> genForeignValue t o

damageDropLeaf :: Damage
damageDropLeaf _ _ s@Statement{stLeaves, stProof}
    | null stLeaves = pure s
    | otherwise = do
        i <- choose (0, length stLeaves - 1)
        keepOps <- elements [True, False]
        let close x = if x > i then x - 1 else x
            ops' =
                [ (close a, close b)
                | (a, b) <- opsOf stProof
                , a /= i
                , b /= i
                ]
        pure
            s
                { stLeaves = removeAtIndex i stLeaves
                , stProof =
                    if keepOps then stProof else onOps (const ops') stProof
                }

opsOf :: CompletenessProof Hash -> [(Int, Int)]
opsOf (CompletenessWitness ops _) = ops
opsOf _ = []

damageAddLeaf :: Damage
damageAddLeaf t o s@Statement{stPrefix, stLeaves, stProof} = do
    new <- genForeignLeaf t o stPrefix stLeaves
    i <-
        frequency
            [ (2, pure (length stLeaves))
            , (2, choose (0, length stLeaves))
            , (1, pure 0)
            ]
    -- Often the merge operations are shifted past the new leaf, so
    -- that the proof still describes the leaves it was built for.
    shift <- elements [True, True, False]
    pure
        s
            { stLeaves = insertAtIndex i new stLeaves
            , stProof =
                if shift
                    then
                        onOps
                            (onIndices (\x -> if x >= i then x + 1 else x))
                            stProof
                    else stProof
            }

damageChangeLeaf :: Damage
damageChangeLeaf t o s@Statement{stLeaves}
    | null stLeaves = pure s
    | otherwise = do
        i <- choose (0, length stLeaves - 1)
        let Indirect k v = stLeaves !! i
            bits = length k
        new <-
            oneof
                [ Indirect k <$> genForeignValue t o
                , (\j -> Indirect (flipBitAt j k) v) <$> choose (0, max 0 (bits - 1))
                , (\j -> Indirect (flipBitAt j k) v)
                    <$> choose (max 0 (bits - 3), max 0 (bits - 1))
                , pure (Indirect (take (bits - 1) k) v)
                , (\d -> Indirect (k <> [d]) v) <$> elements [L, R]
                ]
        pure s{stLeaves = replaceAtIndex i new stLeaves}

damageReorder :: Damage
damageReorder _ _ s@Statement{stLeaves, stProof}
    | n < 2 = pure s
    | otherwise = do
        perm <-
            oneof
                [ shuffle [0 .. n - 1]
                , (0 :) <$> shuffle [1 .. n - 1]
                , pure (reverse [0 .. n - 1])
                , swapTwo
                ]
        -- Often the merge operations follow the leaves to their new
        -- places, so that the proof still describes the same tree.
        follow <- elements [True, False]
        let place i = fromMaybe i (elemIndex i perm)
        pure
            s
                { stLeaves = [stLeaves !! i | i <- perm]
                , stProof =
                    if follow then onOps (onIndices place) stProof else stProof
                }
  where
    n = length stLeaves
    swapTwo = do
        i <- choose (0, n - 1)
        j <- choose (0, n - 1)
        pure
            [ if x == i then j else if x == j then i else x
            | x <- [0 .. n - 1]
            ]

damagePrefix :: Damage
damagePrefix t o s@Statement{stPrefix} = do
    p <- genOtherPrefix t o stPrefix
    pure s{stPrefix = p}

genIndex :: Int -> Gen Int
genIndex n =
    frequency
        [ (6, choose (0, max 0 (n - 1)))
        , (2, choose (n, n + 4))
        , (2, choose (-4, -1))
        , (1, elements [minBound, maxBound])
        ]

damageOps :: Damage
damageOps _ _ s@Statement{stLeaves, stProof = CompletenessWitness ops steps} = do
    ops' <-
        oneof
            [ outOfRange
            , duplicated
            , dropped
            , extra
            , swapped
            , pure (reverse ops)
            , same
            , swappedPair
            , pure (take (length ops - 1) ops)
            , secondUsedAgain
            ]
    pure s{stProof = CompletenessWitness ops' steps}
  where
    n = length stLeaves
    index = choose (0, max 0 (length ops - 1))
    outOfRange
        | null ops = pure ops
        | otherwise = do
            at <- index
            bad <- genIndex n
            first <- elements [True, False]
            let (i, j) = ops !! at
            pure
                $ replaceAtIndex
                    at
                    (if first then (bad, j) else (i, bad))
                    ops
    duplicated
        | null ops = pure ops
        | otherwise = do
            at <- index
            pure (insertAtIndex at (ops !! at) ops)
    dropped
        | null ops = pure ops
        | otherwise = (`removeAtIndex` ops) <$> index
    extra = do
        op <- (,) <$> genIndex n <*> genIndex n
        at <- choose (0, length ops)
        pure (insertAtIndex at op ops)
    swapped
        | length ops < 2 = pure ops
        | otherwise = do
            a <- index
            b <- index
            pure
                $ replaceAtIndex a (ops !! b)
                $ replaceAtIndex b (ops !! a) ops
    same
        | null ops = pure ops
        | otherwise = do
            at <- index
            let (i, _) = ops !! at
            pure (replaceAtIndex at (i, i) ops)
    swappedPair
        | null ops = pure ops
        | otherwise = do
            at <- index
            let (i, j) = ops !! at
            pure (replaceAtIndex at (j, i) ops)
    secondUsedAgain
        | length ops < 2 = pure ops
        | otherwise = do
            a <- index
            b <- index
            let (i, _) = ops !! a
                (_, j) = ops !! b
            pure (replaceAtIndex a (i, j) ops)
damageOps _ _ s = pure s

-- | The inclusion steps of a witness damaged.
damageSteps :: Tree -> Tree -> Statement -> Gen Statement
damageSteps t o s@Statement{stPrefix, stProof = CompletenessWitness ops steps} = do
    steps' <-
        oneof
            [ asInclusionSteps
            , consumedOfOne
            , consumedOfHead
            , pure (drop 1 steps)
            , pure (take (length steps - 1) steps)
            , pure (reverse steps)
            , stepsOfAnotherPrefix
            ]
    pure s{stProof = CompletenessWitness ops steps'}
  where
    index = choose (0, max 0 (length steps - 1))
    asInclusionSteps =
        proofSteps
            <$> damageStructure
                t
                o
                InclusionProof
                    { proofKey = stPrefix
                    , proofValue = treeRoot t
                    , proofSteps = steps
                    , proofRootJump = []
                    }
    consumedOfOne
        | null steps = pure steps
        | otherwise = do
            at <- index
            c' <- genConsumedOf (stepConsumed (steps !! at))
            pure
                $ replaceAtIndex
                    at
                    (steps !! at){stepConsumed = c'}
                    steps
    consumedOfHead = case steps of
        [] -> pure steps
        step : rest -> do
            c' <- genConsumedOf (stepConsumed step)
            pure (step{stepConsumed = c'} : rest)
    genConsumedOf c =
        oneof
            [ pure 0
            , pure (negate c)
            , elements [c - 1, c + 1, c + 2, c + treeKeyBits t]
            , choose (1, treeKeyBits t + 1)
            , elements [minBound, maxBound]
            ]
    stepsOfAnotherPrefix = do
        p <- genPrefix t
        pure $ case completenessOf t p of
            Just (CompletenessWitness _ other) -> other
            _ -> steps
damageSteps _ _ s = pure s

damageProof :: Damage
damageProof t o s@Statement{stPrefix, stProof} =
    oneof
        [ (\e -> s{stProof = CompletenessEmpty e}) <$> genWildExclusion t o
        , pure
            s
                { stProof =
                    maybe stProof CompletenessEmpty (exclusionOf t stPrefix)
                }
        , pure s{stLeaves = []}
        , do
            p <- genPrefix t
            pure s{stProof = fromMaybe stProof (completenessOf t p)}
        , pure s{stProof = CompletenessWitness [] []}
        ]

-- | A statement built from the honest one of a random prefix, with
-- none to three of the damages applied; most have one.
genDamagedStatement
    :: Tree -> Tree -> Gen (Statement, [String])
genDamagedStatement t o = do
    p <- genPrefix t
    let honest = honestOrBare t p
    n <-
        frequency
            [(1, pure 0), (7, pure 1), (3, pure 2), (1, pure 3)]
    applyDamages n honest
  where
    applyDamages n honest =
        foldM
            ( \(s, names) _ -> do
                (name, damage) <- elements damages
                s' <- damage t o s
                pure (s', name : names)
            )
            (honest, [])
            [1 .. n :: Int]

-- | A statement with up to five damages, to be given to the
-- verifiers without any expectation about the verdict.
genWildStatement :: Tree -> Tree -> Gen Statement
genWildStatement t o = do
    p <- genPrefix t
    let honest = honestOrBare t p
    n <- choose (1, 5 :: Int)
    foldM
        (\s _ -> elements damages >>= \(_, damage) -> damage t o s)
        honest
        [1 .. n]

-- | Smaller statements for a failing one: fewer leaves, fewer merge
-- operations and steps, smaller step sizes. The prefix is kept, so
-- a shrunk counterexample stays in the class it was generated for.
shrinkStatement :: Statement -> [Statement]
shrinkStatement s@Statement{stLeaves, stProof} =
    [s{stLeaves = ls} | ls <- shrinkList (const []) stLeaves]
        <> case stProof of
            CompletenessWitness ops steps ->
                [ s{stProof = CompletenessWitness ops' steps}
                | ops' <- shrinkList (const []) ops
                ]
                    <> [ s{stProof = CompletenessWitness ops steps'}
                       | steps' <- shrinkList shrinkStep steps
                       ]
            CompletenessEmpty _ -> []
  where
    shrinkStep step@ProofStep{stepConsumed} =
        [step{stepConsumed = c} | c <- shrinkIntegral stepConsumed]

-- | A statement with keys as bit strings.
describeStatement :: Statement -> String
describeStatement Statement{stPrefix, stLeaves, stProof} =
    "prefix "
        <> renderKey stPrefix
        <> " leaves "
        <> show [(renderKey jump, value) | Indirect{jump, value} <- stLeaves]
        <> " "
        <> case stProof of
            CompletenessWitness ops steps ->
                "merge operations "
                    <> show ops
                    <> " steps "
                    <> show
                        [ (stepConsumed, renderKey jump, value)
                        | ProofStep
                            { stepConsumed
                            , stepSibling = Indirect{jump, value}
                            } <-
                            steps
                        ]
            CompletenessEmpty e -> "empty, " <> describeExclusion e

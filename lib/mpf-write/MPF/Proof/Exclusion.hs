{-# LANGUAGE StrictData #-}

-- |
-- Module      : MPF.Proof.Exclusion
-- Description : MPF exclusion-proof generation and verification
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- Exclusion proofs for MPF keys. The populated proof variant reuses the
-- existing 'MPFProofStep' structure so the current Aiken proof-step codec
-- remains usable for exclusion mode too.
module MPF.Proof.Exclusion
    ( MPFExclusionProof (..)
    , mpfExclusionProofSteps
    , mkMPFExclusionProof
    , foldMPFExclusionProof
    , verifyMPFExclusionProof
    ) where

import Data.List (foldl', isPrefixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Database.KV.Transaction
    ( GCompare
    , Selector
    , Transaction
    , query
    )
import MPF.Hashes (MPFHashing (..))
import MPF.Interface
    ( FromHexKV (..)
    , HexDigit (..)
    , HexIndirect (..)
    , HexKey
    , compareHexKeys
    )
import MPF.Proof.Insertion (MPFProofStep (..))

-- | Exclusion proof for an MPF key.
data MPFExclusionProof a
    = MPFExclusionEmpty
        { mpeTargetKey :: HexKey
        }
    | MPFExclusionWitness
        { mpeTargetKey :: HexKey
        , mpeProofSteps :: [MPFProofStep a]
        }
    deriving (Show, Eq)

-- | Generate an exclusion proof for a key.
mkMPFExclusionProof
    :: (Monad m, GCompare d)
    => HexKey
    -> FromHexKV k v a
    -> MPFHashing a
    -> Selector d HexKey (HexIndirect a)
    -> k
    -> Transaction m cf d ops (Maybe (MPFExclusionProof a))
mkMPFExclusionProof prefix FromHexKV{fromHexK} hashing sel k = do
    let targetKey = fromHexK k
    mRoot <- query sel prefix
    case mRoot of
        Nothing ->
            pure
                $ Just
                    MPFExclusionEmpty
                        { mpeTargetKey = targetKey
                        }
        Just root@HexIndirect{hexJump = rootJump, hexIsLeaf}
            | hexIsLeaf -> do
                mStep <-
                    divergenceStep
                        hashing
                        sel
                        prefix
                        []
                        targetKey
                        root
                pure $ mkWitness targetKey . pure <$> mStep
            | not (rootJump `isPrefixOf` targetKey) -> do
                mStep <-
                    divergenceStep
                        hashing
                        sel
                        prefix
                        []
                        targetKey
                        root
                pure $ mkWitness targetKey . pure <$> mStep
            | otherwise ->
                case drop (length rootJump) targetKey of
                    [] -> pure Nothing
                    remaining -> do
                        mSteps <- go prefix [] rootJump remaining
                        pure $ mkWitness targetKey <$> mSteps
  where
    mkWitness targetKey steps =
        MPFExclusionWitness
            { mpeTargetKey = targetKey
            , mpeProofSteps = steps
            }

    go _ _ _ [] = pure Nothing
    go dbPath logicalPath branchJump (x : ks) = do
        let branchPath = dbPath <> branchJump
        sibDetails <- fetchSiblingDetails sel branchPath x
        mCurrentStep <-
            buildBranchStep
                hashing
                sel
                branchPath
                logicalPath
                branchJump
                x
                sibDetails
        case mCurrentStep of
            Nothing -> pure Nothing
            Just currentStep -> do
                mChild <- query sel (branchPath <> [x])
                case mChild of
                    Nothing ->
                        pure $ Just [currentStep]
                    Just child@HexIndirect{hexJump = childJump, hexIsLeaf}
                        | childJump `isPrefixOf` ks ->
                            case drop (length childJump) ks of
                                []
                                    | hexIsLeaf -> pure Nothing
                                    | otherwise -> pure Nothing
                                rest -> do
                                    mRest <-
                                        go
                                            (branchPath <> [x])
                                            (logicalPath <> branchJump <> [x])
                                            childJump
                                            rest
                                    pure $ fmap (++ [currentStep]) mRest
                        | otherwise -> do
                            let childPath = branchPath <> [x]
                                childLogicalPath =
                                    logicalPath <> branchJump <> [x]
                            mTerminal <-
                                divergenceStep
                                    hashing
                                    sel
                                    childPath
                                    childLogicalPath
                                    ks
                                    child
                            pure $ fmap (: [currentStep]) mTerminal

-- | View an exclusion proof as the shared Aiken / JS proof-step list.
--
-- Empty-tree exclusion uses the same transport shape as upstream:
-- an empty proof-step list verified in exclusion mode.
mpfExclusionProofSteps
    :: MPFExclusionProof a -> [MPFProofStep a]
mpfExclusionProofSteps MPFExclusionEmpty{} = []
mpfExclusionProofSteps MPFExclusionWitness{mpeProofSteps} =
    mpeProofSteps

-- | Fold an exclusion proof to the trusted root hash.
foldMPFExclusionProof
    :: MPFHashing a -> MPFExclusionProof a -> Maybe a
foldMPFExclusionProof _ MPFExclusionEmpty{} = Nothing
foldMPFExclusionProof
    hashing
    MPFExclusionWitness{mpeProofSteps} =
        foldl' (step hashing) Nothing mpeProofSteps

-- | Verify an exclusion proof against a trusted root.
verifyMPFExclusionProof
    :: Eq a
    => MPFHashing a
    -> Maybe a
    -> MPFExclusionProof a
    -> Bool
verifyMPFExclusionProof
    hashing
    trustedRoot
    proof@MPFExclusionEmpty{} =
        trustedRoot == foldMPFExclusionProof hashing proof
verifyMPFExclusionProof
    hashing
    trustedRoot
    proof@MPFExclusionWitness{mpeTargetKey, mpeProofSteps} =
        proofMatchesTarget mpeTargetKey mpeProofSteps
            && trustedRoot
                == foldMPFExclusionProof hashing proof

proofMatchesTarget
    :: HexKey -> [MPFProofStep a] -> Bool
proofMatchesTarget targetKey steps = go targetKey (reverse steps)
  where
    go _ [] = True
    go key (proofStep : rest) = case consumeStep key proofStep of
        Just key' -> go key' rest
        Nothing -> False

consumeStep
    :: HexKey -> MPFProofStep a -> Maybe HexKey
consumeStep key proofStep = do
    let (jump, position) = case proofStep of
            ProofStepBranch{psbJump, psbPosition} ->
                (psbJump, psbPosition)
            ProofStepLeaf{pslBranchJump, pslOurPosition} ->
                (pslBranchJump, pslOurPosition)
            ProofStepFork{psfBranchJump, psfOurPosition} ->
                (psfBranchJump, psfOurPosition)
    if jump `isPrefixOf` key
        then case drop (length jump) key of
            d : ds | d == position -> Just ds
            _ -> Nothing
        else Nothing

step :: MPFHashing a -> Maybe a -> MPFProofStep a -> Maybe a
step hashing acc proofStep = case proofStep of
    ProofStepBranch{psbJump, psbPosition, psbSiblingHashes} ->
        Just
            $ branchHash
                hashing
                psbJump
                ( merkleRoot
                    hashing
                    [ if HexDigit n == psbPosition
                        then acc
                        else
                            Map.lookup
                                (HexDigit n)
                                (Map.fromList psbSiblingHashes)
                    | n <- [0 .. 15]
                    ]
                )
    ProofStepLeaf
        { pslBranchJump
        , pslOurPosition
        , pslNeighborNibble
        , pslNeighborSuffix
        , pslNeighborValueDigest
        } ->
            let neighborHash =
                    leafHash
                        hashing
                        pslNeighborSuffix
                        pslNeighborValueDigest
            in  case acc of
                    Nothing ->
                        Just
                            $ leafHash
                                hashing
                                ( pslBranchJump
                                    <> [pslNeighborNibble]
                                    <> pslNeighborSuffix
                                )
                                pslNeighborValueDigest
                    Just subtreeHash ->
                        Just
                            $ branchHash
                                hashing
                                pslBranchJump
                                ( merkleRoot
                                    hashing
                                    [ if HexDigit n == pslOurPosition
                                        then Just subtreeHash
                                        else
                                            if HexDigit n
                                                == pslNeighborNibble
                                                then Just neighborHash
                                                else Nothing
                                    | n <- [0 .. 15]
                                    ]
                                )
    ProofStepFork
        { psfBranchJump
        , psfOurPosition
        , psfNeighborPrefix
        , psfNeighborIndex
        , psfMerkleRoot
        } ->
            let neighborHash =
                    branchHash
                        hashing
                        psfNeighborPrefix
                        psfMerkleRoot
            in  case acc of
                    Nothing ->
                        Just
                            $ branchHash
                                hashing
                                ( psfBranchJump
                                    <> [psfNeighborIndex]
                                    <> psfNeighborPrefix
                                )
                                psfMerkleRoot
                    Just subtreeHash ->
                        Just
                            $ branchHash
                                hashing
                                psfBranchJump
                                ( merkleRoot
                                    hashing
                                    [ if HexDigit n == psfOurPosition
                                        then Just subtreeHash
                                        else
                                            if HexDigit n
                                                == psfNeighborIndex
                                                then Just neighborHash
                                                else Nothing
                                    | n <- [0 .. 15]
                                    ]
                                )

buildBranchStep
    :: (Monad m, GCompare d)
    => MPFHashing a
    -> Selector d HexKey (HexIndirect a)
    -> HexKey
    -> HexKey
    -> HexKey
    -> HexDigit
    -> Map HexDigit (HexIndirect a)
    -> Transaction m cf d ops (Maybe (MPFProofStep a))
buildBranchStep
    hashing
    sel
    branchPath
    logicalPath
    branchJump
    x
    sibDetails =
        case Map.toList sibDetails of
            [] -> pure Nothing
            [ ( d
                    , HexIndirect
                        { hexJump = sibSuffix
                        , hexValue = sibVal
                        , hexIsLeaf = True
                        }
                    )
                ] ->
                    pure
                        $ Just
                            ProofStepLeaf
                                { pslBranchJump = branchJump
                                , pslOurPosition = x
                                , pslNeighborKeyPath =
                                    logicalPath
                                        <> branchJump
                                        <> [d]
                                        <> sibSuffix
                                , pslNeighborNibble = d
                                , pslNeighborSuffix = sibSuffix
                                , pslNeighborValueDigest = sibVal
                                }
            [ ( d
                    , HexIndirect
                        { hexJump = sibPrefix
                        , hexIsLeaf = False
                        }
                    )
                ] -> do
                    mr <-
                        fetchBranchMerkleRoot
                            hashing
                            sel
                            (branchPath <> [d])
                            sibPrefix
                    pure
                        $ Just
                            ProofStepFork
                                { psfBranchJump = branchJump
                                , psfOurPosition = x
                                , psfNeighborPrefix = sibPrefix
                                , psfNeighborIndex = d
                                , psfMerkleRoot = mr
                                }
            nonEmpty ->
                pure
                    $ Just
                        ProofStepBranch
                            { psbJump = branchJump
                            , psbPosition = x
                            , psbSiblingHashes =
                                [ (d, computeNodeHash hashing hi)
                                | (d, hi) <- nonEmpty
                                ]
                            }

divergenceStep
    :: (Monad m, GCompare d)
    => MPFHashing a
    -> Selector d HexKey (HexIndirect a)
    -> HexKey
    -> HexKey
    -> HexKey
    -> HexIndirect a
    -> Transaction m cf d ops (Maybe (MPFProofStep a))
divergenceStep
    hashing
    sel
    dbPath
    logicalPath
    targetKey
    HexIndirect
        { hexJump = witnessJump
        , hexValue = witnessValue
        , hexIsLeaf
        } =
        case splitDivergence targetKey witnessJump of
            Nothing -> pure Nothing
            Just (common, ourNibble, witnessNibble, witnessSuffix) ->
                if hexIsLeaf
                    then
                        pure
                            $ Just
                                ProofStepLeaf
                                    { pslBranchJump = common
                                    , pslOurPosition = ourNibble
                                    , pslNeighborKeyPath =
                                        logicalPath <> witnessJump
                                    , pslNeighborNibble = witnessNibble
                                    , pslNeighborSuffix = witnessSuffix
                                    , pslNeighborValueDigest =
                                        witnessValue
                                    }
                    else do
                        mr <-
                            fetchBranchMerkleRoot
                                hashing
                                sel
                                dbPath
                                witnessJump
                        pure
                            $ Just
                                ProofStepFork
                                    { psfBranchJump = common
                                    , psfOurPosition = ourNibble
                                    , psfNeighborPrefix = witnessSuffix
                                    , psfNeighborIndex = witnessNibble
                                    , psfMerkleRoot = mr
                                    }

splitDivergence
    :: HexKey
    -> HexKey
    -> Maybe (HexKey, HexDigit, HexDigit, HexKey)
splitDivergence targetKey witnessKey =
    case compareHexKeys targetKey witnessKey of
        (common, ourNibble : _, witnessNibble : witnessSuffix) ->
            Just
                ( common
                , ourNibble
                , witnessNibble
                , witnessSuffix
                )
        _ -> Nothing

fetchSiblingDetails
    :: (Monad m, GCompare d)
    => Selector d HexKey (HexIndirect a)
    -> HexKey
    -> HexDigit
    -> Transaction m cf d ops (Map HexDigit (HexIndirect a))
fetchSiblingDetails sel pfx exclude = do
    pairs <- mapM fetchOne digits
    pure
        $ Map.fromList
            [(d, hi) | (d, Just hi) <- pairs]
  where
    digits =
        [ HexDigit n
        | n <- [0 .. 15]
        , HexDigit n /= exclude
        ]

    fetchOne d = do
        mi <- query sel (pfx <> [d])
        pure (d, mi)

fetchBranchMerkleRoot
    :: (Monad m, GCompare d)
    => MPFHashing a
    -> Selector d HexKey (HexIndirect a)
    -> HexKey
    -> HexKey
    -> Transaction m cf d ops a
fetchBranchMerkleRoot
    hashing'@MPFHashing{merkleRoot}
    sel
    nodePath
    nodeJump = do
        children <- mapM fetchChild [HexDigit n | n <- [0 .. 15]]
        pure $ merkleRoot children
      where
        fetchChild d = do
            mi <- query sel (nodePath <> nodeJump <> [d])
            pure $ fmap (computeNodeHash hashing') mi

computeNodeHash :: MPFHashing a -> HexIndirect a -> a
computeNodeHash
    MPFHashing{leafHash}
    HexIndirect{hexJump, hexValue, hexIsLeaf} =
        if hexIsLeaf
            then leafHash hexJump hexValue
            else hexValue

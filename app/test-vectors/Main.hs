-- | Generate Aiken test vectors for the CSMT library.
--
-- Builds trees using the pure in-memory backend and outputs
-- proofs in Aiken-compatible format.
module Main (main) where

import CSMT.Backend.Pure
    ( InMemoryDB
    , Pure
    , emptyInMemoryDB
    , pureDatabase
    , runPure
    )
import CSMT.Backend.Standalone
    ( Standalone (..)
    , StandaloneCF
    , StandaloneCodecs (..)
    , StandaloneOp
    )
import CSMT.Hashes
    ( Hash
    , fromKVHashes
    , hashHashing
    , isoHash
    , renderHash
    )
import CSMT.Insertion (inserting)
import CSMT.Interface
    ( Direction (..)
    , Indirect (..)
    )
import CSMT.Interface qualified as I
import CSMT.Proof.Insertion
    ( InclusionProof (..)
    , ProofStep (..)
    , buildInclusionProof
    )
import Control.Lens (simple)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.List (intercalate)
import Database.KV.Transaction (Transaction, runTransactionUnguarded)
import Numeric (showHex)

-- | Codecs for ByteString keys and values with Hash nodes.
bsCodecs :: StandaloneCodecs ByteString ByteString Hash
bsCodecs =
    StandaloneCodecs
        { keyCodec = simple
        , valueCodec = simple
        , nodeCodec = isoHash
        }

-- | Run a transaction in the pure backend.
runTx
    :: Transaction
        Pure
        StandaloneCF
        (Standalone ByteString ByteString Hash)
        StandaloneOp
        a
    -> Pure a
runTx = runTransactionUnguarded (pureDatabase bsCodecs)

-- | Insert a ByteString key-value pair.
insertBS :: ByteString -> ByteString -> Pure ()
insertBS k v =
    runTx
        $ inserting
            fromKVHashes
            hashHashing
            StandaloneKVCol
            StandaloneCSMTCol
            k
            v

-- | Generate an inclusion proof for a ByteString key.
proofBS :: ByteString -> Pure (Maybe (ByteString, InclusionProof Hash))
proofBS k =
    runTx
        $ buildInclusionProof
            fromKVHashes
            StandaloneKVCol
            StandaloneCSMTCol
            hashHashing
            k

-- | Get the root hash.
rootHashBS :: Pure (Maybe Hash)
rootHashBS = runTx $ I.root hashHashing StandaloneCSMTCol

-- | Format helpers

toHex :: ByteString -> String
toHex bs =
    "#\""
        ++ concatMap (\w -> pad2 (showHex w "")) (B.unpack bs)
        ++ "\""
  where
    pad2 [c] = ['0', c]
    pad2 s = s

toBitpath :: [Direction] -> String
toBitpath [] = "#\"\""
toBitpath ds = "#[" ++ intercalate ", " (map d ds) ++ "]"
  where
    d L = "0"
    d R = "1"

fmtIndirect :: Indirect Hash -> String
fmtIndirect (Indirect jmp val) =
    "Indirect { jump: "
        ++ toBitpath jmp
        ++ ", hash: "
        ++ toHex (renderHash val)
        ++ " }"

fmtStep :: ProofStep Hash -> String
fmtStep (ProofStep consumed sibling) =
    "ProofStep { consumed: "
        ++ show consumed
        ++ ", sibling: "
        ++ fmtIndirect sibling
        ++ " }"

fmtProof :: InclusionProof Hash -> [String]
fmtProof p =
    [ "  let proof = Proof {"
    , "    root_jump: " ++ toBitpath (proofRootJump p) ++ ","
    ]
        ++ stepsLines
        ++ ["  }"]
  where
    stepsLines = case proofSteps p of
        [] -> ["    steps: [],"]
        steps ->
            ["    steps: ["]
                ++ ["      " ++ fmtStep s ++ "," | s <- steps]
                ++ ["    ],"]

-- | Build a tree from key-value pairs.
buildTree :: [(ByteString, ByteString)] -> InMemoryDB
buildTree kvs =
    snd $ runPure emptyInMemoryDB $ mapM_ (uncurry insertBS) kvs

-- | Get proof and root from a db.
getProofAndRoot
    :: InMemoryDB
    -> ByteString
    -> (Maybe (InclusionProof Hash), Maybe Hash)
getProofAndRoot db k =
    let ((mp, r), _) = runPure db $ do
            p <- proofBS k
            rh <- rootHashBS
            pure (p, rh)
    in  (fmap snd mp, r)

-- | Print a test for `has`.
printHas
    :: String
    -> ByteString
    -> ByteString
    -> Hash
    -> InclusionProof Hash
    -> IO ()
printHas name key val r p = do
    putStrLn $ "test " ++ name ++ "() {"
    putStrLn $ "  let trie = from_root(" ++ toHex (renderHash r) ++ ")"
    mapM_ putStrLn (fmtProof p)
    putStrLn
        $ "  has(trie, "
            ++ toHex key
            ++ ", "
            ++ toHex val
            ++ ", proof)"
    putStrLn "}"
    putStrLn ""

main :: IO ()
main = do
    putStrLn
        "// Auto-generated CSMT test vectors from haskell-mts"
    putStrLn
        "// Tests the Aiken CSMT against the reference Haskell."
    putStrLn ""
    putStrLn
        "use aiken/csmt.{Proof, ProofStep, empty, from_root,"
    putStrLn
        "  has, insert, delete, update, root, is_empty}"
    putStrLn "use aiken/csmt/hashing.{Indirect}"
    putStrLn ""

    -- 1. Single element: has
    do
        let db = buildTree [("\xab", "\xcd")]
            (Just p, Just r) = getProofAndRoot db "\xab"
        printHas "vec_has_single" "\xab" "\xcd" r p

    -- 2. Single element: insert into empty
    do
        let db = buildTree [("\xab", "\xcd")]
            (Just p, Just r) = getProofAndRoot db "\xab"
        putStrLn "test vec_insert_into_empty() {"
        mapM_ putStrLn (fmtProof p)
        putStrLn
            $ "  let trie = insert(empty, "
                ++ toHex "\xab"
                ++ ", "
                ++ toHex "\xcd"
                ++ ", proof)"
        putStrLn $ "  root(trie) == " ++ toHex (renderHash r)
        putStrLn "}"
        putStrLn ""

    -- 3. Two elements diverging at bit 0
    do
        let db = buildTree [("\x00", "\xaa"), ("\x80", "\xbb")]
            (Just p0, Just r) = getProofAndRoot db "\x00"
            (Just p1, _) = getProofAndRoot db "\x80"
        printHas "vec_has_two_left" "\x00" "\xaa" r p0
        printHas "vec_has_two_right" "\x80" "\xbb" r p1

    -- 4. Two elements with shared prefix
    do
        let db =
                buildTree
                    [("\xab\x00", "\x11"), ("\xab\x80", "\x22")]
            (Just p0, Just r) = getProofAndRoot db "\xab\x00"
            (Just p1, _) = getProofAndRoot db "\xab\x80"
        printHas "vec_has_shared_prefix_left" "\xab\x00" "\x11" r p0
        printHas
            "vec_has_shared_prefix_right"
            "\xab\x80"
            "\x22"
            r
            p1

    -- 5. Three elements
    do
        let db =
                buildTree
                    [ ("\x00", "\x01")
                    , ("\x40", "\x02")
                    , ("\x80", "\x03")
                    ]
            (Just pa, Just r) = getProofAndRoot db "\x00"
            (Just pb, _) = getProofAndRoot db "\x40"
            (Just pc, _) = getProofAndRoot db "\x80"
        printHas "vec_has_three_a" "\x00" "\x01" r pa
        printHas "vec_has_three_b" "\x40" "\x02" r pb
        printHas "vec_has_three_c" "\x80" "\x03" r pc

    -- 6. Four elements (full binary 2 levels)
    do
        let db =
                buildTree
                    [ ("\x00", "\x10")
                    , ("\x40", "\x20")
                    , ("\x80", "\x30")
                    , ("\xc0", "\x40")
                    ]
            (Just pa, Just r) = getProofAndRoot db "\x00"
            (Just pd, _) = getProofAndRoot db "\xc0"
        printHas "vec_has_four_first" "\x00" "\x10" r pa
        printHas "vec_has_four_last" "\xc0" "\x40" r pd

    -- 7. Insert sequence: empty -> one -> two
    do
        let db1 = buildTree [("\x10", "\xaa")]
            (Just p1, Just r1) = getProofAndRoot db1 "\x10"
        putStrLn "test vec_insert_first() {"
        mapM_ putStrLn (fmtProof p1)
        putStrLn
            $ "  let trie = insert(empty, "
                ++ toHex "\x10"
                ++ ", "
                ++ toHex "\xaa"
                ++ ", proof)"
        putStrLn $ "  root(trie) == " ++ toHex (renderHash r1)
        putStrLn "}"
        putStrLn ""

        let db2 =
                buildTree [("\x10", "\xaa"), ("\x20", "\xbb")]
            (Just p2, Just r2) = getProofAndRoot db2 "\x20"
        putStrLn "test vec_insert_second() {"
        mapM_ putStrLn (fmtProof p2)
        putStrLn
            $ "  let old_trie = from_root("
                ++ toHex (renderHash r1)
                ++ ")"
        putStrLn
            $ "  let trie = insert(old_trie, "
                ++ toHex "\x20"
                ++ ", "
                ++ toHex "\xbb"
                ++ ", proof)"
        putStrLn $ "  root(trie) == " ++ toHex (renderHash r2)
        putStrLn "}"
        putStrLn ""

    -- 8. Delete: two elements, delete one
    do
        let db2 =
                buildTree
                    [("\x50", "\xdd"), ("\xa0", "\xee")]
            (Just pDel, Just r2) = getProofAndRoot db2 "\xa0"
            db1 = buildTree [("\x50", "\xdd")]
            (_, Just r1) = getProofAndRoot db1 "\x50"
        putStrLn "test vec_delete_element() {"
        putStrLn
            $ "  let trie = from_root("
                ++ toHex (renderHash r2)
                ++ ")"
        mapM_ putStrLn (fmtProof pDel)
        putStrLn
            $ "  let trie2 = delete(trie, "
                ++ toHex "\xa0"
                ++ ", "
                ++ toHex "\xee"
                ++ ", proof)"
        putStrLn $ "  root(trie2) == " ++ toHex (renderHash r1)
        putStrLn "}"
        putStrLn ""

    -- 9. Delete last element (back to empty)
    do
        let db = buildTree [("\x42", "\xff")]
            (Just p, Just r) = getProofAndRoot db "\x42"
        putStrLn "test vec_delete_to_empty() {"
        putStrLn
            $ "  let trie = from_root("
                ++ toHex (renderHash r)
                ++ ")"
        mapM_ putStrLn (fmtProof p)
        putStrLn
            $ "  let trie2 = delete(trie, "
                ++ toHex "\x42"
                ++ ", "
                ++ toHex "\xff"
                ++ ", proof)"
        putStrLn "  is_empty(trie2)"
        putStrLn "}"
        putStrLn ""

    -- 10. Update value
    do
        let db = buildTree [("\x42", "\x01\x02")]
            (Just p, Just rOld) = getProofAndRoot db "\x42"
            dbNew = buildTree [("\x42", "\x03\x04")]
            (_, Just rNew) = getProofAndRoot dbNew "\x42"
        putStrLn "test vec_update_value() {"
        putStrLn
            $ "  let trie = from_root("
                ++ toHex (renderHash rOld)
                ++ ")"
        mapM_ putStrLn (fmtProof p)
        putStrLn
            $ "  let trie2 = update(trie, "
                ++ toHex "\x42"
                ++ ", proof, "
                ++ toHex "\x01\x02"
                ++ ", "
                ++ toHex "\x03\x04"
                ++ ")"
        putStrLn $ "  root(trie2) == " ++ toHex (renderHash rNew)
        putStrLn "}"
        putStrLn ""

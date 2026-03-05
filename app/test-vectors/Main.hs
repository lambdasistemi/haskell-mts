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
import CSMT.Fifo (counterToKey)
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
            []
            fromKVHashes
            hashHashing
            StandaloneKVCol
            StandaloneCSMTCol
            k
            v

-- | Generate an inclusion proof for a ByteString key.
proofBS
    :: ByteString -> Pure (Maybe (ByteString, InclusionProof Hash))
proofBS k =
    runTx
        $ buildInclusionProof
            []
            fromKVHashes
            StandaloneKVCol
            StandaloneCSMTCol
            hashHashing
            k

-- | Get the root hash.
rootHashBS :: Pure (Maybe Hash)
rootHashBS = runTx $ I.root hashHashing StandaloneCSMTCol []

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
fmtProof = fmtProofAs "proof"

-- | Format a proof with a custom variable name.
fmtProofAs :: String -> InclusionProof Hash -> [String]
fmtProofAs name p =
    [ "  let " ++ name ++ " = Proof {"
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
        "  has, insert, delete, update, pop, pop_max, push_min, push_max, root, is_empty}"
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

    -- ---------------------------------------------------------------
    -- Pop (minimum extraction) test vectors
    -- ---------------------------------------------------------------

    -- 11. Pop: single element (pop to empty)
    do
        let db = buildTree [("\x00", "\xaa")]
            (Just p, Just r) = getProofAndRoot db "\x00"
        putStrLn "test vec_pop_single() {"
        putStrLn
            $ "  let trie = from_root("
                ++ toHex (renderHash r)
                ++ ")"
        mapM_ putStrLn (fmtProof p)
        putStrLn
            $ "  let trie2 = pop(trie, "
                ++ toHex "\x00"
                ++ ", "
                ++ toHex "\xaa"
                ++ ", proof)"
        putStrLn "  is_empty(trie2)"
        putStrLn "}"
        putStrLn ""

    -- 12. Pop: minimum from three elements
    do
        let db =
                buildTree
                    [ ("\x00", "\x01")
                    , ("\x40", "\x02")
                    , ("\x80", "\x03")
                    ]
            (Just pMin, Just r3) = getProofAndRoot db "\x00"
            db2 = buildTree [("\x40", "\x02"), ("\x80", "\x03")]
            (_, Just r2) = getProofAndRoot db2 "\x40"
        putStrLn "test vec_pop_minimum_of_three() {"
        putStrLn
            $ "  let trie = from_root("
                ++ toHex (renderHash r3)
                ++ ")"
        mapM_ putStrLn (fmtProof pMin)
        putStrLn
            $ "  let trie2 = pop(trie, "
                ++ toHex "\x00"
                ++ ", "
                ++ toHex "\x01"
                ++ ", proof)"
        putStrLn $ "  root(trie2) == " ++ toHex (renderHash r2)
        putStrLn "}"
        putStrLn ""

    -- 13. Pop: minimum from four elements
    do
        let db =
                buildTree
                    [ ("\x00", "\x10")
                    , ("\x40", "\x20")
                    , ("\x80", "\x30")
                    , ("\xc0", "\x40")
                    ]
            (Just pMin, Just r4) = getProofAndRoot db "\x00"
            db3 =
                buildTree
                    [ ("\x40", "\x20")
                    , ("\x80", "\x30")
                    , ("\xc0", "\x40")
                    ]
            (_, Just r3) = getProofAndRoot db3 "\x40"
        putStrLn "test vec_pop_minimum_of_four() {"
        putStrLn
            $ "  let trie = from_root("
                ++ toHex (renderHash r4)
                ++ ")"
        mapM_ putStrLn (fmtProof pMin)
        putStrLn
            $ "  let trie2 = pop(trie, "
                ++ toHex "\x00"
                ++ ", "
                ++ toHex "\x10"
                ++ ", proof)"
        putStrLn $ "  root(trie2) == " ++ toHex (renderHash r3)
        putStrLn "}"
        putStrLn ""

    -- ---------------------------------------------------------------
    -- Pop max vectors
    -- ---------------------------------------------------------------

    -- pop_max: single element (pop max to empty)
    do
        let db = buildTree [("\xff", "\xaa")]
            (Just p, Just r) = getProofAndRoot db "\xff"
        putStrLn "test vec_pop_max_single() {"
        putStrLn
            $ "  let trie = from_root("
                ++ toHex (renderHash r)
                ++ ")"
        mapM_ putStrLn (fmtProof p)
        putStrLn
            $ "  let trie2 = pop_max(trie, "
                ++ toHex "\xff"
                ++ ", "
                ++ toHex "\xaa"
                ++ ", proof)"
        putStrLn "  is_empty(trie2)"
        putStrLn "}"
        putStrLn ""

    -- pop_max: maximum from three elements
    do
        let db =
                buildTree
                    [ ("\x00", "\x01")
                    , ("\x40", "\x02")
                    , ("\x80", "\x03")
                    ]
            (Just pMax, Just r3) = getProofAndRoot db "\x80"
            db2 = buildTree [("\x00", "\x01"), ("\x40", "\x02")]
            (_, Just r2) = getProofAndRoot db2 "\x00"
        putStrLn "test vec_pop_max_of_three() {"
        putStrLn
            $ "  let trie = from_root("
                ++ toHex (renderHash r3)
                ++ ")"
        mapM_ putStrLn (fmtProof pMax)
        putStrLn
            $ "  let trie2 = pop_max(trie, "
                ++ toHex "\x80"
                ++ ", "
                ++ toHex "\x03"
                ++ ", proof)"
        putStrLn $ "  root(trie2) == " ++ toHex (renderHash r2)
        putStrLn "}"
        putStrLn ""

    -- pop_max: maximum from four elements
    do
        let db =
                buildTree
                    [ ("\x00", "\x10")
                    , ("\x40", "\x20")
                    , ("\x80", "\x30")
                    , ("\xc0", "\x40")
                    ]
            (Just pMax, Just r4) = getProofAndRoot db "\xc0"
            db3 =
                buildTree
                    [ ("\x00", "\x10")
                    , ("\x40", "\x20")
                    , ("\x80", "\x30")
                    ]
            (_, Just r3) = getProofAndRoot db3 "\x00"
        putStrLn "test vec_pop_max_of_four() {"
        putStrLn
            $ "  let trie = from_root("
                ++ toHex (renderHash r4)
                ++ ")"
        mapM_ putStrLn (fmtProof pMax)
        putStrLn
            $ "  let trie2 = pop_max(trie, "
                ++ toHex "\xc0"
                ++ ", "
                ++ toHex "\x40"
                ++ ", proof)"
        putStrLn $ "  root(trie2) == " ++ toHex (renderHash r3)
        putStrLn "}"
        putStrLn ""

    -- ---------------------------------------------------------------
    -- Push min vectors
    -- ---------------------------------------------------------------

    -- push_min: insert min into empty
    do
        let db = buildTree [("\x00", "\xaa")]
            (Just p, Just r) = getProofAndRoot db "\x00"
        putStrLn "test vec_push_min_single() {"
        mapM_ putStrLn (fmtProof p)
        putStrLn
            $ "  let trie = push_min(empty, "
                ++ toHex "\x00"
                ++ ", "
                ++ toHex "\xaa"
                ++ ", proof)"
        putStrLn $ "  root(trie) == " ++ toHex (renderHash r)
        putStrLn "}"
        putStrLn ""

    -- push_min: push min into {0x40, 0x80}
    do
        let db3 =
                buildTree
                    [ ("\x00", "\x01")
                    , ("\x40", "\x02")
                    , ("\x80", "\x03")
                    ]
            (Just p, Just r3) = getProofAndRoot db3 "\x00"
            db2 = buildTree [("\x40", "\x02"), ("\x80", "\x03")]
            (_, Just r2) = getProofAndRoot db2 "\x40"
        putStrLn "test vec_push_min_of_three() {"
        putStrLn
            $ "  let trie = from_root("
                ++ toHex (renderHash r2)
                ++ ")"
        mapM_ putStrLn (fmtProof p)
        putStrLn
            $ "  let trie2 = push_min(trie, "
                ++ toHex "\x00"
                ++ ", "
                ++ toHex "\x01"
                ++ ", proof)"
        putStrLn $ "  root(trie2) == " ++ toHex (renderHash r3)
        putStrLn "}"
        putStrLn ""

    -- push_min: push min into {0x40, 0x80, 0xc0}
    do
        let db4 =
                buildTree
                    [ ("\x00", "\x10")
                    , ("\x40", "\x20")
                    , ("\x80", "\x30")
                    , ("\xc0", "\x40")
                    ]
            (Just p, Just r4) = getProofAndRoot db4 "\x00"
            db3 =
                buildTree
                    [ ("\x40", "\x20")
                    , ("\x80", "\x30")
                    , ("\xc0", "\x40")
                    ]
            (_, Just r3) = getProofAndRoot db3 "\x40"
        putStrLn "test vec_push_min_of_four() {"
        putStrLn
            $ "  let trie = from_root("
                ++ toHex (renderHash r3)
                ++ ")"
        mapM_ putStrLn (fmtProof p)
        putStrLn
            $ "  let trie2 = push_min(trie, "
                ++ toHex "\x00"
                ++ ", "
                ++ toHex "\x10"
                ++ ", proof)"
        putStrLn $ "  root(trie2) == " ++ toHex (renderHash r4)
        putStrLn "}"
        putStrLn ""

    -- ---------------------------------------------------------------
    -- Push max vectors
    -- ---------------------------------------------------------------

    -- push_max: insert max into empty
    do
        let db = buildTree [("\xff", "\xaa")]
            (Just p, Just r) = getProofAndRoot db "\xff"
        putStrLn "test vec_push_max_single() {"
        mapM_ putStrLn (fmtProof p)
        putStrLn
            $ "  let trie = push_max(empty, "
                ++ toHex "\xff"
                ++ ", "
                ++ toHex "\xaa"
                ++ ", proof)"
        putStrLn $ "  root(trie) == " ++ toHex (renderHash r)
        putStrLn "}"
        putStrLn ""

    -- push_max: push max into {0x00, 0x40}
    do
        let db3 =
                buildTree
                    [ ("\x00", "\x01")
                    , ("\x40", "\x02")
                    , ("\x80", "\x03")
                    ]
            (Just p, Just r3) = getProofAndRoot db3 "\x80"
            db2 = buildTree [("\x00", "\x01"), ("\x40", "\x02")]
            (_, Just r2) = getProofAndRoot db2 "\x00"
        putStrLn "test vec_push_max_of_three() {"
        putStrLn
            $ "  let trie = from_root("
                ++ toHex (renderHash r2)
                ++ ")"
        mapM_ putStrLn (fmtProof p)
        putStrLn
            $ "  let trie2 = push_max(trie, "
                ++ toHex "\x80"
                ++ ", "
                ++ toHex "\x03"
                ++ ", proof)"
        putStrLn $ "  root(trie2) == " ++ toHex (renderHash r3)
        putStrLn "}"
        putStrLn ""

    -- push_max: push max into {0x00, 0x40, 0x80}
    do
        let db4 =
                buildTree
                    [ ("\x00", "\x10")
                    , ("\x40", "\x20")
                    , ("\x80", "\x30")
                    , ("\xc0", "\x40")
                    ]
            (Just p, Just r4) = getProofAndRoot db4 "\xc0"
            db3 =
                buildTree
                    [ ("\x00", "\x10")
                    , ("\x40", "\x20")
                    , ("\x80", "\x30")
                    ]
            (_, Just r3) = getProofAndRoot db3 "\x00"
        putStrLn "test vec_push_max_of_four() {"
        putStrLn
            $ "  let trie = from_root("
                ++ toHex (renderHash r3)
                ++ ")"
        mapM_ putStrLn (fmtProof p)
        putStrLn
            $ "  let trie2 = push_max(trie, "
                ++ toHex "\xc0"
                ++ ", "
                ++ toHex "\x40"
                ++ ", proof)"
        putStrLn $ "  root(trie2) == " ++ toHex (renderHash r4)
        putStrLn "}"
        putStrLn ""

    -- ---------------------------------------------------------------
    -- FIFO test vectors (indexBytes = 1)
    -- ---------------------------------------------------------------

    putStrLn "// FIFO test vectors"
    putStrLn ""
    putStrLn "use aiken/csmt/fifo.{Fifo, push, pop, root_hash}"
    putStrLn ""

    let ixB = 1 :: Int
        fifoKey = counterToKey ixB
        vA = "\xaa" :: ByteString
        vB = "\xbb" :: ByteString
        vC = "\xcc" :: ByteString

    -- Build intermediate DBs: after 1 push, 2 pushes, 3 pushes
    let db1 = buildTree [(fifoKey 0, vA)]
        db2 = buildTree [(fifoKey 0, vA), (fifoKey 1, vB)]
        db3 =
            buildTree
                [(fifoKey 0, vA), (fifoKey 1, vB), (fifoKey 2, vC)]

    -- 11. FIFO: push one element
    do
        let (Just p, Just r) = getProofAndRoot db1 (fifoKey 0)
        putStrLn "test vec_fifo_push_one() {"
        putStrLn "  let fifo = Fifo { trie: empty, head: 0, tail: 0 }"
        mapM_ putStrLn (fmtProof p)
        putStrLn
            $ "  let fifo2 = push(fifo, "
                ++ show ixB
                ++ ", "
                ++ toHex vA
                ++ ", proof)"
        putStrLn
            $ "  root_hash(fifo2) == "
                ++ toHex (renderHash r)
        putStrLn "}"
        putStrLn ""

    -- 12. FIFO: push second element
    do
        let (Just p, Just r2) = getProofAndRoot db2 (fifoKey 1)
            (_, Just r1) = getProofAndRoot db1 (fifoKey 0)
        putStrLn "test vec_fifo_push_two() {"
        putStrLn
            $ "  let fifo = Fifo { trie: from_root("
                ++ toHex (renderHash r1)
                ++ "), head: 0, tail: 1 }"
        mapM_ putStrLn (fmtProof p)
        putStrLn
            $ "  let fifo2 = push(fifo, "
                ++ show ixB
                ++ ", "
                ++ toHex vB
                ++ ", proof)"
        putStrLn
            $ "  root_hash(fifo2) == "
                ++ toHex (renderHash r2)
        putStrLn "}"
        putStrLn ""

    -- 13. FIFO: push third element
    do
        let (Just p, Just r3) = getProofAndRoot db3 (fifoKey 2)
            (_, Just r2) = getProofAndRoot db2 (fifoKey 1)
        putStrLn "test vec_fifo_push_three() {"
        putStrLn
            $ "  let fifo = Fifo { trie: from_root("
                ++ toHex (renderHash r2)
                ++ "), head: 0, tail: 2 }"
        mapM_ putStrLn (fmtProof p)
        putStrLn
            $ "  let fifo2 = push(fifo, "
                ++ show ixB
                ++ ", "
                ++ toHex vC
                ++ ", proof)"
        putStrLn
            $ "  root_hash(fifo2) == "
                ++ toHex (renderHash r3)
        putStrLn "}"
        putStrLn ""

    -- 14. FIFO: pop first element from 3-element queue
    do
        let (Just p0, Just r3) = getProofAndRoot db3 (fifoKey 0)
            (_, Just r2after) = getProofAndRoot db2after (fifoKey 1)
            db2after =
                buildTree [(fifoKey 1, vB), (fifoKey 2, vC)]
        putStrLn "test vec_fifo_pop_one() {"
        putStrLn
            $ "  let fifo = Fifo { trie: from_root("
                ++ toHex (renderHash r3)
                ++ "), head: 0, tail: 3 }"
        mapM_ putStrLn (fmtProof p0)
        putStrLn
            $ "  let fifo2 = pop(fifo, "
                ++ show ixB
                ++ ", "
                ++ toHex vA
                ++ ", proof)"
        putStrLn
            $ "  root_hash(fifo2) == "
                ++ toHex (renderHash r2after)
        putStrLn "}"
        putStrLn ""

    -- 15. FIFO: pop all three (sequential)
    do
        let (Just p0, Just r3) = getProofAndRoot db3 (fifoKey 0)
            db_12 = buildTree [(fifoKey 1, vB), (fifoKey 2, vC)]
            (Just p1, Just r12) = getProofAndRoot db_12 (fifoKey 1)
            db_2 = buildTree [(fifoKey 2, vC)]
            (Just p2, Just _r2only) = getProofAndRoot db_2 (fifoKey 2)

        putStrLn "test vec_fifo_pop_all() {"
        -- Pop element 0
        putStrLn
            $ "  let fifo = Fifo { trie: from_root("
                ++ toHex (renderHash r3)
                ++ "), head: 0, tail: 3 }"
        mapM_ putStrLn (fmtProof p0)
        putStrLn
            $ "  let fifo2 = pop(fifo, "
                ++ show ixB
                ++ ", "
                ++ toHex vA
                ++ ", proof)"
        -- Pop element 1
        mapM_ putStrLn (fmtProofAs "proof2" p1)
        putStrLn
            $ "  let fifo3 = pop(fifo2, "
                ++ show ixB
                ++ ", "
                ++ toHex vB
                ++ ", proof2)"
        -- Pop element 2
        mapM_ putStrLn (fmtProofAs "proof3" p2)
        putStrLn
            $ "  let fifo4 = pop(fifo3, "
                ++ show ixB
                ++ ", "
                ++ toHex vC
                ++ ", proof3)"
        putStrLn "  is_empty(fifo4)"
        putStrLn "}"
        putStrLn ""

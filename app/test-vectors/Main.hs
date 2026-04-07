-- | Generate Aiken test vectors for the CSMT library.
--
-- Builds trees using the pure in-memory backend and outputs
-- proofs in Aiken-compatible format via the aiken-codegen DSL.
module Main (main) where

import Aiken.Codegen
    ( BodyM
    , Def (Blank)
    , Expr
    , ModuleM
    , bind
    , call
    , comment
    , emit
    , emitTest
    , field
    , hex
    , int
    , item
    , list
    , record
    , renderModule
    , runModule
    , useFrom
    , var
    , (.==)
    )
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
import Database.KV.Transaction
    ( Transaction
    , runTransactionUnguarded
    )

-- -----------------------------------------------------------
-- MPF backend helpers
-- -----------------------------------------------------------

-- | Codecs for ByteString keys and values with
-- Hash nodes.
bsCodecs
    :: StandaloneCodecs ByteString ByteString Hash
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

-- | Generate an inclusion proof for a ByteString
-- key.
proofBS
    :: ByteString
    -> Pure (Maybe (ByteString, InclusionProof Hash))
proofBS k =
    runTx
        $ buildInclusionProof
            []
            fromKVHashes
            StandaloneKVCol
            StandaloneCSMTCol
            k

-- | Get the root hash.
rootHashBS :: Pure (Maybe Hash)
rootHashBS =
    runTx $ I.root hashHashing StandaloneCSMTCol []

-- | Build a tree from key-value pairs.
buildTree :: [(ByteString, ByteString)] -> InMemoryDB
buildTree kvs =
    snd
        $ runPure emptyInMemoryDB
        $ mapM_ (uncurry insertBS) kvs

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

-- -----------------------------------------------------------
-- Domain → Expr conversions
-- -----------------------------------------------------------

-- | Convert a direction bitpath to an Aiken hex
-- literal. Each direction becomes a byte (0 or 1).
bitpath :: [Direction] -> Expr
bitpath ds = hex $ B.pack $ map dirByte ds
  where
    dirByte L = 0
    dirByte R = 1

-- | Convert an Indirect Hash to an Aiken record
-- expression.
indirectExpr :: Indirect Hash -> Expr
indirectExpr (Indirect jmp val) =
    record "Indirect" $ do
        field "jump" $ bitpath jmp
        field "hash" $ hex $ renderHash val

-- | Convert a ProofStep Hash to an Aiken record
-- expression.
stepExpr :: ProofStep Hash -> Expr
stepExpr (ProofStep consumed sibling) =
    record "ProofStep" $ do
        field "consumed"
            $ int
            $ fromIntegral consumed
        field "sibling" $ indirectExpr sibling

-- | Convert an InclusionProof to an Aiken Proof
-- record expression.
proofExpr :: InclusionProof Hash -> Expr
proofExpr p =
    record "Proof" $ do
        field "root_jump" $ bitpath (proofRootJump p)
        field "steps"
            $ list
            $ mapM_ (item . stepExpr) (proofSteps p)

-- | Bind a proof variable in a BodyM.
bindProof
    :: String -> InclusionProof Hash -> BodyM Expr
bindProof name p = bind name (proofExpr p)

-- | Hash expression for root comparison.
rootExpr :: Hash -> Expr
rootExpr = hex . renderHash

-- -----------------------------------------------------------
-- Test vector builders
-- -----------------------------------------------------------

-- | Emit a 'has' test.
emitHas
    :: String
    -> ByteString
    -> ByteString
    -> Hash
    -> InclusionProof Hash
    -> ModuleM ()
emitHas name key val r p =
    emitTest name $ do
        trie <- bind "trie" $ call "from_root" [rootExpr r]
        proof <- bindProof "proof" p
        pure $ call "has" [trie, hex key, hex val, proof]

-- | Emit an 'insert into empty' test.
emitInsertEmpty
    :: String
    -> ByteString
    -> ByteString
    -> Hash
    -> InclusionProof Hash
    -> ModuleM ()
emitInsertEmpty name key val r p =
    emitTest name $ do
        proof <- bindProof "proof" p
        trie <-
            bind "trie"
                $ call
                    "insert"
                    [var "empty", hex key, hex val, proof]
        pure $ call "root" [trie] .== rootExpr r

-- | Emit an insert test (from existing root).
emitInsert
    :: String
    -> Hash
    -> ByteString
    -> ByteString
    -> Hash
    -> InclusionProof Hash
    -> ModuleM ()
emitInsert name oldRoot key val newRoot p =
    emitTest name $ do
        proof <- bindProof "proof" p
        oldTrie <-
            bind "old_trie"
                $ call "from_root" [rootExpr oldRoot]
        trie <-
            bind "trie"
                $ call
                    "insert"
                    [oldTrie, hex key, hex val, proof]
        pure $ call "root" [trie] .== rootExpr newRoot

-- | Emit a delete test.
emitDelete
    :: String
    -> Hash
    -> ByteString
    -> ByteString
    -> Hash
    -> InclusionProof Hash
    -> ModuleM ()
emitDelete name oldRoot key val newRoot p =
    emitTest name $ do
        trie <-
            bind "trie"
                $ call "from_root" [rootExpr oldRoot]
        proof <- bindProof "proof" p
        trie2 <-
            bind "trie2"
                $ call
                    "delete"
                    [trie, hex key, hex val, proof]
        pure $ call "root" [trie2] .== rootExpr newRoot

-- | Emit a delete-to-empty test.
emitDeleteToEmpty
    :: String
    -> Hash
    -> ByteString
    -> ByteString
    -> InclusionProof Hash
    -> ModuleM ()
emitDeleteToEmpty name r key val p =
    emitTest name $ do
        trie <-
            bind "trie"
                $ call "from_root" [rootExpr r]
        proof <- bindProof "proof" p
        trie2 <-
            bind "trie2"
                $ call
                    "delete"
                    [trie, hex key, hex val, proof]
        pure $ call "is_empty" [trie2]

-- | Emit a pop test (to remaining root).
emitPop
    :: String
    -> Hash
    -> ByteString
    -> ByteString
    -> Hash
    -> InclusionProof Hash
    -> ModuleM ()
emitPop name r key val remainingRoot p =
    emitTest name $ do
        trie <-
            bind "trie"
                $ call "from_root" [rootExpr r]
        proof <- bindProof "proof" p
        trie2 <-
            bind "trie2"
                $ call
                    "pop"
                    [trie, hex key, hex val, proof]
        pure
            $ call "root" [trie2]
                .== rootExpr remainingRoot

-- | Emit a pop-to-empty test.
emitPopToEmpty
    :: String
    -> Hash
    -> ByteString
    -> ByteString
    -> InclusionProof Hash
    -> ModuleM ()
emitPopToEmpty name r key val p =
    emitTest name $ do
        trie <-
            bind "trie"
                $ call "from_root" [rootExpr r]
        proof <- bindProof "proof" p
        trie2 <-
            bind "trie2"
                $ call
                    "pop"
                    [trie, hex key, hex val, proof]
        pure $ call "is_empty" [trie2]

-- | Emit a pop_max test (to remaining root).
emitPopMax
    :: String
    -> Hash
    -> ByteString
    -> ByteString
    -> Hash
    -> InclusionProof Hash
    -> ModuleM ()
emitPopMax name r key val remainingRoot p =
    emitTest name $ do
        trie <-
            bind "trie"
                $ call "from_root" [rootExpr r]
        proof <- bindProof "proof" p
        trie2 <-
            bind "trie2"
                $ call
                    "pop_max"
                    [trie, hex key, hex val, proof]
        pure
            $ call "root" [trie2]
                .== rootExpr remainingRoot

-- | Emit a pop_max-to-empty test.
emitPopMaxToEmpty
    :: String
    -> Hash
    -> ByteString
    -> ByteString
    -> InclusionProof Hash
    -> ModuleM ()
emitPopMaxToEmpty name r key val p =
    emitTest name $ do
        trie <-
            bind "trie"
                $ call "from_root" [rootExpr r]
        proof <- bindProof "proof" p
        trie2 <-
            bind "trie2"
                $ call
                    "pop_max"
                    [trie, hex key, hex val, proof]
        pure $ call "is_empty" [trie2]

-- | Emit a push_min into empty test.
emitPushMinEmpty
    :: String
    -> ByteString
    -> ByteString
    -> Hash
    -> InclusionProof Hash
    -> ModuleM ()
emitPushMinEmpty name key val r p =
    emitTest name $ do
        proof <- bindProof "proof" p
        trie <-
            bind "trie"
                $ call
                    "push_min"
                    [var "empty", hex key, hex val, proof]
        pure $ call "root" [trie] .== rootExpr r

-- | Emit a push_min test (from existing root).
emitPushMin
    :: String
    -> Hash
    -> ByteString
    -> ByteString
    -> Hash
    -> InclusionProof Hash
    -> ModuleM ()
emitPushMin name oldRoot key val newRoot p =
    emitTest name $ do
        trie <-
            bind "trie"
                $ call "from_root" [rootExpr oldRoot]
        proof <- bindProof "proof" p
        trie2 <-
            bind "trie2"
                $ call
                    "push_min"
                    [trie, hex key, hex val, proof]
        pure
            $ call "root" [trie2]
                .== rootExpr newRoot

-- | Emit a push_max into empty test.
emitPushMaxEmpty
    :: String
    -> ByteString
    -> ByteString
    -> Hash
    -> InclusionProof Hash
    -> ModuleM ()
emitPushMaxEmpty name key val r p =
    emitTest name $ do
        proof <- bindProof "proof" p
        trie <-
            bind "trie"
                $ call
                    "push_max"
                    [var "empty", hex key, hex val, proof]
        pure $ call "root" [trie] .== rootExpr r

-- | Emit a push_max test (from existing root).
emitPushMax
    :: String
    -> Hash
    -> ByteString
    -> ByteString
    -> Hash
    -> InclusionProof Hash
    -> ModuleM ()
emitPushMax name oldRoot key val newRoot p =
    emitTest name $ do
        trie <-
            bind "trie"
                $ call "from_root" [rootExpr oldRoot]
        proof <- bindProof "proof" p
        trie2 <-
            bind "trie2"
                $ call
                    "push_max"
                    [trie, hex key, hex val, proof]
        pure
            $ call "root" [trie2]
                .== rootExpr newRoot

-- | Emit an update test.
emitUpdate
    :: String
    -> Hash
    -> ByteString
    -> ByteString
    -> ByteString
    -> Hash
    -> InclusionProof Hash
    -> ModuleM ()
emitUpdate name oldRoot key oldVal newVal newRoot p =
    emitTest name $ do
        trie <-
            bind "trie"
                $ call "from_root" [rootExpr oldRoot]
        proof <- bindProof "proof" p
        trie2 <-
            bind "trie2"
                $ call
                    "update"
                    [ trie
                    , hex key
                    , proof
                    , hex oldVal
                    , hex newVal
                    ]
        pure
            $ call "root" [trie2]
                .== rootExpr newRoot

-- -----------------------------------------------------------
-- FIFO test helpers
-- -----------------------------------------------------------

-- | Emit a FIFO push test.
emitFifoPush
    :: String
    -> Maybe Hash
    -> Int
    -> Int
    -> Int
    -> ByteString
    -> Hash
    -> InclusionProof Hash
    -> ModuleM ()
emitFifoPush name mOldRoot hd tl ixB val newRoot p =
    emitTest name $ do
        let trieE = case mOldRoot of
                Nothing -> var "empty"
                Just r -> call "from_root" [rootExpr r]
            fifoE =
                record "Fifo" $ do
                    field "trie" trieE
                    field "head" $ int $ fromIntegral hd
                    field "tail" $ int $ fromIntegral tl
        fifo <- bind "fifo" fifoE
        proof <- bindProof "proof" p
        fifo2 <-
            bind "fifo2"
                $ call
                    "push"
                    [ fifo
                    , int $ fromIntegral ixB
                    , hex val
                    , proof
                    ]
        pure
            $ call "root_hash" [fifo2]
                .== rootExpr newRoot

-- | Emit a FIFO pop test.
emitFifoPop
    :: String
    -> Hash
    -> Int
    -> Int
    -> Int
    -> ByteString
    -> Hash
    -> InclusionProof Hash
    -> ModuleM ()
emitFifoPop name r hd tl ixB val newRoot p =
    emitTest name $ do
        let fifoE =
                record "Fifo" $ do
                    field "trie"
                        $ call "from_root" [rootExpr r]
                    field "head" $ int $ fromIntegral hd
                    field "tail" $ int $ fromIntegral tl
        fifo <- bind "fifo" fifoE
        proof <- bindProof "proof" p
        fifo2 <-
            bind "fifo2"
                $ call
                    "pop"
                    [ fifo
                    , int $ fromIntegral ixB
                    , hex val
                    , proof
                    ]
        pure
            $ call "root_hash" [fifo2]
                .== rootExpr newRoot

-- | Emit a FIFO pop-to-empty test.
emitFifoPopToEmpty
    :: String
    -> Hash
    -> Int
    -> Int
    -> Int
    -> ByteString
    -> InclusionProof Hash
    -> ModuleM ()
emitFifoPopToEmpty name r hd tl ixB val p =
    emitTest name $ do
        let fifoE =
                record "Fifo" $ do
                    field "trie"
                        $ call "from_root" [rootExpr r]
                    field "head" $ int $ fromIntegral hd
                    field "tail" $ int $ fromIntegral tl
        fifo <- bind "fifo" fifoE
        proof <- bindProof "proof" p
        fifo2 <-
            bind "fifo2"
                $ call
                    "pop"
                    [ fifo
                    , int $ fromIntegral ixB
                    , hex val
                    , proof
                    ]
        pure $ call "is_empty" [fifo2]

-- -----------------------------------------------------------
-- Aiken module
-- -----------------------------------------------------------

aikenModule :: ModuleM ()
aikenModule = do
    emit
        $ comment
            "Auto-generated CSMT test vectors\
            \ from haskell-mts"
    emit
        $ comment
            "Tests the Aiken CSMT against the\
            \ reference Haskell."
    emit Blank
    emit
        $ useFrom
            "aiken/csmt"
            [ "Proof"
            , "ProofStep"
            , "empty"
            , "from_root"
            , "has"
            , "insert"
            , "delete"
            , "update"
            , "pop"
            , "pop_max"
            , "push_min"
            , "push_max"
            , "root"
            , "is_empty"
            ]
    emit $ useFrom "aiken/csmt/hashing" ["Indirect"]
    emit Blank

    -- 1. Single element: has
    do
        let db = buildTree [("\xab", "\xcd")]
            (Just p, Just r) =
                getProofAndRoot db "\xab"
        emitHas "vec_has_single" "\xab" "\xcd" r p

    -- 2. Single element: insert into empty
    do
        let db = buildTree [("\xab", "\xcd")]
            (Just p, Just r) =
                getProofAndRoot db "\xab"
        emitInsertEmpty
            "vec_insert_into_empty"
            "\xab"
            "\xcd"
            r
            p

    -- 3. Two elements diverging at bit 0
    do
        let db =
                buildTree
                    [("\x00", "\xaa"), ("\x80", "\xbb")]
            (Just p0, Just r) =
                getProofAndRoot db "\x00"
            (Just p1, _) = getProofAndRoot db "\x80"
        emitHas "vec_has_two_left" "\x00" "\xaa" r p0
        emitHas "vec_has_two_right" "\x80" "\xbb" r p1

    -- 4. Two elements with shared prefix
    do
        let db =
                buildTree
                    [ ("\xab\x00", "\x11")
                    , ("\xab\x80", "\x22")
                    ]
            (Just p0, Just r) =
                getProofAndRoot db "\xab\x00"
            (Just p1, _) =
                getProofAndRoot db "\xab\x80"
        emitHas
            "vec_has_shared_prefix_left"
            "\xab\x00"
            "\x11"
            r
            p0
        emitHas
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
            (Just pa, Just r) =
                getProofAndRoot db "\x00"
            (Just pb, _) = getProofAndRoot db "\x40"
            (Just pc, _) = getProofAndRoot db "\x80"
        emitHas "vec_has_three_a" "\x00" "\x01" r pa
        emitHas "vec_has_three_b" "\x40" "\x02" r pb
        emitHas "vec_has_three_c" "\x80" "\x03" r pc

    -- 6. Four elements
    do
        let db =
                buildTree
                    [ ("\x00", "\x10")
                    , ("\x40", "\x20")
                    , ("\x80", "\x30")
                    , ("\xc0", "\x40")
                    ]
            (Just pa, Just r) =
                getProofAndRoot db "\x00"
            (Just pd, _) = getProofAndRoot db "\xc0"
        emitHas "vec_has_four_first" "\x00" "\x10" r pa
        emitHas "vec_has_four_last" "\xc0" "\x40" r pd

    -- 7. Insert sequence: empty -> one -> two
    do
        let db1 = buildTree [("\x10", "\xaa")]
            (Just p1, Just r1) =
                getProofAndRoot db1 "\x10"
        emitInsertEmpty
            "vec_insert_first"
            "\x10"
            "\xaa"
            r1
            p1

        let db2 =
                buildTree
                    [("\x10", "\xaa"), ("\x20", "\xbb")]
            (Just p2, Just r2) =
                getProofAndRoot db2 "\x20"
        emitInsert
            "vec_insert_second"
            r1
            "\x20"
            "\xbb"
            r2
            p2

    -- 8. Delete: two elements, delete one
    do
        let db2 =
                buildTree
                    [("\x50", "\xdd"), ("\xa0", "\xee")]
            (Just pDel, Just r2) =
                getProofAndRoot db2 "\xa0"
            db1 = buildTree [("\x50", "\xdd")]
            (_, Just r1) = getProofAndRoot db1 "\x50"
        emitDelete
            "vec_delete_element"
            r2
            "\xa0"
            "\xee"
            r1
            pDel

    -- 9. Delete last element (back to empty)
    do
        let db = buildTree [("\x42", "\xff")]
            (Just p, Just r) =
                getProofAndRoot db "\x42"
        emitDeleteToEmpty
            "vec_delete_to_empty"
            r
            "\x42"
            "\xff"
            p

    -- 10. Update value
    do
        let db = buildTree [("\x42", "\x01\x02")]
            (Just p, Just rOld) =
                getProofAndRoot db "\x42"
            dbNew = buildTree [("\x42", "\x03\x04")]
            (_, Just rNew) =
                getProofAndRoot dbNew "\x42"
        emitUpdate
            "vec_update_value"
            rOld
            "\x42"
            "\x01\x02"
            "\x03\x04"
            rNew
            p

    -- -------------------------------------------------------
    -- Pop (minimum extraction) test vectors
    -- -------------------------------------------------------

    -- 11. Pop: single element (pop to empty)
    do
        let db = buildTree [("\x00", "\xaa")]
            (Just p, Just r) =
                getProofAndRoot db "\x00"
        emitPopToEmpty
            "vec_pop_single"
            r
            "\x00"
            "\xaa"
            p

    -- 12. Pop: minimum from three elements
    do
        let db =
                buildTree
                    [ ("\x00", "\x01")
                    , ("\x40", "\x02")
                    , ("\x80", "\x03")
                    ]
            (Just pMin, Just r3) =
                getProofAndRoot db "\x00"
            db2 =
                buildTree
                    [("\x40", "\x02"), ("\x80", "\x03")]
            (_, Just r2) = getProofAndRoot db2 "\x40"
        emitPop
            "vec_pop_minimum_of_three"
            r3
            "\x00"
            "\x01"
            r2
            pMin

    -- 13. Pop: minimum from four elements
    do
        let db =
                buildTree
                    [ ("\x00", "\x10")
                    , ("\x40", "\x20")
                    , ("\x80", "\x30")
                    , ("\xc0", "\x40")
                    ]
            (Just pMin, Just r4) =
                getProofAndRoot db "\x00"
            db3 =
                buildTree
                    [ ("\x40", "\x20")
                    , ("\x80", "\x30")
                    , ("\xc0", "\x40")
                    ]
            (_, Just r3) = getProofAndRoot db3 "\x40"
        emitPop
            "vec_pop_minimum_of_four"
            r4
            "\x00"
            "\x10"
            r3
            pMin

    -- -------------------------------------------------------
    -- Pop max vectors
    -- -------------------------------------------------------

    -- pop_max: single element (pop max to empty)
    do
        let db = buildTree [("\xff", "\xaa")]
            (Just p, Just r) =
                getProofAndRoot db "\xff"
        emitPopMaxToEmpty
            "vec_pop_max_single"
            r
            "\xff"
            "\xaa"
            p

    -- pop_max: maximum from three elements
    do
        let db =
                buildTree
                    [ ("\x00", "\x01")
                    , ("\x40", "\x02")
                    , ("\x80", "\x03")
                    ]
            (Just pMax, Just r3) =
                getProofAndRoot db "\x80"
            db2 =
                buildTree
                    [("\x00", "\x01"), ("\x40", "\x02")]
            (_, Just r2) = getProofAndRoot db2 "\x00"
        emitPopMax
            "vec_pop_max_of_three"
            r3
            "\x80"
            "\x03"
            r2
            pMax

    -- pop_max: maximum from four elements
    do
        let db =
                buildTree
                    [ ("\x00", "\x10")
                    , ("\x40", "\x20")
                    , ("\x80", "\x30")
                    , ("\xc0", "\x40")
                    ]
            (Just pMax, Just r4) =
                getProofAndRoot db "\xc0"
            db3 =
                buildTree
                    [ ("\x00", "\x10")
                    , ("\x40", "\x20")
                    , ("\x80", "\x30")
                    ]
            (_, Just r3) = getProofAndRoot db3 "\x00"
        emitPopMax
            "vec_pop_max_of_four"
            r4
            "\xc0"
            "\x40"
            r3
            pMax

    -- -------------------------------------------------------
    -- Push min vectors
    -- -------------------------------------------------------

    -- push_min: insert min into empty
    do
        let db = buildTree [("\x00", "\xaa")]
            (Just p, Just r) =
                getProofAndRoot db "\x00"
        emitPushMinEmpty
            "vec_push_min_single"
            "\x00"
            "\xaa"
            r
            p

    -- push_min: push min into {0x40, 0x80}
    do
        let db3 =
                buildTree
                    [ ("\x00", "\x01")
                    , ("\x40", "\x02")
                    , ("\x80", "\x03")
                    ]
            (Just p, Just r3) =
                getProofAndRoot db3 "\x00"
            db2 =
                buildTree
                    [("\x40", "\x02"), ("\x80", "\x03")]
            (_, Just r2) = getProofAndRoot db2 "\x40"
        emitPushMin
            "vec_push_min_of_three"
            r2
            "\x00"
            "\x01"
            r3
            p

    -- push_min: push min into {0x40, 0x80, 0xc0}
    do
        let db4 =
                buildTree
                    [ ("\x00", "\x10")
                    , ("\x40", "\x20")
                    , ("\x80", "\x30")
                    , ("\xc0", "\x40")
                    ]
            (Just p, Just r4) =
                getProofAndRoot db4 "\x00"
            db3 =
                buildTree
                    [ ("\x40", "\x20")
                    , ("\x80", "\x30")
                    , ("\xc0", "\x40")
                    ]
            (_, Just r3) = getProofAndRoot db3 "\x40"
        emitPushMin
            "vec_push_min_of_four"
            r3
            "\x00"
            "\x10"
            r4
            p

    -- -------------------------------------------------------
    -- Push max vectors
    -- -------------------------------------------------------

    -- push_max: insert max into empty
    do
        let db = buildTree [("\xff", "\xaa")]
            (Just p, Just r) =
                getProofAndRoot db "\xff"
        emitPushMaxEmpty
            "vec_push_max_single"
            "\xff"
            "\xaa"
            r
            p

    -- push_max: push max into {0x00, 0x40}
    do
        let db3 =
                buildTree
                    [ ("\x00", "\x01")
                    , ("\x40", "\x02")
                    , ("\x80", "\x03")
                    ]
            (Just p, Just r3) =
                getProofAndRoot db3 "\x80"
            db2 =
                buildTree
                    [("\x00", "\x01"), ("\x40", "\x02")]
            (_, Just r2) = getProofAndRoot db2 "\x00"
        emitPushMax
            "vec_push_max_of_three"
            r2
            "\x80"
            "\x03"
            r3
            p

    -- push_max: push max into {0x00, 0x40, 0x80}
    do
        let db4 =
                buildTree
                    [ ("\x00", "\x10")
                    , ("\x40", "\x20")
                    , ("\x80", "\x30")
                    , ("\xc0", "\x40")
                    ]
            (Just p, Just r4) =
                getProofAndRoot db4 "\xc0"
            db3 =
                buildTree
                    [ ("\x00", "\x10")
                    , ("\x40", "\x20")
                    , ("\x80", "\x30")
                    ]
            (_, Just r3) = getProofAndRoot db3 "\x00"
        emitPushMax
            "vec_push_max_of_four"
            r3
            "\xc0"
            "\x40"
            r4
            p

    -- -------------------------------------------------------
    -- FIFO test vectors (indexBytes = 1)
    -- -------------------------------------------------------

    emit Blank
    emit $ comment "FIFO test vectors"
    emit Blank

    let ixB = 1 :: Int
        fifoKey = counterToKey ixB
        vA = "\xaa" :: ByteString
        vB = "\xbb" :: ByteString
        vC = "\xcc" :: ByteString

    -- Build intermediate DBs
    let db1 = buildTree [(fifoKey 0, vA)]
        db2 =
            buildTree
                [(fifoKey 0, vA), (fifoKey 1, vB)]
        db3 =
            buildTree
                [ (fifoKey 0, vA)
                , (fifoKey 1, vB)
                , (fifoKey 2, vC)
                ]

    -- FIFO: push one element
    do
        let (Just p, Just r) =
                getProofAndRoot db1 (fifoKey 0)
        emitFifoPush
            "vec_fifo_push_one"
            Nothing
            0
            0
            ixB
            vA
            r
            p

    -- FIFO: push second element
    do
        let (Just p, Just r2) =
                getProofAndRoot db2 (fifoKey 1)
            (_, Just r1) =
                getProofAndRoot db1 (fifoKey 0)
        emitFifoPush
            "vec_fifo_push_two"
            (Just r1)
            0
            1
            ixB
            vB
            r2
            p

    -- FIFO: push third element
    do
        let (Just p, Just r3) =
                getProofAndRoot db3 (fifoKey 2)
            (_, Just r2) =
                getProofAndRoot db2 (fifoKey 1)
        emitFifoPush
            "vec_fifo_push_three"
            (Just r2)
            0
            2
            ixB
            vC
            r3
            p

    -- FIFO: pop first element from 3-element queue
    do
        let (Just p0, Just r3) =
                getProofAndRoot db3 (fifoKey 0)
            db2after =
                buildTree
                    [(fifoKey 1, vB), (fifoKey 2, vC)]
            (_, Just r2after) =
                getProofAndRoot db2after (fifoKey 1)
        emitFifoPop
            "vec_fifo_pop_one"
            r3
            0
            3
            ixB
            vA
            r2after
            p0

    -- FIFO: pop all three (sequential)
    do
        let (Just p0, Just r3) =
                getProofAndRoot db3 (fifoKey 0)
            db_12 =
                buildTree
                    [(fifoKey 1, vB), (fifoKey 2, vC)]
            (Just p1, Just r12) =
                getProofAndRoot db_12 (fifoKey 1)
            db_2 = buildTree [(fifoKey 2, vC)]
            (Just p2, Just _r2only) =
                getProofAndRoot db_2 (fifoKey 2)

        emitTest "vec_fifo_pop_all" $ do
            -- Pop element 0
            let fifo0E =
                    record "Fifo" $ do
                        field "trie"
                            $ call
                                "from_root"
                                [rootExpr r3]
                        field "head" $ int 0
                        field "tail" $ int 3
            fifo <- bind "fifo" fifo0E
            proof <- bindProof "proof" p0
            fifo2 <-
                bind "fifo2"
                    $ call
                        "pop"
                        [ fifo
                        , int $ fromIntegral ixB
                        , hex vA
                        , proof
                        ]
            -- Pop element 1
            proof2 <- bindProof "proof2" p1
            fifo3 <-
                bind "fifo3"
                    $ call
                        "pop"
                        [ fifo2
                        , int $ fromIntegral ixB
                        , hex vB
                        , proof2
                        ]
            -- Pop element 2
            proof3 <- bindProof "proof3" p2
            fifo4 <-
                bind "fifo4"
                    $ call
                        "pop"
                        [ fifo3
                        , int $ fromIntegral ixB
                        , hex vC
                        , proof3
                        ]
            pure $ call "is_empty" [fifo4]

-- -----------------------------------------------------------
-- Main
-- -----------------------------------------------------------

main :: IO ()
main =
    putStr
        $ renderModule
        $ runModule aikenModule

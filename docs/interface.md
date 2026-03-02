# MTS Interface

The shared `MerkleTreeStore` interface lets application code work with any
trie implementation without depending on CSMT or MPF internals.

## Type Families

Each implementation defines a phantom type tag (e.g. `CsmtImpl`, `MpfImpl`)
and provides type family instances:

```haskell
type family MtsKey imp               -- Key type
type family MtsValue imp             -- Value type
type family MtsHash imp              -- Hash type
type family MtsProof imp             -- Inclusion proof type
type family MtsLeaf imp              -- Leaf type (for completeness proofs)
type family MtsCompletenessProof imp -- Completeness proof type
```

### CSMT Type Instances

| Family | Type |
|--------|------|
| `MtsKey CsmtImpl` | `ByteString` |
| `MtsValue CsmtImpl` | `ByteString` |
| `MtsHash CsmtImpl` | `Hash` (Blake2b-256) |
| `MtsProof CsmtImpl` | `InclusionProof Hash` |
| `MtsLeaf CsmtImpl` | `Indirect Hash` |
| `MtsCompletenessProof CsmtImpl` | `CompletenessProof` |

### MPF Type Instances

| Family | Type |
|--------|------|
| `MtsKey MpfImpl` | `ByteString` |
| `MtsValue MpfImpl` | `ByteString` |
| `MtsHash MpfImpl` | `MPFHash` (Blake2b-256) |
| `MtsProof MpfImpl` | `MPFProof MPFHash` |
| `MtsLeaf MpfImpl` | `HexIndirect MPFHash` |
| `MtsCompletenessProof MpfImpl` | `()` (not yet implemented) |

## MerkleTreeStore Record

The `MerkleTreeStore` record provides 10 operations:

```haskell
data MerkleTreeStore imp m = MerkleTreeStore
    { mtsInsert       :: MtsKey imp -> MtsValue imp -> m ()
    , mtsDelete       :: MtsKey imp -> m ()
    , mtsRootHash     :: m (Maybe (MtsHash imp))
    , mtsMkProof      :: MtsKey imp -> m (Maybe (MtsProof imp))
    , mtsVerifyProof  :: MtsValue imp -> MtsProof imp -> m Bool
    , mtsFoldProof    :: MtsHash imp -> MtsProof imp -> MtsHash imp
    , mtsBatchInsert  :: [(MtsKey imp, MtsValue imp)] -> m ()
    , mtsCollectLeaves :: m [MtsLeaf imp]
    , mtsMkCompletenessProof
        :: m (Maybe (MtsCompletenessProof imp))
    , mtsVerifyCompletenessProof
        :: [MtsLeaf imp] -> MtsCompletenessProof imp -> m Bool
    }
```

## Constructors

### `csmtMerkleTreeStore`

Build a CSMT-backed store. Requires a natural transformation from the
database monad to `IO`, a `Database` handle, a `FromKV` record, and a
`Hashing` record:

```haskell
csmtMerkleTreeStore
    :: (MonadFail m)
    => (forall b. m b -> IO b)
    -> Database m StandaloneCF (Standalone ByteString ByteString Hash) StandaloneOp
    -> FromKV ByteString ByteString Hash
    -> Hashing Hash
    -> MerkleTreeStore CsmtImpl IO
```

### `mpfMerkleTreeStore`

Build an MPF-backed store. Same pattern, with MPF-specific types:

```haskell
mpfMerkleTreeStore
    :: (MonadFail m)
    => (forall b. m b -> IO b)
    -> Database m MPFStandaloneCF (MPFStandalone ByteString ByteString MPFHash) MPFStandaloneOp
    -> FromHexKV ByteString ByteString MPFHash
    -> MPFHashing MPFHash
    -> MerkleTreeStore MpfImpl IO
```

## Usage Example

From the test suite (`MTS.PropertySpec`), showing how to construct both
stores:

```haskell
-- CSMT store using in-memory backend
mkCsmtStore :: IO (MerkleTreeStore CsmtImpl IO)
mkCsmtStore = do
    ref <- newIORef emptyInMemoryDB
    let run action = do
            db <- readIORef ref
            let (a, db') = runPure db action
            writeIORef ref db'
            pure a
    pure $ csmtMerkleTreeStore run (pureDatabase csmtCodecs)
                               fromKVHashes hashHashing

-- MPF store using in-memory backend
mkMpfStore :: IO (MerkleTreeStore MpfImpl IO)
mkMpfStore = do
    ref <- newIORef emptyMPFInMemoryDB
    let run action = do
            db <- readIORef ref
            let (a, db') = runMPFPure db action
            writeIORef ref db'
            pure a
    pure $ mpfMerkleTreeStore run (mpfPureDatabase mpfCodecs)
                              fromHexKVBS mpfHashing
```

## Shared QuickCheck Properties

The `MTS.Properties` module provides 12 properties that any
`MerkleTreeStore` implementation should satisfy:

| # | Property | Description |
|---|----------|-------------|
| 1 | `propInsertVerify` | Insert k v, then verify k v returns True |
| 2 | `propMultipleInsertAllVerify` | Insert N pairs, all verify |
| 3 | `propInsertionOrderIndependence` | Same keys in any order produce the same root hash |
| 4 | `propDeleteRemovesKey` | Insert k v, delete k, verify fails |
| 5 | `propDeletePreservesSiblings` | Delete one key, other keys still verify |
| 6 | `propBatchEqualsSequential` | Batch insert produces same root as sequential |
| 7 | `propInsertDeleteAllEmpty` | Insert N, delete all N, root is Nothing |
| 8 | `propEmptyTreeNoRoot` | Empty tree has no root hash |
| 9 | `propSingleInsertHasRoot` | Single insert produces a root hash |
| 10 | `propWrongValueRejects` | Verify with wrong value returns False |
| 11 | `propCompletenessRoundTrip` | Insert N, completeness proof verifies |
| 12 | `propCompletenessEmpty` | Empty tree has no completeness proof |
| 13 | `propCompletenessAfterDelete` | Completeness proof verifies after partial deletion |

CSMT passes all 13 properties. MPF passes properties 1-10; completeness
properties (11-13) are pending implementation.

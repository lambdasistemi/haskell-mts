# CSMT (Binary Trie)

The Compact Sparse Merkle Tree is MTS's binary trie implementation. It uses
single-bit branching (L/R directions) with path compression via jump fields.

!!! info
    This page covers CSMT-specific details. For the shared interface, see
    [MTS Interface](interface.md). For general Merkle tree concepts, see
    [Concepts](concepts.md).

## Overview

CSMT is the original implementation in this package. It provides:

- Binary trie with path compression (`Indirect` nodes with `jump` fields)
- CBOR-encoded inclusion proofs (see
  [Inclusion Proof Format](architecture/inclusion-proof.md))
- Completeness proofs over prefix-grouped subtrees
- Secondary indexing via configurable `treePrefix`
- CLI tool for interactive operations (see [CLI Manual](manual.md))
- TypeScript proof verifier for browser/Node.js (see
  [TypeScript Verifier](typescript.md))

## Key Types

### `FromKV` - Key/Value Conversion

```haskell
data FromKV k v a = FromKV
    { fromK      :: k -> Key        -- User key to binary path
    , fromV      :: v -> a           -- User value to hash
    , treePrefix :: v -> Key         -- Optional prefix for secondary indexing
    }
```

The default `fromKVHashes` hashes both key and value with Blake2b-256 and
uses no prefix (`treePrefix = const []`).

### `Indirect` - Tree Nodes

```haskell
data Indirect a = Indirect
    { jump  :: Key    -- Path compression: bits to skip
    , value :: a      -- Hash value
    }
```

### `Hashing` - Hash Operations

```haskell
data Hashing a = Hashing
    { rootHash    :: Indirect a -> a
    , combineHash :: Indirect a -> Indirect a -> a
    }
```

## Storage

CSMT uses two columns:

| Column | Key | Value |
|--------|-----|-------|
| KV | User key (`k`) | User value (`v`) |
| CSMT | `treePrefix(v) <> fromK(k)` | `Indirect a` (jump + hash) |

See [Storage Layer](architecture/storage.md) for serialization details.

## Inclusion Proofs

CSMT inclusion proofs are CBOR-encoded and self-contained. They include
the key, value hash, root hash, proof steps (sibling hashes), and root
jump path. Verification is pure computation.

See [Inclusion Proof Format](architecture/inclusion-proof.md) for the
CDDL specification and verification algorithm.

## Completeness Proofs

CSMT supports completeness proofs via the `CSMT.Proof.Completeness`
module. When `treePrefix` groups entries by a common prefix, a
completeness proof demonstrates that a given set of leaves is the
**entire** contents of that subtree:

1. Collect all leaves under the prefix (`collectValues`)
2. Generate a proof (`generateProof`)
3. Verify by folding the proof and comparing the reconstructed root

## CLI Tool

The `mts` executable provides an interactive CLI for CSMT operations.
It reads from `CSMT_DB_PATH` and supports insert, delete, query, root
hash, and proof generation/verification.

See [CLI Manual](manual.md) for full usage.

## Parallel Population (`patchParallel`)

For bulk-loading large datasets (e.g. Cardano UTxO restoration) or
replaying journal entries, `CSMT.Populate.patchParallel` builds
independent bucket transactions from a batch of operations:

1. **Prepare**: caller runs `expandToBucketDepth` to push any
   path-compressed node whose jump crosses the bucket boundary
   below it. This ensures each bucket's subtree is self-contained.
2. **Bucket**: `patchParallel` groups operations by the first N
   bits of the tree key and returns one `Transaction` per non-empty
   bucket. Each transaction applies tree ops (`insertingDirect` /
   `deletingDirect`) and deletes the corresponding journal entries.
3. **Execute**: the caller runs the returned transactions
   concurrently (e.g. via `mapConcurrently_`). The subtrees write
   to disjoint regions of the CSMT column.
4. **Merge**: caller runs `mergeSubtreeRoots` to read the subtree
   roots and rebuild the top N levels with correct path compression.

Works on both empty and non-empty trees. Supports inserts and deletes
via the `PatchOp` type.

```haskell
patchParallel
    :: (GCompare d, Ord jk, Monad m)
    => Int                         -- bucket bits (e.g. 4 → 16 buckets)
    -> Key                         -- global prefix
    -> Hashing a
    -> Selector d Key (Indirect a) -- CSMT column
    -> Selector d jk v             -- journal column
    -> [(jk, PatchOp Key a)]       -- (journal key, tree op) pairs
    -> [Transaction m cf d ops ()] -- independent bucket transactions
```

Benchmarks on a development machine (RocksDB, `-O2 -threaded -N`):

| N entries | Sequential | 4 bits (16x) | 8 bits (256x) |
|-----------|------------|--------------|---------------|
| 1,000 | 6,500/s | 34,000/s (5x) | 43,000/s (7x) |
| 5,000 | 5,000/s | 29,000/s (6x) | 35,000/s (7x) |
| 10,000 | 4,700/s | 25,000/s (5x) | 32,000/s (7x) |
| 50,000 | 3,700/s | 19,000/s (5x) | 24,000/s (6x) |

## Split-Mode Operations (`Ops` GADT)

The `Ops` GADT provides bidirectional mode transitions for the full
KVOnly ↔ Full lifecycle:

```haskell
data Ops (mode :: Mode) m cf d ops k v a where
    OpsKVOnly :: { kvCommon :: CommonOps, toFull :: IO (Maybe ...) } -> Ops 'KVOnly ...
    OpsFull   :: { fullCommon :: CommonOps, opsRootHash :: ..., toKVOnly :: IO (Maybe ...) } -> Ops 'Full ...
```

- **KVOnly**: insert/delete write KV + journal. `toFull` replays
  the journal via `patchParallel` with concurrent execution.
- **Full**: insert/delete write KV + update CSMT tree. `toKVOnly`
  verifies journal is empty before transitioning.
- **`CommonOps`**: `opsInsert`, `opsDelete`, `opsQuery` —
  available in both modes.

### Crash Safety

The `toFull` transition performs three non-atomic steps:

1. `expandToBucketDepth` — separate transaction
2. `mapConcurrently_ (runTx . snd) bucketTxns` — N parallel
   transactions (each deletes its journal entries)
3. `mergeSubtreeRoots` — separate transaction

A **sentinel flag** in the journal column brackets this sequence:

```
1. Write sentinel + expandToBucketDepth (one transaction)
2. replayLoop (parallel bucket transactions)
3. mergeSubtreeRoots + delete sentinel (one transaction)
```

If the process crashes between steps 1–4, the next `toFull` call
detects the sentinel, runs `mergeSubtreeRoots` to fix the tree
top, deletes the sentinel, then replays remaining journal entries
normally.

**Sentinel format**: key = `""` (empty, sorts first), value =
`0xFF || Word8(bucketBits) || encodedPrefix`.

**Recovery guarantees**:

- Each bucket transaction is atomic (tree ops + journal deletes)
- Committed buckets are consistent; their journal entries are gone
- Uncommitted entries remain in the journal for re-replay
- `expandToBucketDepth` is idempotent
- `mergeSubtreeRoots` reads subtree roots and rebuilds the top;
  safe to re-run after partial bucket commits

The `DbState` type exposes recovery at database open time:

```haskell
data DbState m cf d ops k v a
    = NeedsRecovery (IO (DbState m cf d ops k v a))
    | Ready (ReadyState m cf d ops k v a)
```

## Benchmarks: CSMT vs MPF

Unified benchmark comparing CSMT (binary trie, Haskell/RocksDB) against
MPF (16-ary trie, Haskell/RocksDB and JS/LevelDB reference). All use
blake2b-hashed keys for identical trie depth. Proofs use Aiken CBOR for
MPF and compact CBOR for CSMT.

### N = 1,000

| | Insert | Proof gen | Delete | Proof CBOR | DB size |
|---|---|---|---|---|---|
| **CSMT (Haskell/RocksDB)** | 3,542/s | 5,486/s | 4,154/s | **453 bytes** | 1,053 KB |
| **MPF (Haskell/RocksDB)** | 3,146/s | 3,073/s | 2,860/s | 426 bytes | **414 KB** |
| MPF JS (LevelDB) | 897/s | 1,467/s | 1,050/s | 426 bytes | 4,362 KB |

### N = 10,000

| | Insert | Proof gen | Delete | Proof CBOR | DB size |
|---|---|---|---|---|---|
| **CSMT (Haskell/RocksDB)** | 2,750/s | 4,285/s | 3,170/s | 582 bytes | 12,412 KB |
| **MPF (Haskell/RocksDB)** | 2,406/s | 2,451/s | 2,212/s | **538 bytes** | **3,557 KB** |
| MPF JS (LevelDB) | 723/s | 1,082/s | 805/s | 538 bytes | 10,172 KB |

### N = 100,000

| | Insert | Proof gen | Delete | Proof CBOR | DB size |
|---|---|---|---|---|---|
| **CSMT (Haskell/RocksDB)** | 2,146/s | 3,410/s | 2,482/s | 711 bytes | 169,778 KB |
| **MPF (Haskell/RocksDB)** | 1,807/s | 1,846/s | 1,710/s | **646 bytes** | **38,185 KB** |

### Key findings

- **CSMT is faster** on all operations (1.2-1.8x on insert, 1.5-1.8x on proofs)
- **MPF proofs are 6-9% smaller** at N=1K (426 vs 453 bytes), gap narrows at scale
- **MPF DB is 2.5-4.5x smaller** than CSMT across all sizes
- **Both Haskell implementations are 3-4x faster** than the JS MPF reference
- CSMT compact proofs are **58% smaller** than the old CBOR encoding (453 vs 1,079 at N=1K)

### Proof size details

| N | CSMT old CBOR | CSMT compact | MPF Aiken CBOR |
|---|---|---|---|
| 1,000 | 1,079 bytes | **453 bytes** | 426 bytes |
| 10,000 | 1,195 bytes | **582 bytes** | 538 bytes |
| 100,000 | 1,321 bytes | **711 bytes** | 646 bytes |

### patchParallel (CSMT bulk population)

For initial population from N entries, `patchParallel` provides 5-7x
speedup over sequential insertion by distributing work across 16-256
independent subtree buckets:

| N entries | Sequential | 4 bits (16x) | 8 bits (256x) |
|-----------|------------|--------------|---------------|
| 1,000 | 6,500/s | 34,000/s (5x) | 43,000/s (7x) |
| 5,000 | 5,000/s | 29,000/s (6x) | 35,000/s (7x) |
| 10,000 | 4,700/s | 25,000/s (5x) | 32,000/s (7x) |
| 50,000 | 3,700/s | 19,000/s (5x) | 24,000/s (6x) |

## Worked Example

See [Worked Example](architecture/example.md) for a step-by-step
walkthrough of CSMT storage and hash computation.

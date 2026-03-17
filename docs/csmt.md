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

For bulk-loading large datasets (e.g. Cardano UTxO restoration),
`CSMT.Populate.patchParallel` inserts entries in parallel:

1. **Prepare**: `expandToBucketDepth` pushes any path-compressed node
   whose jump crosses the bucket boundary below it. This ensures
   each bucket's subtree is self-contained.
2. **Produce**: the caller streams `(treeKey, hash)` pairs via a
   callback. Each pair is dispatched to the correct bucket's
   `TBQueue` by the first N bits of the tree key.
3. **Consume**: `2^N` consumer threads each build their subtree
   in batched transactions using `insertingDirect`. The subtrees
   write to disjoint regions of the CSMT column.
4. **Merge**: `mergeSubtreeRoots` reads the subtree roots and
   rebuilds the top N levels with correct path compression.

Works on both empty and non-empty trees.

```haskell
patchParallel
    :: Int                -- bucket bits (e.g. 4 → 16 buckets)
    -> Int                -- batch size per transaction
    -> Natural            -- TBQueue bound (backpressure)
    -> Key                -- global prefix
    -> Hashing a
    -> Selector d Key (Indirect a)
    -> (forall b. Transaction IO cf d ops b -> IO b)
    -> ((Key -> a -> IO ()) -> IO ())  -- producer callback
    -> IO ()
```

Benchmarks on a development machine (RocksDB, `-O2 -threaded -N`):

| N entries | Sequential | 4 bits (16x) | 8 bits (256x) |
|-----------|------------|--------------|---------------|
| 1,000 | 6,500/s | 34,000/s (5x) | 43,000/s (7x) |
| 5,000 | 5,000/s | 29,000/s (6x) | 35,000/s (7x) |
| 10,000 | 4,700/s | 25,000/s (5x) | 32,000/s (7x) |
| 50,000 | 3,700/s | 19,000/s (5x) | 24,000/s (6x) |

## Benchmarks

Preliminary benchmarks show ~900 insertions/second on a standard
development machine over a 3.5M Cardano UTxO dataset.

## Worked Example

See [Worked Example](architecture/example.md) for a step-by-step
walkthrough of CSMT storage and hash computation.

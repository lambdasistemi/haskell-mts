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

## Benchmarks

Preliminary benchmarks show ~900 insertions/second on a standard
development machine over a 3.5M Cardano UTxO dataset.

## Worked Example

See [Worked Example](architecture/example.md) for a step-by-step
walkthrough of CSMT storage and hash computation.

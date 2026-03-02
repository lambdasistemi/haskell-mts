!!! warning
    This project is in early development and is not production-ready. Use at your own risk.

# MTS - Merkle Tree Store

## What is MTS?

MTS (Merkle Tree Store) is a Haskell library providing a shared interface for
authenticated key-value stores backed by Merkle tries. It ships with two
implementations:

- **CSMT** - Compact Sparse Merkle Tree: a binary trie with path compression,
  CBOR-encoded inclusion proofs, and completeness proofs via secondary
  indexing.
- **MPF** - Merkle Patricia Forest: a 16-ary trie using hex nibble keys,
  with batch/streaming inserts and root hashes compatible with the Aiken
  reference implementation.

Both implementations conform to a single `MerkleTreeStore` record type
parameterised by an implementation tag and a monad, so application code
can be written once and run against either backend.

## Features

- **Shared interface**: `MerkleTreeStore` record with type families for key,
  value, hash, proof, leaf, and completeness proof types
  ([MTS Interface](interface.md))
- **12 QuickCheck properties**: Verify feature parity across implementations
  (insert-verify, order independence, batch equivalence, completeness
  round-trip, etc.)
- **Two trie backends**: Binary (CSMT) and 16-ary (MPF), each with RocksDB
  and in-memory storage
- **Merkle proofs**: Inclusion proofs for both implementations; CSMT also
  supports completeness proofs over prefix-grouped subtrees
- **Batch and streaming inserts**: MPF supports `insertingBatch`,
  `insertingChunked`, and `insertingStream` for large datasets
- **Aiken compatibility**: MPF produces root hashes matching the Aiken
  `MerkleTree` implementation (verified against the 30-fruit test vector)
- **CLI tool**: Interactive command-line interface for CSMT tree operations
- **TypeScript verifier**: Client-side CSMT proof verification for
  browser/Node.js

## Quick Start

=== "MTS Interface"
    ```haskell
    import MTS.Interface (MerkleTreeStore(..))

    example :: MerkleTreeStore imp IO -> IO ()
    example store = do
        mtsInsert store "key" "value"
        proof <- mtsMkProof store "key"
        root  <- mtsRootHash store
        print (proof, root)
    ```

=== "CLI"
    ```bash
    export CSMT_DB_PATH=./mydb
    mts
    > i key1 value1
    > q key1
    AQDjun1C8tTl1kdY1oon8sAQWL86/UMiJyZFswQ9Sf49XQAA
    ```

## Status

### Shared Interface (`mts`)
- [x] `MerkleTreeStore` record with type families
- [x] 12 shared QuickCheck properties
- [x] CSMT passes all 12 properties
- [x] MPF passes 9 of 12 (completeness proofs pending)

### CSMT Implementation (`mts:csmt`)
- [x] Insertion and deletion
- [x] Inclusion proof generation and verification (CBOR)
- [x] Completeness proofs (prefix-based subtrees)
- [x] Persistent storage (RocksDB)
- [x] Secondary indexing via `treePrefix`
- [x] CLI tool
- [x] TypeScript proof verifier
- [x] Insertion benchmarks

### MPF Implementation (`mts:mpf`)
- [x] Insertion and deletion
- [x] Inclusion proof generation and verification
- [x] Batch, chunked, and streaming inserts
- [x] Aiken-compatible root hashes
- [x] Persistent storage (RocksDB)
- [ ] Completeness proofs
- [ ] Benchmarks

### Planned
- [ ] HTTP service with RESTful API
- [ ] MPF completeness proofs
- [ ] Production-grade testing

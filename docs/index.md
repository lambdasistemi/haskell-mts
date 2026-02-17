!!! warning
    This project is in early development and is not production-ready. Use at your own risk.

# CSMT - Compact Sparse Merkle Tree

## What is CSMT?

A Compact Sparse Merkle Tree is a space-efficient variant of a Merkle tree optimized for sparse key spaces. It enables cryptographic proofs of inclusion (or exclusion) while minimizing storage requirements through path compression.

## Features

This package provides:

- **Haskell Library**: A CSMT implementation with persistent storage backends, offering efficient insertion, deletion, and proof generation for applications requiring verifiable data structures.
- **Secondary Indexing**: Configurable `treePrefix` allows grouping entries by a value-derived prefix (e.g. address), enabling completeness proofs over subsets of the tree.
- **CLI Tool**: Interactive command-line interface for tree operations including adding/removing elements, generating proofs, and verifying membership.
- **Preimage Storage**: Automatic storage of key-value preimages in sync with the CSMT, enabling value retrieval alongside proof verification.

## Performance

Preliminary benchmarks indicate that the CSMT library sustains a throughput of 900 insertions per second on a standard development machine over a 3.5M cardano UTxOs dataset.

There is room for optimization via parallel insertions, but these results are promising for an initial implementation.

## Quick Start

=== "CLI"
    ```bash
    export CSMT_DB_PATH=./mydb
    csmt
    > i key1 value1
    > q key1
    AQDjun1C8tTl1kdY1oon8sAQWL86/UMiJyZFswQ9Sf49XQAA
    ```

=== "Library"
    ```haskell
    import CSMT
    import CSMT.Backend.RocksDB

    main = withRocksDB "mydb" 256 256 $ \runDB -> do
        runDB $ runTransaction $
            insert fromKVHashes kvCol csmtCol "key" "value"
    ```

## Status

### Library
- [x] Insertion and deletion
- [x] Proof generation and verification
- [x] Persistent storage (RocksDB)
- [x] Comprehensive tests
- [x] Insertion benchmarks
- [ ] Deletion/proof benchmarks
- [ ] Production-grade testing
- [ ] Raw key support (vs hashed keys)

### CLI Tool
- [x] Add/remove elements
- [x] Query elements
- [x] Generate and verify proofs

### Planned
- [ ] HTTP service with RESTful API
- [ ] Parallel batch insertions

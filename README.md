# MTS - Merkle Tree Store

[![CI](https://github.com/lambdasistemi/haskell-mts/actions/workflows/CI.yaml/badge.svg)](https://github.com/lambdasistemi/haskell-mts/actions/workflows/CI.yaml)
[![Documentation](https://github.com/lambdasistemi/haskell-mts/actions/workflows/deploy-docs.yaml/badge.svg)](https://github.com/lambdasistemi/haskell-mts/actions/workflows/deploy-docs.yaml)

A Haskell library providing a shared Merkle tree store interface with two
implementations:

- **CSMT** - Compact Sparse Merkle Tree (binary trie, path compression, CBOR
  inclusion proofs)
- **MPF** - Merkle Patricia Forest (16-ary trie, hex nibble keys, Aiken
  compatible)

Both implementations share a common `MerkleTreeStore` record with 12
QuickCheck properties proving feature parity.

> **Warning**: This project is in early development and is not production-ready.

## Features

- **Shared interface**: `MerkleTreeStore` record parameterised by
  implementation tag and monad, with type families for key/value/hash/proof
  types
- **Two trie backends**: Binary (CSMT) and 16-ary (MPF), swappable via the
  shared interface
- **Merkle proofs**: Inclusion proofs for both implementations; CSMT also
  supports completeness proofs
- **Persistent storage**: RocksDB backend for both implementations
- **Batch and streaming inserts**: MPF supports batch, chunked, and streaming
  insertion modes
- **Aiken compatibility**: MPF produces root hashes matching the Aiken
  reference implementation
- **CLI tool**: Interactive command-line interface for CSMT operations
- **TypeScript verifier**: Client-side CSMT proof verification in
  browser/Node.js

## Quick Start

### Using the MTS Interface (recommended)

```haskell
import MTS.Interface (MerkleTreeStore(..))

-- Works with any implementation
example :: MerkleTreeStore imp IO -> IO ()
example store = do
    mtsInsert store "key" "value"
    proof <- mtsMkProof store "key"
    root  <- mtsRootHash store
    print (proof, root)
```

### Constructing a CSMT Store

```haskell
import CSMT.MTS (csmtMerkleTreeStore)
import CSMT.Hashes (fromKVHashes, hashHashing)
import CSMT.Backend.RocksDB (withStandaloneRocksDB)

main :: IO ()
main = withStandaloneRocksDB "mydb" codecs $ \run db ->
    let store = csmtMerkleTreeStore run db fromKVHashes hashHashing
    in mtsInsert store "key" "value"
```

### Constructing an MPF Store

```haskell
import MPF.MTS (mpfMerkleTreeStore)
import MPF.Hashes (fromHexKVHashes, mpfHashing)
import MPF.Backend.RocksDB (withMPFStandaloneRocksDB)

main :: IO ()
main = withMPFStandaloneRocksDB "mydb" codecs $ \run db ->
    let store = mpfMerkleTreeStore run db fromHexKVHashes mpfHashing
    in mtsInsert store "key" "value"
```

## Installation

### Using Nix

```bash
nix shell nixpkgs#cachix -c cachix use paolino
nix shell github:lambdasistemi/haskell-mts --refresh
```

### Using Cabal

Requires a working Haskell environment and RocksDB development files:

```bash
cabal install
```

## CLI Tool

The `mts` executable provides an interactive CLI for CSMT operations:

```bash
export CSMT_DB_PATH=./mydb
mts
> i key1 value1
> q key1
AQDjun1C8tTl1kdY1oon8sAQWL86/UMiJyZFswQ9Sf49XQAA
> r
NrJMih3czFriydMUwvFKFK6VYKZYVjKpKGe1WC4e+VU=
```

## Documentation

Full documentation at [lambdasistemi.github.io/haskell-mts](https://lambdasistemi.github.io/haskell-mts/)

## License

Apache-2.0

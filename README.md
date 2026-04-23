# MTS - Merkle Tree Store

[![CI](https://github.com/lambdasistemi/haskell-mts/actions/workflows/CI.yaml/badge.svg)](https://github.com/lambdasistemi/haskell-mts/actions/workflows/CI.yaml)
[![Documentation](https://github.com/lambdasistemi/haskell-mts/actions/workflows/deploy-docs.yaml/badge.svg)](https://github.com/lambdasistemi/haskell-mts/actions/workflows/deploy-docs.yaml)

A Haskell library providing a shared Merkle tree store interface with two
implementations:

- **CSMT** - Compact Sparse Merkle Tree (binary trie, path compression, CBOR
  inclusion proofs)
- **MPF** - Merkle Patricia Forest (16-ary trie, hex nibble keys, Aiken
  compatible)

Both implementations share a common `MerkleTreeStore` record. The shared
QuickCheck property suite currently contains 13 properties: CSMT passes all
13, while MPF passes the first 10 and still leaves completeness proofs
pending.

> **Warning**: This project is in early development and is not production-ready.

## Features

- **Shared interface**: `MerkleTreeStore` record parameterised by
  implementation tag and monad, with type families for key/value/hash/proof
  types
- **Two trie backends**: Binary (CSMT) and 16-ary (MPF), swappable via the
  shared interface
- **Merkle proofs**: Inclusion and exclusion proofs for both implementations;
  CSMT also supports completeness proofs
- **Persistent storage**: RocksDB backend for both implementations
- **Batch and streaming inserts**: MPF supports batch, chunked, and streaming
  insertion modes
- **Aiken compatibility**: MPF produces root hashes and proof-step encodings
  matching the Aiken reference implementation
- **Browser demos**: three static demos shipped through the docs site
  (`csmt-verify.wasm`, `csmt-write.wasm`, `mpf-write.wasm` +
  `mpf-verify.wasm`)
- **CLI tool**: Interactive command-line interface for CSMT operations
- **TypeScript verifier**: Client-side CSMT proof verification in
  browser/Node.js
- **Pure MPF verifier**: exact Aiken inclusion/exclusion verification in
  Haskell via `MPF.Verify`

## What Landed For MPF

- explicit exclusion proofs with the same Aiken proof-step transport used for
  inclusion proofs
- a pure `MPF.Verify` module for exact Aiken proof-step verification
- `mpf-write.wasm` and `mpf-verify.wasm`, wired into a browser demo that can
  build, prove, and verify entirely in the browser
- docs-site packaging for all three demos: CSMT verify, CSMT write, and MPF
  write

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
import MPF.Hashes (fromHexKVAikenHashes, mpfHashing)
import MPF.Backend.RocksDB (withMPFStandaloneRocksDB)

main :: IO ()
main = withMPFStandaloneRocksDB "mydb" codecs $ \run db ->
    let store = mpfMerkleTreeStore run db fromHexKVAikenHashes mpfHashing
    in mtsInsert store "key" "value"
```

Use `fromHexKVAikenHashes` when you want the same hashed key path that the
Aiken-compatible proofs and browser demo use. `fromHexKVHashes` still exists
for direct raw-byte-to-nibble routing.

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

## WASM Outputs With Nix

The flake exports both the combined WASM bundle and the individual modules:

```bash
nix build .#wasm-artifacts
nix build .#csmt-verify-wasm
nix build .#csmt-write-wasm
nix build .#mpf-verify-wasm
nix build .#mpf-write-wasm
```

It also exports runnable local preview commands for the static demo bundles:

```bash
PORT=8000 nix run .#csmt-verify-wasm-demo
PORT=8001 nix run .#csmt-wasm-write-demo
PORT=8002 nix run .#mpf-wasm-write-demo
PORT=8003 nix run .#docs
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

Useful entry points:

- [Getting started](https://lambdasistemi.github.io/haskell-mts/installation/)
- [CLI manual](https://lambdasistemi.github.io/haskell-mts/manual/)
- [CSMT WASM verifier demo](https://lambdasistemi.github.io/haskell-mts/wasm-demo/)
- [CSMT WASM write demo](https://lambdasistemi.github.io/haskell-mts/wasm-write-demo/)
- [MPF WASM write demo](https://lambdasistemi.github.io/haskell-mts/wasm-mpf-demo/)

## License

Apache-2.0

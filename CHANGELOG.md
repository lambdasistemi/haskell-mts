# Changelog for mts

## [0.4.0.0](https://github.com/paolino/haskell-mts/compare/v0.3.2...v0.4.0.0) (2026-03-02)

### Features

* rename package from `csmt` to `mts` (Merkle Tree Store)
* introduce shared `MerkleTreeStore` record with type families (`MTS.Interface`)
* add 12 shared QuickCheck properties (`MTS.Properties`)
* add MPF (Merkle Patricia Forest) 16-ary trie implementation
* MPF batch, chunked, and streaming insertion modes
* MPF inclusion proofs with Aiken-compatible proof steps
* CSMT and MPF both provide `MerkleTreeStore` constructors (`csmtMerkleTreeStore`, `mpfMerkleTreeStore`)
* add completeness proofs to shared MTS interface
* restructure cabal file into `mts` (shared), `mts:csmt`, `mts:mpf` sub-libraries

## [0.3.2](https://github.com/paolino/haskell-mts/compare/v0.3.1...v0.3.2) (2026-02-02)

### Bug Fixes

* correct artifact copy commands for bundler outputs ([140e2eb](https://github.com/paolino/haskell-mts/commit/140e2eb33c6845bf6c0f8009bed7e23b9aa22438))
* handle bundler output directories in release upload ([eb3632e](https://github.com/paolino/haskell-mts/commit/eb3632e7ed908e1282484a67fb08a90afbe373eb))

## [0.3.1](https://github.com/paolino/haskell-mts/compare/v0.3.0...v0.3.1) (2026-02-02)

### Bug Fixes

* add workflow_dispatch to release workflow ([5bacf67](https://github.com/paolino/haskell-mts/commit/5bacf670c5d59a89b1614e70c7d94016baeb6dcf))
* read version from manifest instead of version.txt ([5b3b415](https://github.com/paolino/haskell-mts/commit/5b3b4154f57d0a9286210c0af3b18740b1192af9))

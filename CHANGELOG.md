# Changelog for mts

## [1.0.0](https://github.com/lambdasistemi/haskell-mts/compare/v0.3.2...v1.0.0) (2026-04-23)


### ⚠ BREAKING CHANGES

* update TypeScript verifier for new proof format
* remove proofRootHash from InclusionProof
* MerkleTreeStore now takes a mode type parameter. Downstream consumers add 'Full to their types. csmtMerkleTreeStore and mpfMerkleTreeStore now return IO.

### Features

* add batch and bucketed insertion for parallel CSMT population ([ffa0aa9](https://github.com/lambdasistemi/haskell-mts/commit/ffa0aa96984ea416865c25084734850bf825ff03)), closes [#92](https://github.com/lambdasistemi/haskell-mts/issues/92)
* add CBOR proof serialization, root export, and RocksDB MPF tests ([8d97b9a](https://github.com/lambdasistemi/haskell-mts/commit/8d97b9a3c0304400f0ad150479d7ddb02cb8a5cf))
* add compact CSMT inclusion proof CBOR encoding ([7adbba2](https://github.com/lambdasistemi/haskell-mts/commit/7adbba2d49748f59856d7395a3ddd1436cfe71aa)), closes [#128](https://github.com/lambdasistemi/haskell-mts/issues/128)
* add completeness proofs to shared MTS interface ([828ee3f](https://github.com/lambdasistemi/haskell-mts/commit/828ee3ffb1563e207e6f419f37574e922263ecd6))
* add CSMT pop (minimum extraction) test vectors ([542af6d](https://github.com/lambdasistemi/haskell-mts/commit/542af6d3bfa0b59ce806d27513de5fe728fbb940)), closes [#81](https://github.com/lambdasistemi/haskell-mts/issues/81)
* add CSMT vs MPF benchmark table to docs with compact proofs ([f39763d](https://github.com/lambdasistemi/haskell-mts/commit/f39763d3f90d30601646fca30aec1a674763de4b))
* add csmt-test-vectors executable for Aiken test generation ([1c6ab33](https://github.com/lambdasistemi/haskell-mts/commit/1c6ab33456b326120742d4270c606b70a558b323))
* add csmt-verify sublibrary (pure Haskell, no C FFI) ([3d7ba7a](https://github.com/lambdasistemi/haskell-mts/commit/3d7ba7aa19f7b969cee118790899eb0357e2d8c6))
* add CSMT.Fifo module with counterToKey encoding ([ccf9d49](https://github.com/lambdasistemi/haskell-mts/commit/ccf9d49eec8702bac66094300b17f0427c3edcc1))
* add delete support to patchParallel ([ad5d382](https://github.com/lambdasistemi/haskell-mts/commit/ad5d38299183807949b9d7ea4677292a96caeaea)), closes [#95](https://github.com/lambdasistemi/haskell-mts/issues/95)
* add FIFO test vectors and FifoSpec ([abc7c7d](https://github.com/lambdasistemi/haskell-mts/commit/abc7c7d8bf48f1a22b3fa3ce54be547ede028620))
* add hexTreePrefix to MPF FromHexKV ([f9ab8ae](https://github.com/lambdasistemi/haskell-mts/commit/f9ab8ae126d79c5ac8935038a5ede335f86f0499))
* add keyToByteString inverse of byteStringToKey ([b8d1be5](https://github.com/lambdasistemi/haskell-mts/commit/b8d1be55d6ce27b5cb80f12a8f88b3aae1decfb3))
* add MPF (Merkle Patricia Forest) implementation ([aa60ceb](https://github.com/lambdasistemi/haskell-mts/commit/aa60ceb8fad53e64af2776372a763fd22d3c2164))
* add MPF exclusion proofs with property parity ([2462456](https://github.com/lambdasistemi/haskell-mts/commit/2462456eb9e4f00ce76992c1460f4053e15ad9d4))
* add MPF wasm write and verify demo ([d616ec0](https://github.com/lambdasistemi/haskell-mts/commit/d616ec0970a76c9645b172bfedfc2ab8ad530c3b))
* add MTS constructors for CSMT and MPF ([85c7f11](https://github.com/lambdasistemi/haskell-mts/commit/85c7f11d065d43b0ef3da4fde07107aeacf5b6d4))
* add mts:rollbacks Haskell sub-library skeleton ([1a7396f](https://github.com/lambdasistemi/haskell-mts/commit/1a7396fbc93ff6002b3228cf879a5a19c9dee892))
* add mts.cabal and shared property test ([9773a07](https://github.com/lambdasistemi/haskell-mts/commit/9773a07eb0eea689cdb701f73befdaba9b7081a3))
* add Ops GADT with bidirectional KVOnly/Full transitions ([6875efe](https://github.com/lambdasistemi/haskell-mts/commit/6875efe2f7c91c9ff50c1d4722118b280e9ffeda))
* add persistent database metrics (kvCount, journalSize) ([ab3e20e](https://github.com/lambdasistemi/haskell-mts/commit/ab3e20ecd5a9238b061eee33001cb6586f55c727))
* add Plutus Data encoding for CSMT inclusion proofs ([b8d46ff](https://github.com/lambdasistemi/haskell-mts/commit/b8d46ffa18caf6cdc2cf007564349320c0dea47a))
* add pop_max, push_min, push_max test vectors ([090596f](https://github.com/lambdasistemi/haskell-mts/commit/090596f870f3f71d16370dcb8f5e5d587a79e2d9))
* add populateCSMT for parallel tree construction ([3ec7a5d](https://github.com/lambdasistemi/haskell-mts/commit/3ec7a5d8a37266de8e715b2a81ac694c0542dc75))
* add prefix-scoped CSMT operations for dynamic namespaces ([b7696e8](https://github.com/lambdasistemi/haskell-mts/commit/b7696e8d24a9d5c62efdd653d3031e77fd968116))
* add prefix-scoped MPF operations for dynamic namespaces ([d1a88e6](https://github.com/lambdasistemi/haskell-mts/commit/d1a88e67a32255e920a71daa845c7a5d8cca31b0))
* add pruneExcess to MTS.Rollbacks.Store ([75f3581](https://github.com/lambdasistemi/haskell-mts/commit/75f358157724208c257b7c9909bdbe89eb5ff337))
* add replay tracing callback to patchParallel ([6e1f657](https://github.com/lambdasistemi/haskell-mts/commit/6e1f6577e6f885b83d9d221f534a4e5d1d775301)), closes [#100](https://github.com/lambdasistemi/haskell-mts/issues/100)
* add replay tracing with entries-remaining to all replay entry points ([b90cc34](https://github.com/lambdasistemi/haskell-mts/commit/b90cc34abc6006b04208a04bce232c063639353b)), closes [#99](https://github.com/lambdasistemi/haskell-mts/issues/99)
* add RollbackColumn GADT ([da84fd4](https://github.com/lambdasistemi/haskell-mts/commit/da84fd4196034a1417da5153ad2a1396a3929209))
* add shared MTS interface and QuickCheck properties ([192b51e](https://github.com/lambdasistemi/haskell-mts/commit/192b51e968d7760bbf395c8fd77e29faf290d3d8))
* add single-path MPF insertion (38x faster, 80x smaller DB) ([5b2224b](https://github.com/lambdasistemi/haskell-mts/commit/5b2224b43d9c02db9c58569ad9a1c99c71a4a501))
* add split-mode MTS with KVOnly bootstrap and journal replay ([965db27](https://github.com/lambdasistemi/haskell-mts/commit/965db279deb3943797e6d89e118a9631510de25a))
* add Transaction-level rollback operations ([23e7351](https://github.com/lambdasistemi/haskell-mts/commit/23e7351c5b4c7275224c8dcf6524b3cfe3754a0d))
* add treePrefix field to FromKV for secondary indexing ([8a3d055](https://github.com/lambdasistemi/haskell-mts/commit/8a3d0556d0cbd21469f66569950d346ba6819742))
* add unguarded replay runner to mkKVOnlyOps ([1a56c7f](https://github.com/lambdasistemi/haskell-mts/commit/1a56c7f73cf388adaca6fde8611c2f5965b9e5af)), closes [#109](https://github.com/lambdasistemi/haskell-mts/issues/109)
* add unified CSMT vs MPF benchmark with JS reference ([94cf6d1](https://github.com/lambdasistemi/haskell-mts/commit/94cf6d1177ae814fcb26bab29bf014f534da54ee))
* add WithOrigin and RollbackPoint to Types ([7391914](https://github.com/lambdasistemi/haskell-mts/commit/73919142c8c7fee1dfbd80dd2175d6ae7a20cef5))
* anchor proofs to their root hash ([6b01145](https://github.com/lambdasistemi/haskell-mts/commit/6b011452055b6f0278808d93051b90d2f6f9e977)), closes [#63](https://github.com/lambdasistemi/haskell-mts/issues/63)
* browser demo for in-browser CSMT build + proof ([c5fd6c8](https://github.com/lambdasistemi/haskell-mts/commit/c5fd6c895fa0a2554c371afc329ff24bc372e737))
* build csmt-verify as WASM via GHC WASM backend ([9ca7989](https://github.com/lambdasistemi/haskell-mts/commit/9ca7989073c88a5c750e857a592eab4dc1149455))
* CBOR serialization, fixture generator, and cross-language tests ([18cc635](https://github.com/lambdasistemi/haskell-mts/commit/18cc6355104cff97340d241a4f327e588ab81d35))
* compose docs site with wasm-write demo (flake output) ([0397389](https://github.com/lambdasistemi/haskell-mts/commit/039738981c44afd9c109d72c90e5bb56d3f4f079))
* crash recovery sentinel for bucketed replay ([faf6996](https://github.com/lambdasistemi/haskell-mts/commit/faf699616eb0ca724ae2b4d9aba34013aefacc55)), closes [#107](https://github.com/lambdasistemi/haskell-mts/issues/107)
* enable csmt-write for wasm32-wasi ([fd065fd](https://github.com/lambdasistemi/haskell-mts/commit/fd065fdf91c3076a0f21646bee27659da2ef8592))
* exclusion proofs for CSMT ([63903c2](https://github.com/lambdasistemi/haskell-mts/commit/63903c244bd09f3b0680761cfef6e948e8ce8dc6))
* export packHexKey, nibbleBytes, merkleProof from MPF.Hashes ([7c1f9a8](https://github.com/lambdasistemi/haskell-mts/commit/7c1f9a8623bfe01c3a3e8e0d7d3d5ecd97847eb1))
* expose csmt-test-vectors in nix flake ([3a9df26](https://github.com/lambdasistemi/haskell-mts/commit/3a9df26eac1878e3a7aa92f3c9dea4815d8a9aea))
* formalize swap-partition model in Lean 4 ([5edd966](https://github.com/lambdasistemi/haskell-mts/commit/5edd9660752e75134dbf4f2af290868289a51fae))
* implement completeness proofs with prefix inclusion ([f565576](https://github.com/lambdasistemi/haskell-mts/commit/f565576f4eac3fa79638cdeafea2edb653412479)), closes [#58](https://github.com/lambdasistemi/haskell-mts/issues/58)
* Lean model of bucketed replay and crash recovery ([037c218](https://github.com/lambdasistemi/haskell-mts/commit/037c2186fdbe9ea0ad6288900fc7c921af04f7b2))
* make Ops GADT builders column-parametric ([f14e56a](https://github.com/lambdasistemi/haskell-mts/commit/f14e56a62106a80fa0fa99e51b2a6570ad25c45e))
* persistent rollback point counter ([de886a1](https://github.com/lambdasistemi/haskell-mts/commit/de886a155a6fc54f2dac8c1279158c5ba2d98ec0)), closes [#110](https://github.com/lambdasistemi/haskell-mts/issues/110)
* prove journal/KV/CSMT invariants in Lean 4 ([7f57cd4](https://github.com/lambdasistemi/haskell-mts/commit/7f57cd4928c9655ceb42cab9151c4405e5361f29))
* prove rollback correctness (zero sorry) ([effc260](https://github.com/lambdasistemi/haskell-mts/commit/effc26052eb38913c8a7f9592cb72ae6d9919f16))
* publish csmt-verify WASM browser demo in docs ([bc554c5](https://github.com/lambdasistemi/haskell-mts/commit/bc554c527a1b1787aaec30d5da9f64717a4f4c04))
* remove proofRootHash from InclusionProof ([bda4822](https://github.com/lambdasistemi/haskell-mts/commit/bda482219340becb5c37ebee2e10766aaab70c7f))
* serializable InMemoryDB round-trip for csmt-write-wasm ([bfdbec5](https://github.com/lambdasistemi/haskell-mts/commit/bfdbec5f9f3a4986fdc480311730d63e0c93cd50))
* support populateCSMT on non-empty trees ([701af39](https://github.com/lambdasistemi/haskell-mts/commit/701af393f6ba04c556108a6bbc3edfcd96400a4a))
* three-tag journal with elision for new keys ([0f7fc07](https://github.com/lambdasistemi/haskell-mts/commit/0f7fc07ce34e8f6067f14444a52d372d5f50a4d8))
* TypeScript exclusion proof verifier ([957e7a6](https://github.com/lambdasistemi/haskell-mts/commit/957e7a6eaa1ea040096bde6214aa0865f54fe919))
* update TypeScript verifier for new proof format ([1c0de8a](https://github.com/lambdasistemi/haskell-mts/commit/1c0de8a471fa7a6a95142c3a82983e3897393a9b))
* wire DbState state machine into openOps ([c7bd2c0](https://github.com/lambdasistemi/haskell-mts/commit/c7bd2c06305ed7361d905c0985e7cbabba028caa))


### Bug Fixes

* adapt to rocksdb-kv-transactions snapshot API ([80e41c8](https://github.com/lambdasistemi/haskell-mts/commit/80e41c8be145576d9ddc259b9973d8e193361a4c))
* add missing prefix argument to inserting, deleting, buildInclusionProof, root ([4cd37b8](https://github.com/lambdasistemi/haskell-mts/commit/4cd37b80248b218e8f4c2fda8fc6a817e396a407))
* add pop to CSMT test vectors import ([f0e2d22](https://github.com/lambdasistemi/haskell-mts/commit/f0e2d220bbdd448a3ae2bbae068455c54e446eb5))
* address fourmolu and hlint warnings ([1f26a98](https://github.com/lambdasistemi/haskell-mts/commit/1f26a988d238c530ff49ab4e0abb478117b7ae3d))
* address hlint warning in Aiken parser ([4cd13f8](https://github.com/lambdasistemi/haskell-mts/commit/4cd13f802ccafcedc1d3c0944fd20f26eca21564))
* apply hlint suggestion in namespace test ([253ca2e](https://github.com/lambdasistemi/haskell-mts/commit/253ca2e7f0736c1740303162480d0fc9a77a9f68))
* apply hlint suggestion in PlutusData parser ([fd7a20d](https://github.com/lambdasistemi/haskell-mts/commit/fd7a20d9112599ee99b0056ccb6388c6c230ba74))
* CI failures and hlint warnings ([c2a8ddb](https://github.com/lambdasistemi/haskell-mts/commit/c2a8ddb1d20ec90a3c67701a7f3c958b6e979217))
* clean stale nodes in mergeSubtreeRoots + full QC coverage ([8962df5](https://github.com/lambdasistemi/haskell-mts/commit/8962df5bf3efd49c2e3d768e485fc477913613c5))
* correct docs URL in README to lambdasistemi.github.io ([3f2f9ec](https://github.com/lambdasistemi/haskell-mts/commit/3f2f9eceb5cc65f4601f7b9a4d210495b1cbd459))
* eta reduce storeRollbackPoint per hlint ([5c66021](https://github.com/lambdasistemi/haskell-mts/commit/5c6602136c7d3fc96945b2c976e94580f028e2b5))
* exclude storage prefix from pslNeighborKeyPath in proofs ([ff5d814](https://github.com/lambdasistemi/haskell-mts/commit/ff5d814844af5412ffbfeeb045a007042d7eea9c)), closes [#87](https://github.com/lambdasistemi/haskell-mts/issues/87)
* expose transactional MTS constructors for composable operations ([2289bac](https://github.com/lambdasistemi/haskell-mts/commit/2289bacbf0c327183a2c9911b1eeede6b511e1e6)), closes [#66](https://github.com/lambdasistemi/haskell-mts/issues/66)
* format mts.cabal with cabal-fmt ([d0a2400](https://github.com/lambdasistemi/haskell-mts/commit/d0a2400f6fa21db5db4039a293340f954e367814))
* generalize cf/op phantoms in transactional MTS constructors ([106356f](https://github.com/lambdasistemi/haskell-mts/commit/106356f1e2db5d76a893da5e20b6e58ab313b939))
* generate Leaf/Fork proof steps matching Aiken format ([a610227](https://github.com/lambdasistemi/haskell-mts/commit/a610227e01c730b98517ce8c4950f83cc5f04859))
* handle prefix queries with path compression in collectValues and generateProof ([a93c00f](https://github.com/lambdasistemi/haskell-mts/commit/a93c00ff8f16dc97353c12991439a89171226412))
* hash MPF browser-demo keys on the Aiken path ([cf9354d](https://github.com/lambdasistemi/haskell-mts/commit/cf9354dbcf42deb085ecdc2c9e15e840c3253626))
* hlint redundant bracket + metrics QC properties ([4f264a8](https://github.com/lambdasistemi/haskell-mts/commit/4f264a864ae5c3e2cbda6c86454ea842456a905a)), closes [#105](https://github.com/lambdasistemi/haskell-mts/issues/105)
* maintain journalSizeKey counter in mkKVOnlyOps ([011f0ad](https://github.com/lambdasistemi/haskell-mts/commit/011f0adecc8f0d2b30080c8d9ca8590466de3f40)), closes [#137](https://github.com/lambdasistemi/haskell-mts/issues/137)
* match upstream Aiken fork-prefix encoding ([ace7bea](https://github.com/lambdasistemi/haskell-mts/commit/ace7bea956fb910b23e5977be6efc7f0263b3370))
* migrate mkdocs gh-deploy to mkdocs-deploy wrapper ([4a294e6](https://github.com/lambdasistemi/haskell-mts/commit/4a294e6169d5eab7674de85431c629e6bbd8e584)), closes [#89](https://github.com/lambdasistemi/haskell-mts/issues/89)
* remove unused sparse-merkle-trees dependency ([1408660](https://github.com/lambdasistemi/haskell-mts/commit/14086605f157b8e9c1dd5f54dff1ac5f37734422)), closes [#117](https://github.com/lambdasistemi/haskell-mts/issues/117)
* replace partial head with pattern match, fix CI format check ([7d5e6a6](https://github.com/lambdasistemi/haskell-mts/commit/7d5e6a62b81d1729b43644ed7e17ed909e96f714))
* replace stale haskell-csmt references with haskell-mts ([29c04ba](https://github.com/lambdasistemi/haskell-mts/commit/29c04bacfb9f6e4f63151f9a5d1ccbe21e66835c)), closes [#57](https://github.com/lambdasistemi/haskell-mts/issues/57)
* resolve all Lean linter warnings, make warnings errors in CI ([b3ecb3a](https://github.com/lambdasistemi/haskell-mts/commit/b3ecb3ab1082e1a876e6443c0270c24da69d6025))
* resolve CI format and hlint issues ([a935ef1](https://github.com/lambdasistemi/haskell-mts/commit/a935ef12a02745bbfcb9e96c896acda617e7e28f))
* resolve formatting and hlint warnings ([d7c3e72](https://github.com/lambdasistemi/haskell-mts/commit/d7c3e7229205ceb28bf11f5ca3e8477d3363877c))
* revert to bare cachix commands (requires infra PR [#58](https://github.com/lambdasistemi/haskell-mts/issues/58)) ([c763fd9](https://github.com/lambdasistemi/haskell-mts/commit/c763fd953cd0b378f977584650f8ad1c6a77563f))
* sanitize nix store paths in asciinema cast files ([e46ef73](https://github.com/lambdasistemi/haskell-mts/commit/e46ef731076cf0884d5ed0f047d0dd74e622aea8))
* use cachix-action instead of bare cachix command ([2d8d91d](https://github.com/lambdasistemi/haskell-mts/commit/2d8d91d137900cca65401edba09580d5d88112b1))
* use cachix-action with cachix from extraPackages ([857532c](https://github.com/lambdasistemi/haskell-mts/commit/857532ca402a4b8274850e31afe77bf8f2398f79))
* use format-check in CI and add hlint job ([c9436d7](https://github.com/lambdasistemi/haskell-mts/commit/c9436d703f7664ba9d754a8eda499550b5c2d3f3))
* use nix shell for elan in Lean CI job ([5538956](https://github.com/lambdasistemi/haskell-mts/commit/55389561b5f2ccc5687fe3cd4f5de5f607784852))
* write sentinel atomically with expandToBucketDepth ([7ecacc2](https://github.com/lambdasistemi/haskell-mts/commit/7ecacc2b1e2b403d43defe6555ebc4dd9a13f5fa))

## [0.4.0.0](https://github.com/lambdasistemi/haskell-mts/compare/v0.3.2...v0.4.0.0) (2026-03-02)

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

## [0.3.2](https://github.com/lambdasistemi/haskell-mts/compare/v0.3.1...v0.3.2) (2026-02-02)

### Bug Fixes

* correct artifact copy commands for bundler outputs ([140e2eb](https://github.com/lambdasistemi/haskell-mts/commit/140e2eb33c6845bf6c0f8009bed7e23b9aa22438))
* handle bundler output directories in release upload ([eb3632e](https://github.com/lambdasistemi/haskell-mts/commit/eb3632e7ed908e1282484a67fb08a90afbe373eb))

## [0.3.1](https://github.com/lambdasistemi/haskell-mts/compare/v0.3.0...v0.3.1) (2026-02-02)

### Bug Fixes

* add workflow_dispatch to release workflow ([5bacf67](https://github.com/lambdasistemi/haskell-mts/commit/5bacf670c5d59a89b1614e70c7d94016baeb6dcf))
* read version from manifest instead of version.txt ([5b3b415](https://github.com/lambdasistemi/haskell-mts/commit/5b3b4154f57d0a9286210c0af3b18740b1192af9))

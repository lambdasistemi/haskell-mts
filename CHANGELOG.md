# Changelog for csmt-utxo

## 1.0.0 (2026-02-02)


### Features

* add `w` command to cli to query the value at a key ([7af6b45](https://github.com/paolino/haskell-csmt/commit/7af6b45fbdc15c17b744d1c401888759f7c4e5b4))
* add application that runs insertion qurey proof and verification interactively ([2d0e3f0](https://github.com/paolino/haskell-csmt/commit/2d0e3f0e19dba3b74d51ca1c9473a8b2a24bcf99))
* add compressed stm insertion ([93407b2](https://github.com/paolino/haskell-csmt/commit/93407b2c8a8405b1da4088b5ff67623b05d44b78))
* add db filepath option to application ([7099416](https://github.com/paolino/haskell-csmt/commit/7099416d557d68a721e7cbf56f3a61d3cd7b51d6))
* add keccak hashes ([91992f1](https://github.com/paolino/haskell-csmt/commit/91992f1618f28feb2018b0d3163a33634aa889b3))
* add proof of completeness for a CSMT node implementation ([e94df89](https://github.com/paolino/haskell-csmt/commit/e94df895ffbcfbc15f73b10987bc793f6ef4c8b0))
* add proof of inclusion for compressed smt ([8c60781](https://github.com/paolino/haskell-csmt/commit/8c60781ce74d30ff3a2045d26edcfff94768d9c4))
* add release pipeline with release-please ([45b4439](https://github.com/paolino/haskell-csmt/commit/45b4439b5148db2a4a11f48ef31ce259d3d7cf04))
* add transaction lib for rocksdb ([51e4c5e](https://github.com/paolino/haskell-csmt/commit/51e4c5e7d6561e0d5a3a94e4b6d9e92bb9b46937))
* add transaction reset instruction ([69cf9c6](https://github.com/paolino/haskell-csmt/commit/69cf9c663c889800e44153f52ad3fc4b7dbafb8f))
* add transaction support ([ca017e7](https://github.com/paolino/haskell-csmt/commit/ca017e7cdc3aa254473062badb19504d0f07eee3))
* add TypeScript library for CSMT proof verification ([133eb6b](https://github.com/paolino/haskell-csmt/commit/133eb6be8d175fc2b0efc5412a7c92ee675d655b)), closes [#19](https://github.com/paolino/haskell-csmt/issues/19)
* add uncompressed pure smt insertion and proof ([4b42d90](https://github.com/paolino/haskell-csmt/commit/4b42d90cef6f2ba1765af6a1f01ce8618331a7eb))
* allow plain keys ([c5a13ff](https://github.com/paolino/haskell-csmt/commit/c5a13ffcf55ccc3a5694139c1383103956c9414f))
* allow polymorphic cursor seek key ([799f8a1](https://github.com/paolino/haskell-csmt/commit/799f8a18c3dc35523a11b9c46405910907e7c2c5))
* auto-sync cabal version on release PRs ([89f242b](https://github.com/paolino/haskell-csmt/commit/89f242b1d39f7a710d39a001e85e8737c994889e))
* encode rockdbkeys (ByteString) from and to csmt nodes ([4d7e0e1](https://github.com/paolino/haskell-csmt/commit/4d7e0e1c193182626441b3883a3cbfff9b6401b9))
* export a querying DSL outside of transactions ([69b8174](https://github.com/paolino/haskell-csmt/commit/69b8174c63719551e4a16b5d38c568d6b8c7ef70))
* implement facts persistance ([6ab7ab2](https://github.com/paolino/haskell-csmt/commit/6ab7ab29d8a36f114f03db4dc6993369fecd5341))
* implement RocksDB backend ([6135afc](https://github.com/paolino/haskell-csmt/commit/6135afc8df36e54f07735a7babb889a1cf33be94))
* let rocksdb-transactions be public ([c064450](https://github.com/paolino/haskell-csmt/commit/c064450de0fa3d59822125722ff0f28771fa1984))
* limit rocksdb open files ([a5922f1](https://github.com/paolino/haskell-csmt/commit/a5922f1b739ed0a08b4df9c3cb106c8773c2b9a0))
* move mkRocksDBDatabase to a separate package ([770752a](https://github.com/paolino/haskell-csmt/commit/770752aaee4077de7c4b6126aadc89ac4b8c2546))
* remove rocksdb direct support from kv-transactions subpackage ([6928c4c](https://github.com/paolino/haskell-csmt/commit/6928c4c657acad027597eba6ba5ed57d39273048))
* rename project to csmt ([0bb85db](https://github.com/paolino/haskell-csmt/commit/0bb85dbfd2237974ff39bf224f9b697cdd023b65))
* support cursors on rocksdb ([0d5ea88](https://github.com/paolino/haskell-csmt/commit/0d5ea88b347b74b5cebcaf2b33703104bee9dd58))
* support fact deletion ([c6356ec](https://github.com/paolino/haskell-csmt/commit/c6356ecf30adaed219612d459817dc0f37e5752b))
* switch off bloom filters ([6fe6527](https://github.com/paolino/haskell-csmt/commit/6fe652796720a01b32ff7bf6f13b8a3dacc753cc))
* synchronize transactions ([f9c8bbd](https://github.com/paolino/haskell-csmt/commit/f9c8bbd4e6ce40c28fa178f00f0dd00ab17fd5bb))
* unlink monads parameters in newRunTransaction ([455153c](https://github.com/paolino/haskell-csmt/commit/455153cb6cf9fe1732bef3885d590873c09abd63))


### Bug Fixes

* add autogen-modules for Paths_csmt ([928ea61](https://github.com/paolino/haskell-csmt/commit/928ea617263f18f0f68703fd494d980a9b797c30)), closes [#21](https://github.com/paolino/haskell-csmt/issues/21)
* change app cli options ([6903d16](https://github.com/paolino/haskell-csmt/commit/6903d1650233fc16bff6fb050afe4af60738b2a4))
* fix just unit command ([79f6090](https://github.com/paolino/haskell-csmt/commit/79f609007f3ed729e4d817ae428ef02564177a7f))
* keep 0.x versioning in release-please ([02e5591](https://github.com/paolino/haskell-csmt/commit/02e55915a4a554b4cfc80887d8d7a198706a8bde))
* relax QuickCheck bounds for Cardano compatibility ([9686c37](https://github.com/paolino/haskell-csmt/commit/9686c370c62f8d4b861079dbac74b37a9fa604b7))
* unit tests ([8ab5252](https://github.com/paolino/haskell-csmt/commit/8ab52526095db5948c19ef9821582911785bf406))
* update benchmarks ([634b756](https://github.com/paolino/haskell-csmt/commit/634b75618d8fddf4e77c47889ac752a0de8f9d4c))
* update repository URLs from csmt to haskell-csmt ([b15713b](https://github.com/paolino/haskell-csmt/commit/b15713bb52518a753786cd5c310e13ab066bfbfd))

## 0.1.0.0 (2025-10-12)
- end-point to list containers
- end-point to report all logs of a container (non-streaming)

## 0.2.0.0 (2025-10-13)
- streaming logs of a container
- proxy the full [docker logs API](https://docs.docker.com/reference/api/engine/version/v1.41/#tag/Container/operation/ContainerLogs)

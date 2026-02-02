# Changelog for csmt-utxo

## [0.3.1](https://github.com/paolino/haskell-csmt/compare/v0.3.0...v0.3.1) (2026-02-02)


### Bug Fixes

* add workflow_dispatch to release workflow ([5bacf67](https://github.com/paolino/haskell-csmt/commit/5bacf670c5d59a89b1614e70c7d94016baeb6dcf))
* read version from manifest instead of version.txt ([5b3b415](https://github.com/paolino/haskell-csmt/commit/5b3b4154f57d0a9286210c0af3b18740b1192af9))

## 0.1.0.0 (2025-10-12)
- end-point to list containers
- end-point to report all logs of a container (non-streaming)

## 0.2.0.0 (2025-10-13)
- streaming logs of a container
- proxy the full [docker logs API](https://docs.docker.com/reference/api/engine/version/v1.41/#tag/Container/operation/ContainerLogs)

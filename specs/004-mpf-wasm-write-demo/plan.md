# Implementation Plan: MPF WASM Write Path and Browser Demo

**Branch**: `feat/port-mpf-write-path-to-wasm-with-browser-demo-mirr` | **Date**: 2026-04-22 | **Spec**: [spec.md](spec.md)

## Summary

Mirror the completed CSMT write-path work from `feat/wasm-write-path`
onto MPF without changing MPF semantics. The critical first gate is a
hash audit plus a QuickCheck parity property proving the pure Blake2b
path is byte-identical to the current `crypton` route. Once that holds,
split the pure MPF modules into `mpf-write`, add `mpf-write-wasm`, and
mirror the browser/docs plumbing from the CSMT demo.

## Technical Context

**Language/Version**: Haskell 2010 on GHC 9.10.x via Cabal/Nix  
**Primary Dependencies**: `bytestring`, `cereal`, `containers`,
`lens`, `kv-transactions`, `cborg`, pure Blake2b from `mts:csmt-verify`
or equivalent  
**Storage**: In-memory serialized blob for WASM/browser; RocksDB for the
native `mpf` backend  
**Testing**: Hspec + QuickCheck, plus targeted wasm/browser build checks  
**Target Platform**: Native Linux/macOS and `wasm32-wasi`  
**Project Type**: Multi-library Haskell package with wasm executable and
static browser demo  
**Performance Goals**: Preserve current MPF root/proof behavior; browser
flow stays responsive for demo-sized workloads  
**Constraints**: No breaking change for `mts:mpf`; no RocksDB/native
deps inside `mpf-write`; PR should stay reviewable against
`feat/wasm-write-path`  
**Scale/Scope**: New sublibrary, one wasm executable, one browser demo,
docs/flake/nix wiring, and one downstream compatibility check

## Constitution Check

- **Shared Interface, Pluggable Implementations**: Pass. `mts:mpf`
  remains the downstream-facing interface; `mpf-write` only refactors
  packaging to isolate the pure implementation.
- **Property-Driven Correctness**: Pass only after adding the MPF hash
  parity property and keeping the existing shared property suite green.
- **Formal Verification of Invariants**: No new invariant is introduced.
  This ticket preserves existing MPF semantics and adds portability;
  QuickCheck parity plus existing proof properties are the right gate.
- **Hackage-Ready Quality**: New exported modules and executables must be
  wired cleanly in `mts.cabal`, formatted with Fourmolu, and documented.
- **Reproducible Builds via Nix**: Required. `cabal-wasm.project`,
  `nix/wasm.nix`, docs staging, and flake outputs all need updates.
- **Observability and Tracing**: Not central to this ticket.

## Project Structure

```text
specs/004-mpf-wasm-write-demo/
├── spec.md
├── plan.md
└── tasks.md

lib/mpf-write/
└── MPF/...

lib/mpf/
├── MPF.hs
└── MPF/Backend/RocksDB.hs

app/mpf-write-wasm/
└── Main.hs

test/MPF/
├── Blake2bSpec.hs
└── ...

verifiers/browser-write-mpf/
├── index.html
└── write.js

nix/
├── wasm.nix
├── docs.nix
└── mpf-wasm-write-demo.nix
```

**Structure Decision**: Reuse the CSMT split: move pure MPF modules to a
new `lib/mpf-write/` tree, keep native-only pieces under `lib/mpf/`,
and mirror the CSMT wasm/browser/docs files with MPF-specific logic.

## Execution Phases

### Phase 0: Spec and PR framing

Create this spec set, push the issue branch, and open a PR against
`feat/wasm-write-path` so the MPF work can proceed in parallel with
PR #145 without dragging CSMT commits into the review.

### Phase 1: Hash audit and parity proof

Inspect every MPF write-path use of hashing. Replace
`MPF.Hashes.mkMPFHash` with the pure Blake2b implementation, then add a
QuickCheck property that builds identical trees through both hash paths
and asserts identical root bytes.

### Phase 2: Pure-library split

Move pure modules from `lib/mpf/` into `lib/mpf-write/`, add
`library mpf-write` to `mts.cabal`, and convert `library mpf` into a
native-only shim that re-exports the write library plus RocksDB.

### Phase 3: WASM executable and state blob

Create `app/mpf-write-wasm/Main.hs` by mirroring the CSMT wire protocol
and adapting it to MPF insert/delete/proof generation. Add MPF
in-memory DB serialization so the browser can persist and restore state.

### Phase 4: Browser demo and docs

Create `verifiers/browser-write-mpf/`, stage it through nix/docs/flake,
and make the preview deployment publish the MPF demo next to the CSMT
and verifier demos.

### Phase 5: Verification

Run focused tests for the new MPF hashing/property work, build
`mpf-write-wasm`, validate docs build wiring, and build the downstream
consumer against `mts:mpf`.

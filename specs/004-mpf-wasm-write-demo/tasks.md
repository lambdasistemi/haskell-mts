# Tasks: MPF WASM Write Path and Browser Demo

**Branch**: `feat/port-mpf-write-path-to-wasm-with-browser-demo-mirr` | **Plan**: [plan.md](plan.md)

## Phase 1: Spec and PR Setup

- [ ] **T1.1** Create `specs/004-mpf-wasm-write-demo/spec.md`,
  `plan.md`, and `tasks.md`
- [ ] **T1.2** Push the issue branch and open a PR against
  `feat/wasm-write-path`

## Phase 2: Hash Audit and Parity Gate

- [ ] **T2.1** Audit `lib/mpf/` for all `crypton`/`Crypto.Hash` call
  sites and confirm the write path only depends on Blake2b-256
- [ ] **T2.2** Replace `MPF.Hashes.mkMPFHash` with the pure Blake2b path
  and adjust `mts.cabal` dependencies
- [ ] **T2.3** Add `test/MPF/Blake2bSpec.hs` proving MPF roots are
  byte-identical between `crypton` and the pure Blake2b route
- [ ] **T2.4** Run the focused MPF hash/property tests and keep them
  green before proceeding

## Phase 3: `mpf-write` Sublibrary Split

- [ ] **T3.1** Move pure MPF modules from `lib/mpf/` to
  `lib/mpf-write/`
- [ ] **T3.2** Add `library mpf-write` to [mts.cabal](/code/haskell-mts-issue-146/mts.cabal)
- [ ] **T3.3** Convert `library mpf` into a re-export shim plus
  native-only modules
- [ ] **T3.4** Update test-library and executable dependencies to use
  `mts:mpf-write` where appropriate

## Phase 4: WASM Write Executable

- [ ] **T4.1** Create `app/mpf-write-wasm/Main.hs` mirroring the
  CSMT opcode-tagged protocol for insert/delete/query
- [ ] **T4.2** Add MPF in-memory DB serialization and round-trip support
- [ ] **T4.3** Wire `mpf-write-wasm` into `mts.cabal`,
  `cabal-wasm.project`, `flake.nix`, and `nix/wasm.nix`
- [ ] **T4.4** Build `mpf-write-wasm` for `wasm32-wasi`

## Phase 5: Browser Demo and Docs

- [ ] **T5.1** Create `verifiers/browser-write-mpf/index.html` and
  `write.js` by adapting the CSMT browser demo to MPF
- [ ] **T5.2** Add IndexedDB persistence, root/proof display, and
  undo/redo to the MPF demo
- [ ] **T5.3** Stage the demo via `nix/mpf-wasm-write-demo.nix`,
  `nix/docs.nix`, and `mkdocs.yml`
- [ ] **T5.4** Ensure preview deployment publishes the MPF demo

## Phase 6: Downstream Compatibility and Final Verification

- [ ] **T6.1** Build the relevant local test targets for MPF and the wasm
  executable
- [ ] **T6.2** Build the downstream MPFS off-chain consumer against the
  updated `mts:mpf`
- [ ] **T6.3** Update PR description with the final tour of changes and
  verification results

## Dependencies

- T1.1 precedes all implementation work.
- T1.2 should happen before code changes that need review context.
- T2.4 blocks T3.x through T5.x.
- T3.x blocks T4.x because the wasm executable must target `mpf-write`.
- T4.x blocks T5.x because the browser demo needs the final wasm entry
  point and wire protocol.
- T6.x is the final gate before merge.

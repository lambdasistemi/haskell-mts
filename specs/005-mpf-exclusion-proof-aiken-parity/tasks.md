# Tasks: MPF Exclusion Proofs with Aiken Parity

**Input**: Design documents from
`/specs/005-mpf-exclusion-proof-aiken-parity/`
**Prerequisites**: `plan.md`, `spec.md`

## Phase 1: Core Proof API

- [x] T001 Create `lib/mpf-write/MPF/Proof/Exclusion.hs` with
  `MPFExclusionProof`, `mkMPFExclusionProof`, `foldMPFExclusionProof`,
  and `verifyMPFExclusionProof`.
- [x] T002 Expose `MPF.Proof.Exclusion` through `mts.cabal` and the `MPF`
  re-export surface.
- [x] T003 Extend `MPF.Test.Lib` with helpers for building and verifying
  exclusion proofs in the pure backend.

## Phase 2: Focused Tests

- [x] T004 Add `test/MPF/Proof/ExclusionSpec.hs`.
- [x] T005 Cover empty-tree exclusion, present-key rejection, root/leaf
  divergence, branch missing-child exclusion, and tamper rejection.
- [x] T006 Keep the current inclusion Aiken proof-step tests green.

## Phase 3: Follow-on Integration

- [ ] T007 Widen the MTS/browser-facing proof API so MPF exclusion proofs
  can flow into the `ptype = 1` WASM protocol from `#146`.
- [x] T008 Add explicit upstream JS/Aiken exclusion vectors once the core
  proof shape is stable.

## Verification

- [x] T009 Run focused MPF unit tests
- [x] T010 Run formatting on touched files

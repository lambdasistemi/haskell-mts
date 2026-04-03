# Tasks: Exclusion Proof for CSMT

**Input**: Design documents from `/specs/002-exclusion-proof/`
**Prerequisites**: plan.md, spec.md

## Phase 1: Generalize proof fold (Foundational)

**Purpose**: Extract the proof step fold into a callback-based
function that both inclusion and exclusion verification can use.

- [ ] T001 [US1] Generalize `computeRootHash` into `foldProof`
  with callback accumulator in
  `lib/csmt/CSMT/Proof/Insertion.hs`. Rewrite
  `computeRootHash` and `verifyInclusionProof` in terms of
  `foldProof`. Export `foldProof` and `StepView`.
- [ ] T002 [US1] Verify existing inclusion proof tests still pass
  after the refactor (`cabal test unit-tests -O0
  --test-option='--match=/CSMT.Proof.Insertion/'`)

**Checkpoint**: Existing inclusion proofs work unchanged,
`foldProof` is available for exclusion.

---

## Phase 2: User Story 1 — Prove key absence (Priority: P1)

**Goal**: Generate and verify exclusion proofs for absent keys.

**Independent Test**: Insert known keys, generate exclusion proof
for an absent key, verify it passes.

### Implementation

- [ ] T003 [US1] Create `lib/csmt/CSMT/Proof/Exclusion.hs` with
  `ExclusionProof` type (two variants: `ExclusionEmpty`,
  `ExclusionWitness` with target key + witness inclusion proof).
- [ ] T004 [US1] Implement `verifyExclusionProof` using
  `foldProof` with a divergence callback that checks target
  key vs witness key at each step, confirming divergence is
  within a jump (not at a branch boundary). Handle root jump
  divergence before the step fold.
- [ ] T005 [US1] Implement `buildExclusionProof` — walk tree
  following target key, find divergence point, descend to a
  leaf witness, generate witness inclusion proof via existing
  `buildInclusionProof`.
- [ ] T006 [US1] Add `CSMT.Proof.Exclusion` and
  `CSMT.Proof.ExclusionSpec` to `mts.cabal`.

### Tests

- [ ] T007 [US1] Create `test/CSMT/Proof/ExclusionSpec.hs`:
  - Unit: empty tree exclusion
  - Unit: single-element tree, absent key
  - Unit: multi-element tree, absent key
  - Unit: present key returns Nothing
  - QuickCheck: random tree + random absent key → generate
    succeeds, verify passes
  - QuickCheck: random tree + random present key → generate
    returns Nothing

**Checkpoint**: Exclusion proofs generate and verify for all
cases. All existing tests pass.

---

## Phase 3: User Story 2 — Empty tree edge case (Priority: P2)

**Goal**: Handle empty tree exclusion cleanly.

- [ ] T008 [US2] Add `ExclusionEmpty` verification: check root
  hash is empty-tree hash (no root node exists).
- [ ] T009 [US2] Unit test: `ExclusionEmpty` verifies against
  empty root, fails against non-empty root.

**Checkpoint**: Empty tree case covered.

---

## Phase 4: User Story 3 — Tamper detection (Priority: P2)

**Goal**: Verify tampered exclusion proofs are rejected.

- [ ] T010 [US3] QuickCheck property: flip a byte in the witness
  proof → verification fails.
- [ ] T011 [US3] QuickCheck property: swap the target key →
  verification fails.
- [ ] T012 [US3] Unit test: modify a sibling hash in the witness
  proof → verification fails.

**Checkpoint**: Tampered proofs reliably rejected.

---

## Phase 5: Polish

- [ ] T013 Haddock on all exports in `Exclusion.hs`
- [ ] T014 Run `just ci` — format, hlint, cabal check, full
  test suite

---

## Dependencies & Execution Order

- **Phase 1** (T001–T002): Must complete first — `foldProof`
  is the foundation.
- **Phase 2** (T003–T007): Depends on Phase 1. T003 and T004
  can be done together. T005 depends on T003. T006–T007
  depend on T003–T005.
- **Phase 3** (T008–T009): Can run in parallel with Phase 2
  but logically part of T004's implementation.
- **Phase 4** (T010–T012): Depends on Phase 2.
- **Phase 5** (T013–T014): After all phases.

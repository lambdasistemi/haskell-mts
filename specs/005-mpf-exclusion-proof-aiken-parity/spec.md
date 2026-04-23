# Feature Specification: MPF Exclusion Proofs with Aiken Parity

**Feature Branch**: `feat/mpf-exclusion-proof-aiken-parity`
**Created**: 2026-04-22
**Status**: Draft
**Input**: User description: "Implement
`lambdasistemi/haskell-mts#149` on top of the `mpf-write` layout from
PR `#147`."

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Prove an absent MPF key (Priority: P1)

A library consumer wants to prove that a key is absent from an MPF trie
without inventing a second proof-step wire format. They request an
exclusion proof for an absent key and verify it against a trusted root.

**Why this priority**: This is the blocker for `#146`'s `ptype = 1`
browser/WASM path.

**Independent Test**: Insert known keys, generate an exclusion proof for
an absent key, and verify it against the current root.

**Acceptance Scenarios**:

1. **Given** a populated trie and an absent key, **When** exclusion proof
   generation runs, **Then** it returns a proof instead of `Nothing`.
2. **Given** a valid exclusion proof and trusted root, **When**
   verification runs, **Then** it succeeds without needing tree access.
3. **Given** a valid exclusion proof, **When** its proof steps are
   serialized with the existing Aiken proof-step codec, **Then** the
   bytes remain parseable by the current step parser.

---

### User Story 2 - Handle empty trees and present keys correctly (Priority: P2)

A library consumer needs clean behavior at the two boundary cases:
empty trees and keys that are actually present.

**Why this priority**: These are the easiest ways to return a proof that
lies.

**Independent Test**: Generate proof on an empty trie and on a present
key.

**Acceptance Scenarios**:

1. **Given** an empty trie, **When** exclusion proof generation runs,
   **Then** it returns an explicit empty-tree proof.
2. **Given** an empty-tree proof, **When** verification runs against an
   empty root, **Then** it succeeds.
3. **Given** a present key, **When** exclusion proof generation runs,
   **Then** it returns `Nothing`.

---

### User Story 3 - Reject tampered exclusion proofs (Priority: P2)

A verifier receives an exclusion proof whose target key or proof steps
have been modified.

**Why this priority**: The proof is only useful if tampering is
detectable.

**Independent Test**: Generate a valid proof, modify the target key or a
proof step, and ensure verification fails.

**Acceptance Scenarios**:

1. **Given** a valid exclusion proof, **When** the target key is swapped,
   **Then** verification fails.
2. **Given** a valid exclusion proof, **When** a proof step is modified,
   **Then** verification fails.

## Edge Cases

- Empty trie
- Divergence inside the root jump
- Missing child at a branch with multiple siblings
- Divergence inside a compressed child jump
- Terminal witness leaf
- Terminal witness branch

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The MPF library MUST provide a first-class exclusion-proof
  API for absent keys.
- **FR-002**: The exclusion-proof API MUST reuse the existing
  `MPFProofStep` structure rather than inventing a second step encoding.
- **FR-003**: The exclusion verifier MUST be pure and operate on trusted
  root plus proof data only.
- **FR-004**: The exclusion verifier MUST reject proofs whose path does
  not match the claimed target key.
- **FR-005**: Exclusion proof generation MUST return `Nothing` for
  present keys.
- **FR-006**: Empty trees MUST have an explicit exclusion-proof variant.
- **FR-007**: The existing inclusion Aiken proof-step rendering MUST stay
  unchanged for inclusion proofs.
- **FR-008**: The resulting proof steps MUST remain serializable through
  the current Aiken step codec so the `ptype = 1` browser/WASM envelope
  can keep carrying step bytes out-of-band.

### Key Entities

- **MPFExclusionProof**: Proof that a target key is absent. It is either
  an empty-tree witness or a populated proof carrying the target key plus
  shared `MPFProofStep` data.
- **Shared proof steps**: Existing `ProofStepBranch`, `ProofStepFork`,
  and `ProofStepLeaf` constructors reused for exclusion mode.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Absent-key exclusion proofs verify successfully against the
  current root.
- **SC-002**: Present keys do not produce exclusion proofs.
- **SC-003**: Empty-tree exclusion proofs verify only against an empty
  root.
- **SC-004**: Tampered target keys or tampered proof steps are rejected.
- **SC-005**: Existing inclusion Aiken proof-step tests remain green
  unchanged.

## Assumptions

- The first implementation slice may stop at the pure MPF proof API and
  direct tests, before widening `MTS.Interface` and the browser protocol.
- The current work targets the fixed-path `mpf-write` code layout from PR
  `#147`.

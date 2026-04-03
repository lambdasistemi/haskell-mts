# Feature Specification: Exclusion Proof for CSMT

**Feature Branch**: `002-exclusion-proof`
**Created**: 2026-03-28
**Status**: Draft
**Input**: User description: "Implement proof of exclusion for CSMT (lambdasistemi/haskell-mts#2)"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Prove key absence in a populated tree (Priority: P1)

A library consumer wants to prove that a specific key does not exist in the Merkle tree. They call `generateExclusionProof` with the target key, receive a proof object, and can later verify it (or transmit it to a third party for verification) using `verifyExclusionProof`.

**Why this priority**: This is the core feature — without it, exclusion proofs don't exist.

**Independent Test**: Can be tested by inserting known keys, requesting an exclusion proof for an absent key, and verifying the proof succeeds.

**Acceptance Scenarios**:

1. **Given** a tree with keys {A, B, C}, **When** an exclusion proof is generated for absent key D, **Then** the proof is returned successfully and passes verification against the current root hash.
2. **Given** a tree with keys {A, B, C}, **When** an exclusion proof is generated for present key A, **Then** the generation fails (returns Nothing) since the key is included.
3. **Given** a valid exclusion proof, **When** a third party verifies it with the correct root hash, **Then** verification succeeds without needing tree access.

---

### User Story 2 - Prove key absence in an empty tree (Priority: P2)

A library consumer wants to prove that a key does not exist in an empty tree. The proof is trivial: an empty tree contains nothing.

**Why this priority**: Edge case that must be handled cleanly.

**Independent Test**: Can be tested by generating an exclusion proof against an empty tree and verifying it.

**Acceptance Scenarios**:

1. **Given** an empty tree, **When** an exclusion proof is generated for any key, **Then** the proof is returned as a trivial "empty tree" proof and passes verification.
2. **Given** an empty-tree exclusion proof, **When** verified against a non-empty root hash, **Then** verification fails.

---

### User Story 3 - Verify tampered exclusion proof (Priority: P2)

A verifier receives an exclusion proof that has been tampered with. Verification must reject it.

**Why this priority**: Security property — proofs must be unforgeable.

**Independent Test**: Can be tested by generating a valid proof, modifying a field, and checking verification fails.

**Acceptance Scenarios**:

1. **Given** a valid exclusion proof, **When** the target key in the proof is altered, **Then** verification fails.
2. **Given** a valid exclusion proof, **When** a step in the embedded inclusion proof is altered, **Then** verification fails.

---

### Edge Cases

- Empty tree: trivially excluded, no witness needed
- Key diverges at the root node's jump (witness is the root)
- Tree has no child in the required direction at a branch point (the branch absence is the witness)
- Target key is a prefix of an existing key's path, or vice versa
- Single-element tree: the lone element serves as witness for all other keys

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: Library MUST provide `generateExclusionProof` that, given a target key and tree access, returns a proof of the key's absence or Nothing if the key exists
- **FR-002**: Library MUST provide `verifyExclusionProof` that, given hashing functions and an exclusion proof, returns whether the proof is valid — without requiring tree access
- **FR-003**: The exclusion proof MUST contain: an inclusion proof for the witness key, the target key being excluded, and information about where the target key's path diverges from the witness's path
- **FR-004**: Verification MUST check that the embedded inclusion proof is valid and that the target key genuinely diverges from the witness's path at the claimed point
- **FR-005**: For empty trees, the exclusion proof MUST be a distinct "empty" variant that verifies against an empty root
- **FR-006**: Generation MUST return Nothing (fail) when called with a key that exists in the tree
- **FR-007**: The exclusion proof MUST be serializable for transmission and storage
- **FR-008**: The exclusion proof type MUST integrate with the existing MTS interface as a new proof type

### Key Entities

- **ExclusionProof**: A proof that a target key is absent from the tree. Contains the target key, witness information, and divergence data. Two variants: one for empty trees, one for populated trees with a witness.
- **Witness**: An existing tree node whose inclusion proof demonstrates the tree structure at the point where the target key's path diverges. Reuses the existing `InclusionProof` type.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All exclusion proofs for absent keys verify successfully (100% true-positive rate)
- **SC-002**: No exclusion proof can be generated for a present key (0% false-positive rate)
- **SC-003**: Verification of tampered proofs always fails (0% false-negative rate)
- **SC-004**: Exclusion proof verification requires no tree access — it is a pure computation on the proof data alone
- **SC-005**: QuickCheck property tests pass for random trees and random absent keys, covering all divergence scenarios

## Assumptions

- The existing `InclusionProof` type and `verifyInclusionProof` function are correct and reusable as building blocks
- The `Serialize` instances for existing proof types (InclusionProof, ProofStep) are available for composition
- The CSMT tree traversal primitives (reading nodes from the column) are available in the proof generation context
- Empty tree detection uses the existing root-hash query mechanism

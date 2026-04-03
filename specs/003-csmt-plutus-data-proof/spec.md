# Feature Specification: CSMT Plutus Data Proof Format

**Feature Branch**: `feat/csmt-plutus-data-proof`
**Created**: 2026-04-03
**Status**: Draft
**Input**: Issue #130 — compact Plutus Data proof format for on-chain CSMT verification

## User Scenarios & Testing

### User Story 1 - Transaction builder converts CSMT proof to Plutus Data (Priority: P1)

A transaction builder (Haskell off-chain code) has a CSMT inclusion
proof in CBOR format and needs to submit it as a redeemer to an Aiken
validator. It converts the proof to Plutus Data representation at
transaction building time.

**Why this priority**: Without this, no on-chain CSMT verification
is possible. This is the bridge between the off-chain proof format
and the on-chain verifier.

**Independent Test**: Given a CSMT inclusion proof, convert it to
Plutus Data bytes and verify the byte count is within budget.

**Acceptance Scenarios**:

1. **Given** a valid CSMT inclusion proof at N=1K entries,
   **When** converted to Plutus Data,
   **Then** the Plutus Data size is <= the MPF Aiken proof size
   at the same N.
2. **Given** a valid CSMT compact CBOR proof,
   **When** round-tripped through CBOR -> Plutus Data -> back,
   **Then** the reconstructed proof verifies against the same root.

---

### User Story 2 - Aiken validator verifies CSMT inclusion proof (Priority: P1)

An Aiken smart contract receives a CSMT inclusion proof as a
redeemer (Plutus Data) and verifies that a given key-value pair is
included in the Merkle root stored in the datum.

**Why this priority**: The on-chain verifier is the consumer of the
Plutus Data format. The format design must be driven by what makes
verification cheapest in execution units.

**Independent Test**: Deploy the Aiken validator on a devnet,
submit a transaction with a CSMT proof redeemer, and observe
successful validation.

**Acceptance Scenarios**:

1. **Given** a CSMT root hash in a datum and a valid proof redeemer,
   **When** the validator executes,
   **Then** it succeeds and the execution units are within Plutus
   budget limits.
2. **Given** a tampered proof (one hash byte flipped),
   **When** the validator executes,
   **Then** it fails.

---

### User Story 3 - Benchmark Plutus Data proof sizes against MPF (Priority: P2)

A developer runs the unified benchmark and sees CSMT Plutus Data
proof sizes compared side-by-side with MPF Aiken proof sizes at
N=1K, 10K, 100K.

**Why this priority**: Size parity with MPF is a stated goal. The
benchmark proves we achieve it.

**Independent Test**: Run `just bench` and check the output table.

**Acceptance Scenarios**:

1. **Given** N=1K, 10K, 100K datasets,
   **When** the benchmark runs,
   **Then** CSMT Plutus Data proof sizes are reported alongside
   MPF Aiken sizes.

---

### Edge Cases

- What happens with a proof for a tree with a single entry (depth 0)?
- How does the format handle maximum-depth proofs (256-bit keys)?
- What if rootJump is empty (no path compression at root)?

## Requirements

### Functional Requirements

- **FR-001**: System MUST define a Plutus Data layout for CSMT
  inclusion proofs using Constr tags and indefinite-length lists,
  consistent with Aiken's data encoding.
- **FR-002**: System MUST provide a Haskell function
  `renderCsmtPlutusProof :: InclusionProof Hash -> ByteString`
  that produces valid Plutus Data CBOR.
- **FR-003**: System MUST provide a Haskell parser
  `parseCsmtPlutusProof` that round-trips with the renderer.
- **FR-004**: System MUST provide an Aiken validator that verifies
  CSMT inclusion proofs encoded in this Plutus Data format.
- **FR-005**: The off-chain CBOR format (Compact.hs) MUST remain
  unchanged — it is the interchange format for third-party verifiers.
- **FR-006**: The Plutus Data format MUST strip fields the verifier
  already has (key, value, root hash), matching the compact format's
  approach.

### Key Entities

- **CompactProof**: The existing CBOR proof (rootJump + steps).
  Source format for the translator.
- **PlutusProof**: The new Plutus Data proof. Same logical content,
  different encoding optimized for on-chain consumption.
- **ProofStep**: stepConsumed (int) + sibling jump (packed key)
  + sibling hash (32 bytes). Mapped to a Plutus Data Constr.

## Success Criteria

### Measurable Outcomes

- **SC-001**: CSMT Plutus Data proof size <= MPF Aiken proof size
  at N=1K (currently 426 bytes).
- **SC-002**: Round-trip tests pass (CBOR -> Plutus Data -> parse
  -> verify).
- **SC-003**: Aiken validator compiles and passes test vectors.
- **SC-004**: Execution unit cost documented and within Plutus
  transaction limits for typical proofs.

## Assumptions

- The Aiken validator will use Blake2b-256, matching the Haskell
  implementation.
- The Aiken project will live under a `verifiers/` directory in
  haskell-mts (there's already a `verifiers/` dir).
- Plutus Data Constr tags follow the standard mapping: tags 121-127
  = Constr 0-6.
- The proof step structure is simple enough (single variant, unlike
  MPF's 3 variants) that a single Constr tag suffices.

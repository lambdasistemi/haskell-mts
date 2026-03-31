# Feature Specification: Compact CSMT Inclusion Proof CBOR Encoding

**Feature Branch**: `002-compact-csmt-inclusion`
**Created**: 2026-03-31
**Status**: Draft
**Input**: GitHub issue #128

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Smaller proofs for on-chain verification (Priority: P1)

A Cardano transaction submitter generates a CSMT inclusion proof to prove
a UTxO exists in the Merkle tree. The proof must be included in the
transaction, where every byte costs ADA. The current encoding wastes
~330 bytes on data the on-chain verifier already has (key, value, root
hash) and uses 1 byte per binary direction instead of packing 8
directions per byte.

**Why this priority**: Proof size directly determines on-chain transaction
cost. A 60% reduction (~669 bytes to ~250 bytes) saves real ADA per
transaction.

**Independent Test**: Generate proofs for the fruit test dataset, verify
compact encoding round-trips correctly and produces the same root hash
when verified.

**Acceptance Scenarios**:

1. **Given** a 30-fruit CSMT trie, **When** generating a compact proof for
   any fruit, **Then** the compact proof is at least 50% smaller than the
   current CBOR proof.
2. **Given** a compact proof, **When** the verifier reconstructs the root
   hash using the proof plus externally-supplied key and value, **Then**
   the root hash matches the trie's actual root.
3. **Given** a compact proof, **When** parsing it back, **Then** the
   round-tripped proof produces the same root hash as the original.

---

### User Story 2 - On-chain verifier compatibility (Priority: P2)

An on-chain CSMT verifier (to be written in Aiken or Plutus) consumes
compact proofs. The encoding must be deterministic and use standard CBOR
so the on-chain verifier can decode it without custom parsers.

**Why this priority**: Without a defined on-chain format, the compact
proof is only useful off-chain.

**Independent Test**: The CBOR encoding is deterministic — same proof
always produces same bytes.

**Acceptance Scenarios**:

1. **Given** the same inclusion proof, **When** encoded twice, **Then**
   the outputs are byte-for-byte identical.
2. **Given** a compact proof CBOR, **When** decoded by a standard CBOR
   parser, **Then** all fields are accessible without custom decoders.

---

### Edge Cases

- What happens when the root jump is empty (single-element trie)?
- How does the encoding handle a proof with zero steps (key at root)?
- What is the maximum proof size for a maximally deep trie (256 levels)?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST provide `renderCompactProof` that encodes an
  `InclusionProof` to a compact CBOR bytestring, stripping key, value,
  and root hash fields.
- **FR-002**: System MUST provide `parseCompactProof` that decodes a
  compact CBOR bytestring back to an `InclusionProof` given externally
  supplied key, value, and root hash.
- **FR-003**: Binary directions (Key) MUST be packed as bits (8 per byte,
  MSB first) with a length prefix, not as individual CBOR words.
- **FR-004**: Round-trip `parseCompactProof(renderCompactProof(proof))`
  MUST produce a proof that computes the same root hash as the original.
- **FR-005**: Compact proof encoding MUST be deterministic.

### Key Entities

- **CompactProof**: CBOR array of `[rootJumpBits, step1, step2, ...]`
- **CompactStep**: CBOR array of `[consumedInt, siblingJumpBits, siblingHash]`
- **PackedKey**: 2-byte big-endian bit count + packed bits (MSB first)

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Average proof size at N=1K is under 300 bytes (currently 669).
- **SC-002**: Average proof size at N=10K is under 400 bytes (currently 804).
- **SC-003**: All 30 fruit proofs round-trip correctly via compact encoding.
- **SC-004**: QuickCheck property: compact round-trip preserves root hash computation for random proofs.

## Assumptions

- No existing on-chain CSMT verifier constrains the format — we design from scratch.
- The verifier always has the key, value, and root hash externally.
- CBOR is the encoding format (consistent with MPF Aiken proofs).
- The `CSMT.Hashes.CBOR` module (old format) remains for backward compatibility.

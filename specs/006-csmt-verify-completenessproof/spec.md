# Feature Specification: csmt-verify CompletenessProof CBOR codec and pure verifier

**Feature Branch**: `feat/csmt-verify-completeness`
**Created**: 2026-04-30
**Status**: Draft
**Issue**: https://github.com/lambdasistemi/haskell-mts/issues/153
**Downstream**: https://github.com/cardano-foundation/cardano-mpfs-offchain/issues/243

## Context

`csmt-verify` (the WASM/GHC-JS-friendly sublib) currently exposes only
`verifyInclusionProof` and `verifyExclusionProof`. Completeness
verification is missing: `CompletenessProof` lives in `csmt-write` and
has no CBOR codec anywhere in the repo. This blocks the
`cardano-mpfs-offchain` proof redesign (#243) — the redesigned
`GET /tokens` and `GET /tokens/:id` carry a `requests_completeness_proof`
that the offline `cardano-mpfs-client` verifier must replay. The
verifier package depends on `mts:csmt-verify` only (Principle IX:
native + GHC-WASM + GHC-JS portability), so it cannot pull `csmt-write`.

## User Scenarios & Testing

### User Story 1 — Offline client verifies a completeness proof (Priority: P1)

A `cardano-mpfs-client` instance running in a browser (GHC-JS) or
WASM context receives a `requests_completeness_proof` from a `umpfs`
HTTP response, alongside a trusted root hash and the leaves it claims
cover under a known prefix. It must replay the verification without
reaching the `csmt-write` library.

**Why this priority**: This is the only blocker for the #243 proof
redesign. The codec and the pure fold are the entire feature.

**Independent Test**: Generate a `CompletenessProof` server-side via
`generateProof`, encode to CBOR, ship the bytes to a verifier that
imports only `csmt-verify`, decode and verify against the trusted
root and prefix. Verifier returns `True` iff the bytes are authentic.

**Acceptance Scenarios**:

1. **Given** a CSMT populated with N leaves and a prefix `P`,
   **When** the server generates a completeness proof, encodes it as
   CBOR, and the client invokes
   `verifyCompletenessProof rootBytes P leaves proofBytes`,
   **Then** the verifier returns `True`.
2. **Given** the same setup but with the root bytes flipped,
   **When** the client invokes the verifier, **Then** the verifier
   returns `False`.
3. **Given** garbage bytes in place of `proofBytes`, **When** the
   client invokes the verifier, **Then** the verifier returns
   `False` (no exceptions).

### User Story 2 — Empty-prefix completeness for `POST /tx/oracle/end` (Priority: P1)

The `oracle/end` flow in #243 attests that the request set under a
prefix is empty. The server emits a completeness proof with
`leaves = []`; the client must accept it iff the prefix really is
empty in the tree at the trusted root.

**Why this priority**: Load-bearing for one of the two `/tokens`
endpoints in #243. Without this, the empty-set case has no
verifiable wire form.

**Independent Test**: Build a tree, choose a prefix that has no
leaves, generate the proof, verify it on the verifier side with
`leaves = []`.

**Acceptance Scenarios**:

1. **Given** a tree where prefix `P` has no leaves, **When** the
   client invokes
   `verifyCompletenessProof rootBytes P [] proofBytes`,
   **Then** the verifier returns `True`.
2. **Given** the same tree but with one leaf added under `P`,
   **When** the verifier is called with `leaves = []` and the
   stale proof bytes, **Then** the verifier returns `False`.

### Edge Cases

- Single-leaf subtree (`length leaves == 1`, `cpMergeOps == []`):
  the fold short-circuits without invoking `foldMergeOps`.
- Whole-tree completeness (`prefix == []`): `cpInclusionSteps`
  may be empty when the root jump subsumes the leaves.
- Tampered `cpMergeOps` indices that reference leaves out of
  range: the existing fold already returns `Nothing` via
  `foldMergeOps`; the wrapped verifier converts to `False`.

## Requirements

### Functional Requirements

- **FR-001**: `csmt-core` MUST expose a `parseCompletenessProof`
  / `renderCompletenessProof` pair on `CompletenessProof Hash`,
  symmetric with `parseProof` / `renderProof` for inclusion proofs.
- **FR-002**: `csmt-core` MUST own the `CompletenessProof` data
  type and the pure `foldCompletenessProof` fold, so neither
  depends on `csmt-write`.
- **FR-003**: `csmt-verify` MUST expose a function with the shape
  `verifyCompletenessProof :: ByteString -> Key -> [Indirect Hash] -> ByteString -> Bool`
  that returns `False` rather than throwing on malformed input.
- **FR-004**: `csmt-verify` MUST re-export `CompletenessProof` and
  `parseCompletenessProof` so consumers can construct or inspect
  proofs without pulling `csmt-core` directly.
- **FR-005**: `csmt-write` MUST keep its existing API surface
  (`CSMT.Proof.Completeness`); the only change there is to re-export
  the moved types/fold from `csmt-core`. The DB-backed
  `generateProof` / `collectValues` / `queryPrefix` stay in
  `csmt-write`.
- **FR-006**: The wire format MUST round-trip: any
  `CompletenessProof Hash` produced by the write side encodes,
  decodes, and verifies bit-for-bit on the verifier side under
  the matching root.
- **FR-007**: The `csmt-verify` sublibrary MUST remain pure
  Haskell with no new C deps (Principle IX).

### Key Entities

- **CompletenessProof** — `cpMergeOps :: [(Int, Int)]` plus
  `cpInclusionSteps :: [ProofStep a]`. Already exists in
  `csmt-write`; moves to `csmt-core` unchanged.
- **CBOR layout** — list of length 2:
  `[ list-of-pair-of-int  ;  list-of-ProofStep ]`. The pair encoding
  is `[ int, int ]`; `ProofStep` reuses the existing
  `encodeProofStep` codec.

## Success Criteria

- **SC-001**: A property test in the unit-tests suite generates a
  random tree, produces a `CompletenessProof`, runs it through
  `renderCompletenessProof → parseCompletenessProof →
  verifyCompletenessProof`, and asserts acceptance under the
  matching root.
- **SC-002**: An empty-leaves test asserts acceptance for an empty
  prefix in a populated tree.
- **SC-003**: A garbage-bytes test asserts `verifyCompletenessProof`
  returns `False` (not exception) on malformed bytes.
- **SC-004**: `nix --quiet build .#mts-wasm` (or equivalent WASM
  target) keeps building — `csmt-verify` does not gain a C
  dependency.
- **SC-005**: `csmt-write`'s existing `CSMT.Proof.Completeness`
  consumers (`CSMT.MTS`, the existing `CompletenessSpec` tests)
  compile unchanged.

## Assumptions

- The CBOR layout of `(Int, Int)` pairs is a fixed-length-2 list;
  consistent with the existing `encodeProofStep` shape.
- `Hash`-specialised codec is sufficient; no need for a polymorphic
  `CompletenessProof a` codec.
- The fold `foldCompletenessProof` is already pure (verified by
  reading `lib/csmt-write/CSMT/Proof/Completeness.hs:113-143`).
- Existing tests in `test/CSMT/Proof/CompletenessSpec.hs` continue
  to use `foldCompletenessProof` via the re-export from
  `csmt-write`.

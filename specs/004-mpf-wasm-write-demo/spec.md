# Feature Specification: MPF WASM Write Path and Browser Demo

**Feature Branch**: `feat/port-mpf-write-path-to-wasm-with-browser-demo-mirr`
**Created**: 2026-04-22
**Status**: Draft
**Input**: Issue #146 — port the MPF write path to WASM with a browser demo, mirroring the CSMT write-path work from PR #145

## User Scenarios & Testing

### User Story 1 - Browser computes MPF roots off-chain (Priority: P1)

A developer opens the MPF browser demo, inserts and deletes key/value
pairs, and sees the MPF root computed entirely in the browser with no
server round-trip.

**Why this priority**: This is the core deliverable of the ticket. If
the write path does not run under `wasm32-wasi`, there is no browser
story.

**Independent Test**: Build `mpf-write-wasm`, load the browser demo,
start from an empty state blob, apply inserts/deletes, and verify the
reported root changes deterministically and survives reload.

**Acceptance Scenarios**:

1. **Given** an empty browser session, **When** the user inserts a batch
   of key/value pairs, **Then** the demo returns a new persisted state
   blob, a 32-byte root, and a proof envelope for the queried key.
2. **Given** a non-empty persisted session, **When** the user reloads
   the page, **Then** the same state blob is restored and subsequent
   operations continue from that root.

---

### User Story 2 - Pure Blake2b matches native MPF roots (Priority: P1)

A maintainer needs confidence that replacing `crypton` in the MPF write
path does not change trie roots, proof hashes, or Aiken-compat output.

**Why this priority**: If the pure Blake2b route is not byte-identical,
the entire WASM port is invalid because browser-computed roots would no
longer match native or on-chain expectations.

**Independent Test**: Run a QuickCheck property that builds the same MPF
tree with `crypton` hashing and with the pure Blake2b path and asserts
the roots are byte-identical.

**Acceptance Scenarios**:

1. **Given** arbitrary distinct input key/value pairs, **When** the tree
   is built once with `crypton` hashing and once with the pure Blake2b
   route, **Then** both runs produce the same root bytes.
2. **Given** the current MPF module tree, **When** the hash audit is
   completed, **Then** every write-path hash site is confirmed to be
   Blake2b-256 or the implementation stops and flags the blocker.

---

### User Story 3 - Downstream users keep importing `mts:mpf` unchanged (Priority: P2)

A downstream consumer such as MPFS off-chain keeps depending on
`mts:mpf` and builds successfully even after the write-path modules are
split into a new pure `mpf-write` sublibrary.

**Why this priority**: The ticket is a refactor plus portability
feature, not a breaking package redesign.

**Independent Test**: Build the downstream consumer against the updated
`mts:mpf` without changing its imports or cabal configuration.

**Acceptance Scenarios**:

1. **Given** downstream code importing `MPF`, `MPF.Hashes`, and the
   native backend, **When** the package is rebuilt, **Then** those
   imports still resolve through `mts:mpf`.
2. **Given** the browser/WASM-only code path, **When** `mpf-write` is
   built for `wasm32-wasi`, **Then** no RocksDB or native-only
   dependency is required.

## Edge Cases

- Empty tree: the WASM response must emit `ptype = 0xff` and an empty
  proof when the query key has no witness.
- Deletion of a present key must return the updated state blob and the
  best available exclusion witness for the queried key.
- Odd/even nibble-path encodings in `MPF.Hashes.Aiken` must remain
  byte-stable after the pure Blake2b swap.
- Browser persistence must tolerate a missing or corrupt IndexedDB blob
  by resetting cleanly to empty state.

## Requirements

### Functional Requirements

- **FR-001**: System MUST audit the MPF write path and prove that the
  pure Blake2b route yields byte-identical roots to the existing
  `crypton` implementation.
- **FR-002**: System MUST extract a pure `mpf-write` sublibrary
  containing the MPF algebra, pure backend, proof generation, and
  standalone column layout needed by WASM.
- **FR-003**: System MUST keep `mts:mpf` as the downstream-facing
  package surface by re-exporting `mpf-write` modules and retaining the
  native-only RocksDB backend there.
- **FR-004**: System MUST provide an `mpf-write-wasm` executable using
  the same opcode-tagged protocol as `csmt-write-wasm`:
  insert/delete input ops, persisted state blob output, root bytes,
  queried value bytes, proof type, and proof bytes.
- **FR-005**: System MUST provide serializable round-tripping for the
  MPF in-memory database blob so browser hosts can persist and restore
  tree state between invocations.
- **FR-006**: System MUST provide a browser demo under
  `verifiers/browser-write-mpf/` with insert, delete, inclusion proof,
  exclusion proof, persistence, and undo/redo.
- **FR-007**: System MUST stage the MPF demo into the docs build and the
  preview deployment alongside the existing verify and CSMT demos.
- **FR-008**: System MUST preserve the public `mts:mpf` build for
  downstream consumers such as MPFS off-chain.

### Key Entities

- **MPFWriteLibrary**: The pure sublibrary that can compile for both
  native and `wasm32-wasi`.
- **MPFStateBlob**: The serialized `MPFInMemoryDB` payload persisted by
  the browser between WASM invocations.
- **MPFWriteOp**: Opcode-tagged insert/delete mutation passed into the
  WASM executable.
- **MPFProofEnvelope**: The response tuple of root, queried value, proof
  type, and serialized proof bytes.

## Success Criteria

### Measurable Outcomes

- **SC-001**: The new MPF root parity property passes for randomized
  inputs, demonstrating byte-identical roots between `crypton` and the
  pure Blake2b route.
- **SC-002**: `wasm32-wasi-cabal --project-file=cabal-wasm.project build mpf-write-wasm`
  succeeds.
- **SC-003**: The browser demo can insert, delete, prove, verify,
  persist, and undo/redo against the WASM executable with no server.
- **SC-004**: The docs build includes the MPF demo and the preview site
  serves verify, CSMT write, and MPF write demos together.
- **SC-005**: A downstream build against `mts:mpf` succeeds unchanged.

## Assumptions

- The branch remains based on `feat/wasm-write-path` until PR #145
  merges, after which it can be rebased to `main`.
- The existing CSMT browser-write protocol is the canonical wire format
  to mirror for MPF.
- No new Lean invariant work is required because the ticket preserves
  the existing MPF semantics and focuses on portability plus packaging.

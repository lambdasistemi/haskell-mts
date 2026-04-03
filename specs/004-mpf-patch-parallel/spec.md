# Feature Specification: patchParallel for MPF

**Feature Branch**: `feat/mpf-patch-parallel`
**Created**: 2026-04-03
**Status**: Draft
**Input**: Issue #94

## User Scenarios & Testing

### User Story 1 - Parallel journal replay for MPF (Priority: P1)

The MTS split-mode transition (KVOnly → Full) replays the
journal into the MPF tree. Currently this is sequential.
With patchParallel, entries are grouped by hex-digit bucket
prefix and applied concurrently, matching the CSMT pattern.

**Why this priority**: This is the core feature. Without it,
MPF replay is single-threaded.

**Independent Test**: Insert N entries via KVOnly, transition
to Full with parallel replay, verify root hash matches
sequential replay.

**Acceptance Scenarios**:

1. **Given** a tree with N journal entries,
   **When** parallel replay runs with bucketDigits=1,
   **Then** the root hash matches sequential replay.
2. **Given** a tree with mixed inserts and deletes,
   **When** parallel replay runs,
   **Then** the root hash matches sequential replay.

---

### User Story 2 - Crash recovery with MPF buckets (Priority: P1)

If the process crashes during parallel replay, recovery
detects the sentinel and completes the merge.

**Acceptance Scenarios**:

1. **Given** a crash after partial parallel replay,
   **When** the process restarts,
   **Then** recovery runs mergeSubtreeRoots and the final
   state matches a clean replay.

---

### Edge Cases

- Empty tree: no expand needed, replay creates from scratch
- Single entry: one bucket occupied, path compression at root
- All entries in same bucket: merge is a no-op (single root)
- Branch root in single bucket: must recompute branchHash
  with extended prefix

## Requirements

- **FR-001**: Parallel replay MUST produce the same root hash
  as sequential replay for any input
- **FR-002**: The 3-way hash model (leafHash, merkleRoot,
  branchHash) MUST be respected during mergeSubtreeRoots
- **FR-003**: Crash recovery MUST work with the sentinel
  protocol (same as CSMT)
- **FR-004**: bucketDigits parameter controls granularity
  (1 digit = 16 buckets, 2 = 256)

## Success Criteria

- **SC-001**: QuickCheck property: parallel root == sequential
  root (100 runs)
- **SC-002**: Mixed insert+delete property passes
- **SC-003**: All existing MPF tests pass (no regression)

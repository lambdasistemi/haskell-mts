# Feature Specification: Replay Tracing

**Feature Branch**: `001-replay-tracing`
**Created**: 2026-03-27
**Status**: Draft
**Input**: User description: "Add traces to journal replay phase: extend ReplayEvent with entries-remaining count, add trace callbacks to mpfReplayJournal, and add tracing support to standalone csmtReplayJournal"
**Issue**: #99

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Replay Progress Monitoring (Priority: P1)

A library consumer replays a journal after opening a persistent store.
During replay, the consumer receives progress notifications indicating
how many entries have been processed and how many remain. This allows
the consumer to display a progress indicator or log estimated
completion time.

**Why this priority**: Without entries-remaining information, consumers
cannot estimate replay duration or display meaningful progress. This
is the core value of the feature.

**Independent Test**: Can be tested by replaying a journal with a
known number of entries and verifying the callback receives
decreasing entries-remaining counts that reach zero.

**Acceptance Scenarios**:

1. **Given** a journal with N entries, **When** replay begins, **Then** the first trace event reports N entries remaining
2. **Given** a journal replay in progress, **When** each entry is processed, **Then** the entries-remaining count decreases monotonically
3. **Given** a journal replay that completes, **Then** the final trace event reports 0 entries remaining

---

### User Story 2 - MPF Replay Tracing (Priority: P2)

A library consumer using the MPF (Merkle Patricia Forest)
implementation replays a journal and receives the same trace
callbacks that the CSMT implementation provides. The consumer does
not need to know which implementation is active to receive progress
information.

**Why this priority**: MPF currently lacks any replay tracing. Adding
it achieves parity with CSMT and fulfills the shared-interface
principle.

**Independent Test**: Can be tested by replaying an MPF journal with
a trace callback and verifying events are received with correct
entries-remaining counts.

**Acceptance Scenarios**:

1. **Given** an MPF store with a populated journal, **When** replay is triggered with a trace callback, **Then** trace events are emitted for each replayed entry
2. **Given** an MPF replay with tracing, **Then** the trace events include the same information (entry data and entries remaining) as CSMT replay events

---

### User Story 3 - Standalone CSMT Replay Tracing (Priority: P3)

A library consumer using the standalone CSMT replay function (outside
the composed backend) receives trace callbacks during replay.
Currently only the composed backend (`mkKVOnlyOps`, `mkFullOps`)
supports tracing; the standalone `csmtReplayJournal` does not.

**Why this priority**: Completes coverage for all replay entry points.
Some consumers use the standalone function directly.

**Independent Test**: Can be tested by calling the standalone CSMT
replay function with a trace callback and verifying events are
received.

**Acceptance Scenarios**:

1. **Given** a standalone CSMT store with a populated journal, **When** `csmtReplayJournal` is called with a trace callback, **Then** trace events are emitted
2. **Given** a standalone CSMT replay, **Then** trace events include entries-remaining counts consistent with the journal size

---

### Edge Cases

- What happens when the journal is empty? The replay completes immediately with no trace events emitted.
- What happens if the trace callback throws an exception? The replay propagates the exception without catching it (fail-fast behavior).
- What happens during concurrent replays? Each replay maintains its own entries-remaining counter independently.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: Replay trace events MUST include the number of journal entries remaining to be processed
- **FR-002**: The entries-remaining count MUST start at the total journal size and decrease monotonically to zero
- **FR-003**: The MPF replay function MUST accept and invoke a trace callback with the same event structure as CSMT
- **FR-004**: The standalone CSMT replay function MUST accept and invoke a trace callback
- **FR-005**: Tracing MUST be opt-in — callers who do not provide a callback experience no change in behavior
- **FR-006**: All three replay entry points (composed CSMT, standalone CSMT, MPF) MUST emit trace events with consistent structure

### Key Entities

- **Replay Event**: A notification emitted during journal replay, carrying the current entry being processed and the number of entries remaining
- **Trace Callback**: A consumer-provided function invoked once per journal entry during replay

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: 100% of replay entry points (composed CSMT, standalone CSMT, MPF) support trace callbacks
- **SC-002**: Consumers can determine replay progress as a percentage (entries processed / total entries) from trace events alone
- **SC-003**: Existing consumers who do not use tracing experience zero behavioral change (backward compatibility)
- **SC-004**: Trace event structure is consistent across all implementations — a single consumer callback works with any backend

## Assumptions

- The existing `ReplayEvent` type in the CSMT composed backend is the baseline to extend
- The trace callback is a simple function invocation (not a streaming/channel mechanism)
- Performance overhead of invoking the callback once per journal entry is acceptable
- The total journal entry count is known or computable at the start of replay

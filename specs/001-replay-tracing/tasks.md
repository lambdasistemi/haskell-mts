# Tasks: Replay Tracing

**Input**: Design documents from `/specs/001-replay-tracing/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md, contracts/

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

---

## Phase 1: Foundational (Blocking Prerequisites)

**Purpose**: Extend the shared ReplayEvent type used by all three replay entry points

- [x] T001 Add `rsEntriesRemaining :: Int` field to `ReplayStart` constructor in `lib/mts/MTS/Interface.hs` (moved to shared module)
- [x] T002 Update `mkKVOnlyOps` replay loop in `lib/csmt/CSMT/MTS.hs` to read journal size from metrics counter and pass entries-remaining to `ReplayStart`
- [x] T003 Update all existing call sites that construct or pattern-match `ReplayStart` to handle the new field

**Checkpoint**: Composed CSMT replay emits entries-remaining. Existing tests pass.

---

## Phase 2: User Story 1 - Replay Progress Monitoring (Priority: P1)

**Goal**: Entries-remaining count decreases monotonically and reaches zero

**Independent Test**: Replay a journal with known entry count, verify callback receives monotonically decreasing entries-remaining ending at 0

- [x] T004 [US1] Add QuickCheck property in `lib/mts/MTS/Properties.hs` and `test/MTS/PropertySpec.hs` verifying `rsEntriesRemaining` decreases monotonically to 0
- [x] T005 [US1] Add Haddock documentation on `rsEntriesRemaining` field and updated `ReplayEvent` type in `lib/mts/MTS/Interface.hs`

**Checkpoint**: Composed CSMT replay tracing fully works with entries-remaining. Property test passes.

---

## Phase 3: User Story 2 - MPF Replay Tracing (Priority: P2)

**Goal**: `mpfReplayJournal` accepts and invokes a trace callback with entries-remaining

**Independent Test**: Replay an MPF journal with a trace callback, verify events received with correct entries-remaining counts

- [x] T006 [US2] Add `(ReplayEvent -> IO ())` trace callback parameter to `mpfReplayJournal` in `lib/mpf/MPF/MTS.hs`
- [x] T007 [US2] Implement entries-remaining tracking in `mpfReplayJournal` replay loop: read journal size from metrics, emit `ReplayStart`/`ReplayStop` per chunk in `lib/mpf/MPF/MTS.hs`
- [x] T008 [US2] Update all call sites of `mpfReplayJournal` to pass trace callback (including `mpfManagedTransition` in `lib/mpf/MPF/MTS.hs`)
- [x] T009 [US2] Add test in `test/MTS/PropertySpec.hs` verifying MPF replay emits trace events with correct entries-remaining counts
- [x] T010 [US2] Add Haddock documentation on the new trace parameter in `lib/mpf/MPF/MTS.hs`

**Checkpoint**: MPF replay tracing works with same event structure as CSMT.

---

## Phase 4: User Story 3 - Standalone CSMT Replay Tracing (Priority: P3)

**Goal**: `csmtReplayJournal` accepts and invokes a trace callback with entries-remaining

**Independent Test**: Call standalone CSMT replay with a trace callback, verify events received

- [x] T011 [US3] Add `(ReplayEvent -> IO ())` trace callback parameter to `csmtReplayJournal` in `lib/csmt/CSMT/MTS.hs`
- [x] T012 [US3] Implement entries-remaining tracking in `csmtReplayJournal` replay loop: read journal size from metrics, emit `ReplayStart`/`ReplayStop` per chunk in `lib/csmt/CSMT/MTS.hs`
- [x] T013 [US3] Update all call sites of `csmtReplayJournal` to pass trace callback (including `csmtManagedTransition` in `lib/csmt/CSMT/MTS.hs`)
- [x] T014 [US3] Add test in `test/MTS/PropertySpec.hs` verifying standalone CSMT replay emits trace events with correct entries-remaining counts
- [x] T015 [US3] Add Haddock documentation on the new trace parameter in `lib/csmt/CSMT/MTS.hs`

**Checkpoint**: All three replay entry points support tracing with entries-remaining.

---

## Phase 5: Polish

- [x] T016 Run `just format` to ensure fourmolu compliance
- [x] T017 Run `just hlint` and fix any warnings
- [x] T018 Run `just ci` to verify full CI pipeline passes locally
- [x] T019 Update module export lists if new symbols were added

---

## Dependencies & Execution Order

### Phase Dependencies

- **Phase 1 (Foundational)**: No dependencies — start here
- **Phase 2 (US1)**: Depends on Phase 1 (needs extended ReplayEvent)
- **Phase 3 (US2)**: Depends on Phase 1 only (different file: `MPF/MTS.hs`)
- **Phase 4 (US3)**: Depends on Phase 1 only (same file as Phase 1 but different function)
- **Phase 5 (Polish)**: Depends on all phases complete

### Parallel Opportunities

- **Phase 2 and Phase 3** can run in parallel (different files: `CSMT/MTS.hs` tests vs `MPF/MTS.hs`)
- Within Phase 3: T006+T007 before T008+T009+T010
- Within Phase 4: T011+T012 before T013+T014+T015

---

## Implementation Strategy

### MVP First (Phase 1 + Phase 2)

1. Extend `ReplayEvent` with `rsEntriesRemaining` (T001-T003)
2. Add property test and docs (T004-T005)
3. **VALIDATE**: Existing tests pass, new property passes

### Full Delivery

4. Add MPF tracing (T006-T010) — can parallel with step 5
5. Add standalone CSMT tracing (T011-T015)
6. Polish (T016-T019)

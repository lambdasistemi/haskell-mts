# Tasks: patchParallel for MPF

## Phase 1: Raw tree operations

- [ ] T001 Add `insertingRaw` to `MPF/Insertion.hs` — takes
  raw `HexKey` + value hash, no `FromHexKV`
- [ ] T002 Add `deletingRaw` to `MPF/Deletion.hs` — takes
  raw `HexKey`, no `FromHexKV`
- [ ] T003 Verify existing MPF tests pass

## Phase 2: Tree expansion

- [ ] T004 Add `expandToBucketDepthMPF` to `MPF/Insertion.hs`
  — 16-ary expand, split jumps at hex digit boundaries
- [ ] T005 Add `allHexPrefixes` and `hexBucketIndex` helpers

## Phase 3: Subtree merge (the hard part)

- [ ] T006 Add `mergeSubtreeRootsMPF` to `MPF/Insertion.hs`
  — use nodeHash for leaf/branch distinction, handle
  single-bucket branch recomputation
- [ ] T007 Unit test: merge with 1 occupied bucket (leaf)
- [ ] T008 Unit test: merge with 1 occupied bucket (branch)
- [ ] T009 Unit test: merge with multiple occupied buckets

## Phase 4: Populate module

- [ ] T010 Create `MPF/Populate.hs` with `patchParallelMPF`
  — group by hex prefix, return independent transactions
- [ ] T011 Add `MPF.Populate` to `mts.cabal`

## Phase 5: Tests

- [ ] T012 QuickCheck property: parallel root == sequential
  root (insert-only)
- [ ] T013 QuickCheck property: parallel root == sequential
  root (mixed insert + delete)
- [ ] T014 Full regression: all existing tests pass

## Phase 6: MTS integration

- [ ] T015 Wire patchParallelMPF into `MPF/MTS.hs` replay
  loop with sentinel protocol
- [ ] T016 Update ReplayEvent trace to show real bucket counts

## Dependencies

- Phase 1 → Phase 2 → Phase 3 → Phase 4 → Phase 5
- Phase 6 depends on Phase 4
- T006 is the hardest task — budget most time here

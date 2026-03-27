# Research: Replay Tracing

## Existing Trace Infrastructure

### Decision: Extend existing `ReplayEvent` type
**Rationale**: `ReplayEvent` already exists in `CSMT/MTS.hs:1026-1042` with
`ReplayStart` and `ReplayStop` constructors. Adding an `entriesRemaining`
field to `ReplayStart` is backward-compatible via record syntax (callers
already pattern-match on named fields). No new type needed.

**Alternatives considered**:
- New `ReplayProgress` type separate from `ReplayEvent` — rejected because
  it would require a second callback parameter or a sum-of-sums wrapper.
- Per-entry events instead of per-chunk — rejected because the composed
  CSMT backend already traces at chunk granularity, and per-entry would
  change the existing contract.

### Decision: Use journal metrics counter for entries-remaining
**Rationale**: Both CSMT and MPF maintain a `journalSizeKey = "j"` counter
in their metrics column. Reading this once at the start of replay gives
the total. After each chunk, subtract chunk size to get remaining count.
No full-scan needed.

**Alternatives considered**:
- Count entries by iterating the journal at startup — rejected because
  it would add O(n) overhead for something already tracked.

### Decision: Callback-based tracing (not MonadTrace or logging)
**Rationale**: The existing pattern in `mkKVOnlyOps` uses
`(ReplayEvent -> IO ())` as a plain callback. This is framework-agnostic
and matches constitution principle VI (opt-in, not tied to a logging
framework). All new replay functions follow the same pattern.

**Alternatives considered**:
- `MonadWriter` or `MonadTrace` constraint — rejected because it would
  constrain the monad stack for all callers.
- Optional `Maybe (ReplayEvent -> IO ())` parameter — rejected because
  callers can pass `const (pure ())` for no-op; a Maybe adds unwrapping
  noise everywhere.

## Key Code Locations

| Component | File | Line | Has Trace? |
|-----------|------|------|------------|
| `ReplayEvent` | `lib/csmt/CSMT/MTS.hs` | 1026 | Type def |
| `mkKVOnlyOps` replay loop | `lib/csmt/CSMT/MTS.hs` | 1181 | Yes |
| `csmtReplayJournal` | `lib/csmt/CSMT/MTS.hs` | 808 | No |
| `mpfReplayJournal` | `lib/mpf/MPF/MTS.hs` | 630 | No |
| Journal size metric | `lib/csmt/CSMT/MTS.hs` | 174 | Counter |
| MPF journal size metric | `lib/mpf/MPF/MTS.hs` | 168 | Counter |

## Entries-Remaining Semantics

The entries-remaining count is emitted **per chunk**, not per entry:
- At replay start: remaining = total journal size (from metrics counter)
- After each chunk: remaining -= chunk entries processed
- At replay end: remaining = 0

This matches the existing `ReplayStart`/`ReplayStop` granularity in
`mkKVOnlyOps` and avoids per-entry overhead.

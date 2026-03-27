# Contract: Replay Functions API

## Trace Callback Type

All replay entry points accept:
```
trace :: ReplayEvent -> IO ()
```

## Modified Function Signatures

### csmtReplayJournal (standalone)

Current:
```
csmtReplayJournal :: prefix -> chunkSize -> runner -> db -> fromKV -> hashing -> IO ()
```

New:
```
csmtReplayJournal :: prefix -> chunkSize -> runner -> db -> fromKV -> hashing -> trace -> IO ()
```

### mpfReplayJournal (standalone)

Current:
```
mpfReplayJournal :: prefix -> chunkSize -> runner -> db -> fromKV -> hashing -> IO ()
```

New:
```
mpfReplayJournal :: prefix -> chunkSize -> runner -> db -> fromKV -> hashing -> trace -> IO ()
```

### mkKVOnlyOps (composed CSMT)

Existing signature already takes `(ReplayEvent -> IO ())`.
Only change: `ReplayStart` now includes `rsEntriesRemaining`.

## Backward Compatibility

- Adding a field to `ReplayStart` is breaking for callers that
  pattern-match with `RecordWildCards` (they'll get an unused-bind
  warning, not an error). Callers using `NamedFieldPuns` are unaffected.
- Adding a `trace` parameter to standalone functions is a breaking
  signature change. All call sites must be updated.

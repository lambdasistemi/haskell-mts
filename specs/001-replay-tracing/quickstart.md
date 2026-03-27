# Quickstart: Replay Tracing

## Using trace callbacks

### Standalone CSMT replay with progress logging

```haskell
import CSMT.MTS (csmtReplayJournal, ReplayEvent(..))

replayWithLogging db = do
    let trace = \case
            ReplayStart{rsChunkSize, rsEntriesRemaining} ->
                putStrLn $ "Processing " <> show rsChunkSize
                    <> " entries, " <> show rsEntriesRemaining
                    <> " remaining"
            ReplayStop -> putStrLn "Chunk done"
    csmtReplayJournal prefix chunkSize run db fromKV hashing trace
```

### MPF replay with progress bar

```haskell
import MPF.MTS (mpfReplayJournal, ReplayEvent(..))

replayWithProgress db = do
    let trace = \case
            ReplayStart{rsEntriesRemaining} ->
                updateProgressBar rsEntriesRemaining
            ReplayStop -> pure ()
    mpfReplayJournal prefix chunkSize run db fromKV hashing trace
```

### No-op tracing (backward-compatible usage)

```haskell
csmtReplayJournal prefix chunkSize run db fromKV hashing (const $ pure ())
```

## Verifying the feature

```bash
just unit "replay tracing"
```

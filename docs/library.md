# Library API

This page covers using the CSMT library in Haskell applications.

## Overview

The library provides:

- `CSMT` - Main module re-exporting the public API
- `CSMT.Hashes` - Blake2b-256 based operations
- `CSMT.Backend.RocksDB` - Persistent storage backend
- `CSMT.Backend.Pure` - In-memory backend for testing

## Basic Setup

### With RocksDB (Production)

```haskell
import CSMT
import CSMT.Hashes
import CSMT.Backend.RocksDB
import CSMT.Backend.Standalone

-- Open database and run operations
main :: IO ()
main = withRocksDB "path/to/db" 256 256 $ \(RunRocksDB runDB) -> do
    db <- runDB $ standaloneRocksDBDatabase codecs
    -- Use db for transactions
```

### With Pure Backend (Testing)

```haskell
import CSMT
import CSMT.Backend.Pure
import CSMT.Backend.Standalone

-- Run in-memory operations
example :: (result, InMemoryDB)
example = runPure emptyInMemoryDB $ do
    runPureTransaction codecs $ do
        -- Your operations here
```

## Core Operations

### Inserting Values

```haskell
import CSMT.Hashes (insert, fromKVHashes)

-- Insert a key-value pair
insertExample :: Transaction m cf d ops ()
insertExample =
    insert fromKVHashes kvCol csmtCol "mykey" "myvalue"
```

The `insert` function:

1. Stores the key-value pair in the KV column
2. Computes the tree key as `treePrefix(value) <> fromK(key)`
3. Computes the value hash
4. Updates the CSMT structure at the tree key
5. Recomputes affected node hashes

### Deleting Values

```haskell
import CSMT.Hashes (delete, fromKVHashes)

-- Delete a key
deleteExample :: Transaction m cf d ops ()
deleteExample =
    delete fromKVHashes kvCol csmtCol "mykey"
```

Deletion:

1. Looks up the value from the KV column (needed to compute the tree key
   via `treePrefix(value) <> fromK(key)`)
2. Removes the key from the KV column
3. Updates the tree structure (may compact nodes)
4. Recomputes affected hashes

### Querying the Root

```haskell
import CSMT.Hashes (root)

-- Get current root hash
getRootExample :: Transaction m cf d ops (Maybe ByteString)
getRootExample = root csmtCol
```

Returns `Nothing` if the tree is empty.

## Merkle Proofs

### Generating Inclusion Proofs

```haskell
import CSMT.Hashes (generateInclusionProof, fromKVHashes)

-- Generate proof for a key
proofExample :: Transaction m cf d ops (Maybe (ByteString, ByteString))
proofExample =
    generateInclusionProof fromKVHashes kvCol csmtCol "mykey"
```

Returns `Maybe (value, proofBytes)`:

- Looks up the value from the KV column
- Returns both the value and serialized proof
- Returns `Nothing` if the key doesn't exist

This ensures the proof is always consistent with the current tree state.

### Verifying Inclusion Proofs

```haskell
import CSMT.Hashes (verifyInclusionProof)

-- Verify a proof (pure function, no database access needed)
verifyExample :: ByteString -> Bool
verifyExample proofBytes = verifyInclusionProof proofBytes
```

Returns `True` if the proof is internally consistent. The proof is self-contained
with the key, value hash, and root hash embedded.

To verify against a trusted root, parse the proof and compare `proofRootHash`
with your known root.

## Custom Key/Value Types

The library supports custom types via `FromKV`:

```haskell
import CSMT.Interface (FromKV(..))

-- Define conversion for your types
myFromKV :: FromKV MyKey MyValue Hash
myFromKV = FromKV
    { fromK = myKeyToPath      -- Convert key to tree path
    , fromV = myValueToHash    -- Convert value to hash
    , treePrefix = const []    -- No prefix (default)
    }
```

The `treePrefix` field allows secondary indexing by prepending a prefix
derived from the value to the tree key. For example, to index UTxOs by
address:

```haskell
addressIndexed :: FromKV TxIn TxOut Hash
addressIndexed = FromKV
    { fromK = txInToKey
    , fromV = txOutToHash
    , treePrefix = addressToKey . extractAddress
    }
```

This makes the tree key `treePrefix(value) <> fromK(key)`, enabling
completeness proofs over all entries sharing a prefix.

## Codecs

For storage, define codecs using Prisms:

```haskell
import CSMT.Backend.Standalone (StandaloneCodecs(..))
import Control.Lens (prism')

myCodecs :: StandaloneCodecs MyKey MyValue Hash
myCodecs = StandaloneCodecs
    { keyCodec = prism' encodeKey decodeKey
    , valueCodec = prism' encodeValue decodeValue
    , nodeCodec = prism' encodeHash decodeHash
    }
```

## Column Selectors

Operations use type-safe column selectors from `CSMT.Backend.Standalone`:

```haskell
import CSMT.Backend.Standalone (Standalone(..))

-- Use directly as selectors
insert fromKVHashes StandaloneKVCol StandaloneCSMTCol key value
delete fromKVHashes StandaloneKVCol StandaloneCSMTCol key
generateInclusionProof fromKVHashes StandaloneKVCol StandaloneCSMTCol key
```

The GADT constructors serve as selectors:

- `StandaloneKVCol` - Selects the key-value column
- `StandaloneCSMTCol` - Selects the CSMT tree column

## Error Handling

Most operations return `Maybe` or are in a `Transaction` monad:

- `Nothing` typically means "key not found"
- Invalid proofs return `False` from verification
- Database errors surface as exceptions

## Performance Tips

1. **Batch operations**: Group multiple inserts/deletes in a single transaction
2. **Column family tuning**: Adjust `maxFiles` parameters for your workload
3. **Parallel insertion**: Future versions will support parallel batch inserts

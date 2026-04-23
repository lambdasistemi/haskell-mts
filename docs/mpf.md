# MPF (16-ary Trie)

The Merkle Patricia Forest is MTS's 16-ary trie implementation. It uses
hex nibble branching with Aiken-compatible hashing.

!!! info
    This page covers MPF-specific details. For the shared interface, see
    [MTS Interface](interface.md). For general concepts, see
    [Concepts](concepts.md).

## Overview

MPF provides:

- 16-ary trie with hex nibble keys (4 bits per level, depth 64 for
  32-byte keys)
- Path compression via `hexJump` fields on `HexIndirect` nodes
- Leaf/branch distinction (`hexIsLeaf` flag) for different hash schemes
- Aiken-compatible root hashes (verified against 30-fruit test vector)
- Batch, chunked, and streaming insertion modes
- Aiken-compatible inclusion and exclusion proofs
- Pure verification helpers in `MPF.Verify`
- Browser-side build/prove/verify via `mpf-write.wasm` and `mpf-verify.wasm`

## Key Types

### `HexDigit` and `HexKey`

```haskell
newtype HexDigit = HexDigit { unHexDigit :: Word8 }  -- 0-15
type HexKey = [HexDigit]
```

A `ByteString` is converted to a `HexKey` by splitting each byte into
high and low nibbles:

```haskell
byteStringToHexKey :: ByteString -> HexKey
-- byte 0xa3 -> [HexDigit 0xa, HexDigit 0x3]
```

### `HexIndirect` - Tree Nodes

```haskell
data HexIndirect a = HexIndirect
    { hexJump   :: HexKey   -- Path compression: nibbles to skip
    , hexValue  :: a         -- Hash value
    , hexIsLeaf :: Bool      -- True = leaf node, False = branch node
    }
```

The `hexIsLeaf` flag determines which hashing scheme is applied.

### `FromHexKV` - Key/Value Conversion

```haskell
data FromHexKV k v a = FromHexKV
    { fromHexK      :: k -> HexKey    -- User key to nibble path
    , fromHexV      :: v -> a          -- User value to hash
    , hexTreePrefix :: v -> HexKey     -- Optional prefix for secondary indexing
    }
```

`fromHexKVHashes` converts the raw key bytes directly to nibbles and hashes the
value. When you need Aiken/browser parity, use `fromHexKVAikenHashes`
instead: it routes the trie path through `blake2b_256(key)` rendered as 64
hex nibbles, while still storing the original user key in the KV column.

### `MPFHashing` - Hash Operations

```haskell
data MPFHashing a = MPFHashing
    { leafHash   :: HexKey -> a -> a
    , merkleRoot :: [Maybe a] -> a
    , branchHash :: HexKey -> a -> a
    }
```

## Hashing Scheme

MPF uses a specific hashing scheme for Aiken compatibility:

### Leaf Hash

```
leafHash(suffix, valueDigest):
    if even(length(suffix)):
        hashHead = 0xff
        hashTail = packHexKey(suffix)
    else:
        hashHead = 0x00 || first_nibble
        hashTail = packHexKey(remaining_nibbles)
    return blake2b(hashHead || hashTail || valueDigest)
```

### Branch Hash

```
branchHash(prefix, children):
    merkle = pairwiseReduce(children)  -- 16 slots, nullHash for missing
    return blake2b(nibbleBytes(prefix) || merkle)
```

### Merkle Root (Pairwise Reduction)

The 16-slot sparse array is reduced to a single hash by pairwise
concatenation and hashing:

```
[h0, h1, h2, ..., h15]
-> [hash(h0||h1), hash(h2||h3), ..., hash(h14||h15)]
-> [hash(h01||h23), hash(h45||h67), ..., hash(h12_13||h14_15)]
-> [hash(h0123||h4567), hash(h8_11||h12_15)]
-> hash(h0_7||h8_15)
```

Missing children use a null hash (32 zero bytes).

## Insertion Modes

MPF supports multiple insertion strategies:

| Mode | Function | Complexity | Use Case |
|------|----------|------------|----------|
| Sequential | `inserting` | O(n * depth) | Small datasets |
| Batch | `insertingBatch` | O(n log n) | Medium datasets |
| Chunked | `insertingChunked` | Bounded memory | Large datasets |
| Streaming | `insertingStream` | ~16x lower peak memory | Very large datasets |

Streaming insertion groups keys by their first hex digit, processing
each of the 16 subtrees independently.

## Proofs

### Inclusion Proofs

MPF inclusion proofs are built from proof steps that mirror Aiken's
proof-step model:

- branch steps for nodes with many siblings
- fork steps when exactly one non-empty sibling is another branch
- leaf steps when exactly one non-empty sibling is a leaf

Each step also carries the 4-hash SMT witness needed to reconstruct the
16-slot branch reduction.

The Aiken wire format emitted by `renderAikenProof` serializes the proof-step
list only. The pure verifier therefore takes the raw query key and, for
inclusion mode, the raw value as separate inputs.

### Exclusion Proofs

MPF exclusion proofs reuse the same Aiken proof-step encoding as inclusion
proofs. The distinction is carried out-of-band:

- `ptype = 0` for inclusion
- `ptype = 1` for exclusion
- `ptype = 0xff` for the empty-tree sentinel in the browser demo

This keeps the browser/WASM transport aligned with upstream Aiken instead of
inventing a second exclusion-proof payload format.

## Aiken Compatibility

MPF produces root hashes matching the Aiken `MerkleTree` reference
implementation, and the browser write path now derives key paths the same way
the pure verifier does. This is verified by the 30-fruit test vector from the
Aiken test suite:

```
Expected root: 4acd78f345a686361df77541b2e0b533f53362e36620a1fdd3a13e0b61a3b078
```

The test inserts 30 fruit key-value pairs (e.g. `"apple[uid: 58]"` ->
hash of the emoji) and verifies the resulting root hash matches exactly.

The same Aiken-parity work also backs the browser demo:

- `mpf-write.wasm` mutates the forest and emits Aiken proof-step bytes
- `mpf-verify.wasm` re-verifies those bytes against the root, raw key, and
  raw value

## Completeness Proofs

MPF completeness proofs are not yet implemented. The `mtsCollectLeaves`,
`mtsMkCompletenessProof`, and `mtsVerifyCompletenessProof` fields
currently raise an error.

## Browser Demo

For the end-to-end tutorial, see [MPF WASM Write Demo](wasm-mpf-demo.md).
That page walks through insert, delete, inclusion proof generation, exclusion
witness generation, and independent verification in the browser.

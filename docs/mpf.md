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
- Inclusion proofs with SMT proof steps

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

The default `fromHexKVHashes` hashes the key with Blake2b-256, converts
to nibbles, and hashes the value.

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

## Inclusion Proofs

MPF inclusion proofs contain:

- The key and value
- A sequence of proof steps, each with:
    - Node type (leaf or branch)
    - Sibling hash
    - SMT proof: 4 intermediate hashes from the 16-element pairwise
      reduction tree
- The root hash

The SMT proof encodes the path through the binary reduction of the
16-slot array, collecting the opposite subtree's root at each of 4
levels.

## Aiken Compatibility

MPF produces root hashes matching the Aiken `MerkleTree` reference
implementation. This is verified by the 30-fruit test vector from the
Aiken test suite:

```
Expected root: 4acd78f345a686361df77541b2e0b533f53362e36620a1fdd3a13e0b61a3b078
```

The test inserts 30 fruit key-value pairs (e.g. `"apple[uid: 58]"` ->
hash of the emoji) and verifies the resulting root hash matches exactly.

## Completeness Proofs

MPF completeness proofs are not yet implemented. The `mtsCollectLeaves`,
`mtsMkCompletenessProof`, and `mtsVerifyCompletenessProof` fields
currently raise an error.

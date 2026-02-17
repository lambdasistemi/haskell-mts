# Storage Layer

The CSMT uses a dual-column storage model with RocksDB as the backend.

## Column Families

```mermaid
graph LR
    subgraph "KV Column"
        K1[user key] --> V1[user value]
        K2[user key] --> V2[user value]
    end

    subgraph "CSMT Column"
        P1[path prefix] --> I1[Indirect]
        P2[path prefix] --> I2[Indirect]
    end
```

| Column | Key Type | Value Type | Purpose |
|--------|----------|------------|---------|
| KV | User key (`k`) | User value (`v`) | Store original key-value pairs |
| CSMT | `treePrefix(v) <> fromK(k)` | `Indirect a` | Store Merkle tree nodes |

The CSMT column key is computed by prepending `treePrefix(value)` to
`fromK(key)`. When `treePrefix = const []` (the default), this is just
`fromK(key)`. A non-trivial `treePrefix` groups entries with the same
prefix into a common subtree, enabling prefix-based completeness proofs.

## CSMT Node Storage

Each node in the CSMT is stored as a key-value pair:

- **Key**: The path prefix (list of directions from root)
- **Value**: An `Indirect` containing:
    - `jump`: Additional path to skip (path compression)
    - `value`: Hash of the node

```haskell
data Indirect a = Indirect
    { jump :: Key      -- Path compression
    , value :: a       -- Hash value
    }
```

### Example

For a tree containing keys `[L,L,R,R]` and `[L,R,R,L]`:

```
CSMT Column:
  []      -> Indirect { jump = [L], value = hash1 }
  [L,L]   -> Indirect { jump = [R,R], value = leafHash1 }
  [L,R]   -> Indirect { jump = [R,L], value = leafHash2 }
```

The root node at `[]` has `jump = [L]` because both keys share the `L` prefix.

## Path Compression

The `jump` field enables path compression. Instead of storing every node along
a path, we store only the nodes where the tree branches, with `jump` indicating
how many levels to skip.

```mermaid
graph TD
    subgraph "Without Compression"
        R1[Root] --> L1[L]
        L1 --> LL[LL]
        LL --> LLR[LLR]
        LLR --> LLRR[LLRR - leaf]
    end

    subgraph "With Compression"
        R2["[] jump=[L]"] --> LL2["[L,L] jump=[R,R] - leaf"]
    end
```

## Preimage Storage (KV Column)

The KV column stores the original key-value pairs, enabling:

- Retrieval of original values given a key
- Proof generation (needs both key and value — the value is required to
  compute `treePrefix(value)` for the tree key)
- Value lookup after proof verification

```
KV Column:
  "mykey"   -> "myvalue"
  "another" -> "data"
```

## Hash Composition

The Merkle tree hash is computed using Blake2b-256. The `Hashing` record defines
how hashes are combined:

```haskell
data Hashing a = Hashing
    { rootHash :: Indirect a -> a
    , combineHash :: Indirect a -> Indirect a -> a
    }
```

### Root/Leaf Hash

For a single node (leaf or root), the hash is computed by serializing the
entire `Indirect` structure:

```
rootHash(Indirect{jump, value}) = blake2b(serialize(jump) ++ serialize(value))
```

This means the jump path is **included in the hash**, ensuring path compression
doesn't change the Merkle root.

### Combining Child Hashes

For internal nodes, the parent hash combines both children's `Indirect` values:

```
combineHash(left, right) = blake2b(serialize(left) ++ serialize(right))
```

Both the jump paths and hash values of children contribute to the parent hash.

### Direction-Based Ordering

The `addWithDirection` function determines child ordering based on the
direction taken to reach the current node:

```haskell
addWithDirection hashing L left right = combineHash left right
addWithDirection hashing R left right = combineHash right left
```

- Direction `L`: current node is on the left, sibling on right
- Direction `R`: current node is on the right, sibling on left

## Serialization

Keys and Indirect values are serialized to ByteStrings for storage.

### Key/Jump Encoding

Keys (and jumps, which are key infixes) are bitstrings of arbitrary length.
We use a Word16 big-endian to encode the length in bits, followed by the bits
packed into bytes (left-aligned).

| Key/Jump    | Encoding            |
|-------------|---------------------|
| (empty)     | 0x00 0x00           |
| L           | 0x00 0x01 0x00      |
| R           | 0x00 0x01 0x80      |
| LL          | 0x00 0x02 0x00      |
| LR          | 0x00 0x02 0x40      |
| RL          | 0x00 0x02 0x80      |
| RR          | 0x00 0x02 0xc0      |
| LLLLLLLL    | 0x00 0x08 0x00      |
| RRRRRRRR    | 0x00 0x08 0xff      |
| LLLLLLLLL   | 0x00 0x09 0x00 0x00 |
| RRRRRRRRR   | 0x00 0x09 0xff 0x80 |

The bit length also determines byte length:

```haskell
bytesLength len =
    let (l, r) = len `divMod` 8
    in if r == 0 then l else l + 1
```

### ByteString/Hash Encoding

ByteStrings are stored as Word16 big-endian length followed by the bytes.
Hashes are treated as variable-length bytestrings.

| ByteString | Encoding                           |
|------------|-----------------------------------|
| (empty)    | 0x00 0x00                          |
| "a"        | 0x00 0x01 0x61                     |
| "abc"      | 0x00 0x03 0x61 0x62 0x63           |
| "hello"    | 0x00 0x05 0x68 0x65 0x6c 0x6c 0x6f |

### Node (Indirect) Encoding

Nodes are encoded as jump encoding followed by hash encoding:

| Node                              | Encoding                                     |
|-----------------------------------|----------------------------------------------|
| `{jump: "", value: "abc"}`        | 0x00 0x00 0x00 0x03 0x61 0x62 0x63           |
| `{jump: "LRL", value: "data"}`    | 0x00 0x03 0x40 0x00 0x04 0x64 0x61 0x74 0x61 |

## Backend Interface

The storage layer is abstracted through the `Standalone` GADT:

```haskell
data Standalone k v a x where
    StandaloneKVCol   :: Standalone k v a (KV k v)
    StandaloneCSMTCol :: Standalone k v a (KV Key (Indirect a))
```

This allows the same CSMT operations to work with different storage backends
(RocksDB, in-memory, etc.) by providing appropriate codecs.

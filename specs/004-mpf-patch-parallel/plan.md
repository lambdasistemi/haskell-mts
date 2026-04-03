# Implementation Plan: patchParallel for MPF

## The Hard Problem

MPF has a 3-way hash model:
- `leafHash(suffix, valueDigest)` — leaf node hash
- `merkleRoot([Maybe a])` — sparse 16-element pairwise reduction
- `branchHash(prefix, merkleRoot)` — branch node hash

When merging subtree roots after parallel replay:
- Leaf roots store VALUE hash (`hexIsLeaf=True`). Node hash
  requires `leafHash(suffix, value)`.
- Branch roots store BRANCH hash (`hexIsLeaf=False`). Node
  hash is just `hexValue`.

The `nodeHash` function from `MPF/Deletion.hs` handles this:
```haskell
nodeHash hi
    | hexIsLeaf hi = leafHash (hexJump hi) (hexValue hi)
    | otherwise = hexValue hi
```

## Implementation

### 1. Raw insert/delete (no FromHexKV)

Existing `insertingTreeOnly`/`deletingTreeOnly` take
`FromHexKV k v a` and compute tree keys internally.
patchParallel needs versions taking raw `HexKey` + value hash
(tree key already computed, bucket prefix stripped).

Files: `MPF/Insertion.hs`, `MPF/Deletion.hs`

### 2. expandToBucketDepthMPF

16-ary version of CSMT's binary expand. Split jumps at hex
digit boundaries. Recurse into 16 children for branches,
stop at leaves.

File: `MPF/Insertion.hs`

### 3. mergeSubtreeRootsMPF

The core challenge. Algorithm:
1. Read HexIndirect at each of 16^N bucket positions
2. Delete stale nodes above bucket depth
3. If 0 active → done
4. If 1 active leaf → prepend bucket prefix to jump, store
5. If 1 active branch → must recompute: read children,
   compute merkleRoot, branchHash with extended prefix
6. If multiple → build sparse 16-element array using
   nodeHash for each child, compute merkleRoot, branchHash

For bucketDigits > 1: recurse by grouping first hex digit.

File: `MPF/Insertion.hs`

### 4. MPF.Populate module

Mirror of CSMT/Populate.hs: group ops by hex prefix, strip
prefix, return independent transactions.

File: `MPF/Populate.hs` (new)

### 5. MTS integration

Wire parallel replay into MPF/MTS.hs toFull transition with
sentinel protocol.

File: `MPF/MTS.hs`

### 6. Tests

Property: parallel root hash == sequential root hash.

File: `test/MPF/PopulateSpec.hs` (new)

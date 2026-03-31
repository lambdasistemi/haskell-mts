# Implementation Plan: Compact CSMT Inclusion Proof CBOR Encoding

**Branch**: `002-compact-csmt-inclusion` | **Date**: 2026-03-31 | **Spec**: [spec.md](spec.md)

## Summary

Strip redundant fields from CSMT inclusion proofs and pack binary
directions as bits. Target: 669 bytes → ~250 bytes (63% reduction).

## Technical Context

**Language/Version**: Haskell (GHC 9.8.4)
**Dependencies**: cborg (CBOR encoding/decoding, already in use)
**Key Files**:
- `lib/csmt/CSMT/Hashes/Compact.hs` — new module
- `lib/csmt/CSMT/Hashes/CBOR.hs` — existing module (kept for backward compat)
- `lib/csmt/CSMT/Proof/Insertion.hs` — InclusionProof, ProofStep, computeRootHash
- `lib/csmt/CSMT/Interface.hs` — Direction, Key, Indirect, Hashing

## Research

### Current proof structure (669 bytes at N=1K)

```
InclusionProof {
  proofKey      :: [Direction]     -- 256 directions × ~1 byte = ~260 bytes  ← REDUNDANT
  proofValue    :: Hash            -- 32 bytes                               ← REDUNDANT
  proofRootHash :: Hash            -- 32 bytes                               ← REDUNDANT
  proofSteps    :: [ProofStep]     -- actual proof data
  proofRootJump :: [Direction]     -- root path compression
}

ProofStep {
  stepConsumed :: Int              -- bits consumed (1 byte CBOR)
  stepSibling  :: Indirect {
    jump  :: [Direction]           -- encoded as individual words (~N bytes)
    value :: Hash                  -- 32 bytes
  }
}
```

### What the verifier needs

`computeRootHash` (line 153 of Insertion.hs) uses:
1. `proofRootJump` — root node path compression
2. `proofSteps` — each step's `stepConsumed`, sibling `jump`, sibling `value`
3. `proofKey` — but verifier has this externally
4. `proofValue` — but verifier has this externally

### Compact format

```
CBOR array: [rootJumpPacked, step1, step2, ...]
  rootJumpPacked = bytes(2-byte-bitcount ++ packed-bits)
  step = [int(consumed), bytes(siblingJumpPacked), bytes(siblingHash)]
```

Bit packing: 8 directions per byte, MSB first. 2-byte big-endian length
prefix (number of bits).

### Size estimate

Per step: 1 (consumed) + 2 (jump length) + ceil(jumpLen/8) (packed jump) + 34 (hash CBOR) ≈ 38-40 bytes
With ~5-8 steps at N=1K: 190-320 bytes
Root jump: ~5-10 bytes
CBOR overhead: ~10 bytes
**Total: ~210-340 bytes** (vs 669 currently)

## Architecture

### New module: `CSMT.Hashes.Compact`

```haskell
renderCompactProof :: InclusionProof Hash -> ByteString
parseCompactProof :: Key -> Hash -> Hash -> ByteString -> Maybe (InclusionProof Hash)
```

`parseCompactProof` takes key, value, and root hash externally (the
verifier supplies these). Returns a full `InclusionProof` that can be
verified with the existing `verifyInclusionProof`.

### Internal helpers

```haskell
packKey :: Key -> ByteString      -- pack directions as bits
unpackKey :: ByteString -> Maybe Key  -- unpack bits to directions
```

### No changes to existing modules

- `CSMT.Hashes.CBOR` stays for backward compatibility
- `CSMT.Proof.Insertion` unchanged
- `CSMT.Interface` unchanged

## Implementation Phases

### Phase 1: Bit packing (packKey / unpackKey)
- Implement and test direction bit packing
- Property: `unpackKey (packKey k) == Just k` for all keys
- Edge cases: empty key, odd-length key, max-length key (256 bits)

### Phase 2: Compact CBOR encoding
- Implement `renderCompactProof` and `parseCompactProof`
- Property: round-trip preserves root hash computation
- Fruit test vectors: all 30 proofs round-trip correctly

### Phase 3: Benchmark integration
- Add compact proof size to unified benchmark
- Verify size targets (< 300 bytes at N=1K)

## Risks

- **Bit packing edge cases**: odd number of directions, empty keys. Mitigated by property tests.
- **CBOR overhead**: CBOR length prefixes add bytes. Mitigated by using definite-length encoding.
- **Future on-chain verifier**: if an Aiken CSMT verifier is written later, it must match this format. Mitigated by clear documentation of the CBOR schema.

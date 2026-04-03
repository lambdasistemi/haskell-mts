# Implementation Plan: CSMT Plutus Data Proof Format

## Plutus Data Layout

The CSMT proof maps naturally to Plutus Data. The on-chain verifier
receives the proof as a redeemer and the key, value, root hash from
the datum/transaction context.

### Format

```
Proof = Constr 0 [rootJump, step1, step2, ...]
  where
    rootJump = B<packed-bits>       -- bytestring, no length prefix needed
                                     (Aiken knows ByteArray length)
    stepN    = Constr 0 [consumed, sibJump, sibHash]
      where
        consumed = I<int>           -- bits consumed at this level
        sibJump  = B<packed-bits>   -- sibling jump path (packed directions)
        sibHash  = B<32-bytes>      -- sibling hash
```

Compared to the compact CBOR format:
- Drops the 2-byte bit-count prefix from packed keys (Plutus Data
  ByteArray carries its own length; the bit count can be derived
  from `consumed` during verification)
- Uses Constr tags instead of CBOR array framing
- Uses Plutus Data integers instead of CBOR integers

### Size estimate per step

- Constr 0 tag: 2 bytes (0xd8 0x79)
- Indefinite list begin/end: 2 bytes (0x9f + 0xff)
- consumed int: 1 byte (small int < 24)
- sibJump bytestring: 2 + ceil(jumpLen/8) bytes (header + packed bits)
- sibHash bytestring: 34 bytes (0x5820 + 32 bytes)

Per step: ~41 + ceil(jumpLen/8) bytes

At N=1K (avg 10-bit keys, ~5 steps, ~2 bytes avg jump):
  proof overhead (Constr + list): 4 bytes
  rootJump: ~4 bytes
  5 steps * ~43 bytes = ~215 bytes
  Total: ~223 bytes

This should be well under the MPF Aiken target of 426 bytes.

## Implementation Steps

### Phase 1: Haskell Plutus Data encoder/decoder

1. Create `lib/csmt/CSMT/Hashes/PlutusData.hs`
2. Implement `renderPlutusProof :: InclusionProof Hash -> ByteString`
   - Reuse the CBOR primitives from MPF/Hashes/Aiken.hs (cborTag,
     listBegin, cborBreak, cborBytes, cborUInt)
   - Extract shared CBOR primitives to a common module
3. Implement `parsePlutusProof :: Key -> Hash -> Hash -> ByteString -> Maybe (InclusionProof Hash)`
4. Pack directions without the 2-byte length prefix (on-chain, the
   verifier derives bit count from `consumed`)
5. Add to `mts.cabal` exposed-modules

### Phase 2: Tests

1. Round-trip property: for any valid proof, render to Plutus Data
   and parse back, verify against original root hash
2. Golden tests: known proofs with known Plutus Data bytes
3. Size regression: assert Plutus Data size <= MPF Aiken size at
   N=1K

### Phase 3: Aiken validator

1. Create `verifiers/aiken/` project
2. Implement CSMT inclusion proof verifier in Aiken
3. The verifier:
   - Receives proof as redeemer (Plutus Data)
   - Receives root hash from datum
   - Receives key + value hash from datum or transaction context
   - Recomputes root from proof steps
   - Asserts computed root == expected root
4. Test with Aiken's built-in test framework

### Phase 4: Benchmark integration

1. Add Plutus Data proof size column to unified benchmark
2. Report side-by-side: CSMT CBOR, CSMT compact, CSMT Plutus Data,
   MPF CBOR, MPF Aiken

## Shared CBOR Primitives

The CBOR primitives in `MPF.Hashes.Aiken` (cborTag, listBegin,
cborBreak, cborBytes, cborUInt) should be extracted to a shared
module since both MPF and CSMT Plutus Data encodings use them.

New module: `lib/common/MTS/CBOR/PlutusData.hs`

## Key Decision: Bit Packing Without Length Prefix

The compact CBOR format uses a 2-byte length prefix for packed keys
because CBOR bytestrings don't carry semantic length information
(the packed bytes may have trailing padding bits).

In Plutus Data, the verifier knows how many bits to consume from
the `consumed` field of each step. For rootJump, the verifier can
compute the remaining bits after subtracting all consumed bits from
the total key length. So the length prefix is redundant.

However, this means the Aiken verifier must track bit position
during verification. The alternative is to keep the 2-byte prefix
for simplicity at a 2-byte-per-step cost. Given the size budget
is comfortable, keeping the prefix is acceptable if it simplifies
the Aiken code.

**Decision**: Keep the 2-byte prefix for now. Optimize later if
needed. The size target is already achievable.

## Research: Aiken Blake2b Support

Aiken provides `blake2b_256` as a builtin. The CSMT hash function
concatenates serialized indirect values and hashes them. Need to
verify that Aiken can replicate the exact same hash computation:

```
combineHash(left, right) = blake2b(serialize(left) ++ serialize(right))
rootHash(indirect) = blake2b(serialize(indirect))
```

Where `serialize(Indirect jump value)` = `putKey jump ++ putSizedByteString value`

The serialization format (Key as Word16be length + packed bits +
hash bytes) must be replicated exactly in Aiken.

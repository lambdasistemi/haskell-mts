# Tasks: Compact CSMT Inclusion Proof CBOR Encoding

**Branch**: `002-compact-csmt-inclusion` | **Plan**: [plan.md](plan.md)

## Phase 1: Bit Packing

- [ ] **T1.1**: Implement `packKey :: Key -> ByteString` — pack L/R directions into bits (MSB first, 2-byte length prefix)
- [ ] **T1.2**: Implement `unpackKey :: ByteString -> Maybe Key` — inverse of packKey
- [ ] **T1.3**: Property test: `unpackKey (packKey k) == Just k` for random keys
- [ ] **T1.4**: Edge case tests: empty key, single direction, 256 directions, odd length

## Phase 2: Compact CBOR Encoding

- [ ] **T2.1**: Implement `renderCompactProof :: InclusionProof Hash -> ByteString`
- [ ] **T2.2**: Implement `parseCompactProof :: Key -> Hash -> Hash -> ByteString -> Maybe (InclusionProof Hash)`
- [ ] **T2.3**: Add module to `mts.cabal` under `csmt` library
- [ ] **T2.4**: Property test: round-trip preserves root hash (`computeRootHash hashing (parse (render proof)) == computeRootHash hashing proof`)
- [ ] **T2.5**: Fruit vector tests: all 30 fruit proofs round-trip correctly via compact encoding
- [ ] **T2.6**: Property test: `renderCompactProof` is deterministic

## Phase 3: Benchmark & Validation

- [ ] **T3.1**: Add compact proof size to unified benchmark output
- [ ] **T3.2**: Verify SC-001: average proof size < 300 bytes at N=1K
- [ ] **T3.3**: Verify SC-002: average proof size < 400 bytes at N=10K

## Dependencies

- T1.3, T1.4 depend on T1.1, T1.2
- T2.1, T2.2 depend on T1.1, T1.2
- T2.4, T2.5, T2.6 depend on T2.1, T2.2, T2.3
- T3.1 depends on T2.1
- T3.2, T3.3 depend on T3.1

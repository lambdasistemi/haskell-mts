# Implementation Plan: csmt-verify CompletenessProof codec + verifier

**Spec**: [spec.md](./spec.md)
**Issue**: https://github.com/lambdasistemi/haskell-mts/issues/153

## Architecture

```
┌──────────── csmt-core (WASM-safe, no C FFI) ────────────┐
│  CSMT.Core.Types      — Hashing, Indirect, Key (existing)│
│  CSMT.Core.Proof      — InclusionProof, ProofStep        │
│  CSMT.Core.Hash       — Hash, hashingWith                │
│  CSMT.Core.CBOR       — proof codecs                     │
│                         + encodeCompletenessProof (NEW)  │
│                         + decodeCompletenessProof (NEW)  │
│                         + renderCompletenessProof (NEW)  │
│                         + parseCompletenessProof (NEW)   │
│  CSMT.Core.Completeness — CompletenessProof type (NEW,   │
│                         moved from csmt-write)           │
│                         + foldCompletenessProof (NEW,    │
│                         moved from csmt-write, pure)     │
│                         + foldMergeOps, foldInclusionSteps│
└──────────────────────────────────────────────────────────┘
            │                              │
            ▼                              ▼
┌─────── csmt-verify (WASM) ─────┐  ┌──── csmt-write (DB) ──────┐
│  CSMT.Verify                   │  │  CSMT.Proof.Completeness  │
│  + verifyCompletenessProof NEW │  │  re-exports              │
│  re-exports CompletenessProof, │  │   CompletenessProof,     │
│   parseCompletenessProof       │  │   foldCompletenessProof  │
│                                │  │  keeps generateProof,    │
│                                │  │   collectValues,         │
│                                │  │   queryPrefix (DB-bound) │
└────────────────────────────────┘  └──────────────────────────┘
```

## Phases

### Phase 1 — Move pure parts to `csmt-core`

1. New module `CSMT.Core.Completeness` in `lib/csmt-core/`:
   - `data CompletenessProof a` (verbatim from `csmt-write`)
   - `foldCompletenessProof`, `foldMergeOps`, `foldInclusionSteps`
     (verbatim, all already pure, only need to swap
     `import CSMT.Proof.Insertion (ProofStep (..))` for
     `import CSMT.Core.Proof (ProofStep (..))`).
2. Add module to `csmt-core` `exposed-modules` in `mts.cabal`.

### Phase 2 — CBOR codec in `CSMT.Core.CBOR`

Append to `lib/csmt-core/CSMT/Core/CBOR.hs`:

```haskell
encodeCompletenessProof
    :: CompletenessProof Hash -> CBOR.Encoding
encodeCompletenessProof
    CompletenessProof{cpMergeOps, cpInclusionSteps} =
        CBOR.encodeListLen 2
            <> ( CBOR.encodeListLen
                    (fromIntegral (length cpMergeOps))
                <> foldMap encodeMergeOp cpMergeOps
               )
            <> ( CBOR.encodeListLen
                    (fromIntegral (length cpInclusionSteps))
                <> foldMap encodeProofStep cpInclusionSteps
               )
  where
    encodeMergeOp (i, j) =
        CBOR.encodeListLen 2
            <> CBOR.encodeInt i
            <> CBOR.encodeInt j

decodeCompletenessProof
    :: CBOR.Decoder s (CompletenessProof Hash)
decodeCompletenessProof = …

renderCompletenessProof
    :: CompletenessProof Hash -> ByteString
parseCompletenessProof
    :: ByteString -> Maybe (CompletenessProof Hash)
```

Symmetric with `renderProof` / `parseProof` already in the file.

### Phase 3 — Verifier in `csmt-verify`

Append to `lib/csmt-verify/CSMT/Verify.hs`:

```haskell
verifyCompletenessProof
    :: ByteString          -- trusted root bytes
    -> Key                 -- prefix
    -> [Indirect Hash]     -- leaves
    -> ByteString          -- CompletenessProof CBOR
    -> Bool
verifyCompletenessProof rootBs prefixKey leaves proofBs =
    case (parseHash rootBs, parseCompletenessProof proofBs) of
        (Just trustedRoot, Just proof) ->
            case foldCompletenessProof
                hashHashing prefixKey leaves proof of
                Just computed -> computed == trustedRoot
                Nothing -> False
        _ -> False
```

Re-export `CompletenessProof (..)` and `parseCompletenessProof` from
`CSMT.Verify`.

### Phase 4 — Slim `csmt-write/CSMT/Proof/Completeness.hs`

- Remove `data CompletenessProof`, `foldCompletenessProof`,
  `foldMergeOps`, `foldInclusionSteps`.
- Re-export them from `CSMT.Core.Completeness`.
- Keep `generateProof`, `collectValues`, `queryPrefix`
  (DB-bound, depend on `Database.KV.Transaction`).

### Phase 5 — Tests

Add to `test/CSMT/VerifySpec.hs` (or a sibling
`test/CSMT/Verify/CompletenessSpec.hs` to keep the existing file
focused on inclusion):

- **prop_completeness_roundtrip** — populate a tree, generate proof,
  render → parse → verify under the matching root, assert `True`.
- **prop_completeness_tampered_root** — same but flip root bytes,
  assert `False`.
- **prop_completeness_garbage** — random root + random bytes,
  assert `False`.
- **it_empty_prefix_empty_leaves** — empty tree, empty prefix,
  `leaves = []`, assert `True`.

Reuse `manyRandomPaths`, `insertHashes`, `hashCodecs` from
`CSMT.Test.Lib`.

## Vertical commit slicing

One commit per concern, each must compile and pass tests in
isolation (bisect-safe):

1. **chore: move CompletenessProof + foldCompletenessProof to
   csmt-core** — Phase 1 + minimal `csmt-write` re-export. Tests
   continue passing because `CSMT.Proof.Completeness` re-exports
   the moved names.
2. **feat: CBOR codec for CompletenessProof in csmt-core** —
   Phase 2. No verifier yet; can be unit-tested with a tiny
   round-trip prop on a hand-built `CompletenessProof Hash`.
3. **feat: csmt-verify exposes verifyCompletenessProof** —
   Phase 3 + Phase 5 tests. Closes the issue.

This stack is also a clean review surface: the move is pure
relocation with no behaviour change; the codec adds a new file
section; the verifier wires the two together.

## Quality gate per commit

```
nix develop --quiet -c just ci
```

Mirrors GitHub CI exactly (build, unit, lean, format-check, hlint).

## Risk / open questions

- **CBOR layout choice**. The issue says "layout decided here";
  the proposed layout (list of `[i,j]` pairs + list of ProofSteps)
  matches the existing `encodeProofStep` style. No external
  consumer locks this format yet (#243 hasn't shipped) — we own it.
- **`(Int, Int)` ranges**. Indices are leaf indices, bounded by
  number of leaves; `Int` is fine on every target (WASM uses
  64-bit `Int`).
- **No new build deps**. CBOR encoder/decoder primitives are
  already imported in `CSMT.Core.CBOR`; nothing new in
  `csmt-core`'s build-depends.

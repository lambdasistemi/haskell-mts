# Tasks: csmt-verify CompletenessProof codec + verifier

**Spec**: [spec.md](./spec.md)
**Plan**: [plan.md](./plan.md)
**Issue**: https://github.com/lambdasistemi/haskell-mts/issues/153

## Commit 1 — Move pure parts to csmt-core

Bisect-safe: tests on `CSMT.Proof.Completeness` keep passing via re-export.

- [ ] **T001** Create `lib/csmt-core/CSMT/Core/Completeness.hs`
  - Copy `CompletenessProof`, `foldCompletenessProof`,
    `foldMergeOps`, `foldInclusionSteps` from
    `lib/csmt-write/CSMT/Proof/Completeness.hs`.
  - Imports come from `CSMT.Core.Types` and `CSMT.Core.Proof`
    (`ProofStep`).
  - Module header / Haddock travel with the code.
- [ ] **T002** Wire the new module in `mts.cabal`
  - Add `CSMT.Core.Completeness` to `library csmt-core`'s
    `exposed-modules` (alphabetical: between `CBOR` and `Exclusion`).
- [ ] **T003** Slim `lib/csmt-write/CSMT/Proof/Completeness.hs`
  - Drop `CompletenessProof`, `foldCompletenessProof`,
    `foldMergeOps`, `foldInclusionSteps`.
  - Re-export them from `CSMT.Core.Completeness`.
  - Keep `generateProof`, `collectValues`, `queryPrefix`
    (DB-backed) untouched.
  - Keep `import CSMT.Proof.Insertion (ProofStep (..))` if it is
    still referenced internally; otherwise drop it.
- [ ] **T004** `nix develop --quiet -c just ci` — must pass.

## Commit 2 — CBOR codec for CompletenessProof in csmt-core

- [ ] **T005** Append to `lib/csmt-core/CSMT/Core/CBOR.hs`:
  - `encodeCompletenessProof :: CompletenessProof Hash -> CBOR.Encoding`
  - `decodeCompletenessProof :: CBOR.Decoder s (CompletenessProof Hash)`
  - `renderCompletenessProof :: CompletenessProof Hash -> ByteString`
  - `parseCompletenessProof :: ByteString -> Maybe (CompletenessProof Hash)`
  - Layout: list-of-2 — `[ list of [i,j] pairs , list of ProofStep ]`.
  - Reuse `encodeProofStep` / `decodeProofStep`.
- [ ] **T006** Update module export list with the four new names.
- [ ] **T007** `nix develop --quiet -c just ci` — must pass.

## Commit 3 — verifyCompletenessProof + tests

- [ ] **T008** Append to `lib/csmt-verify/CSMT/Verify.hs`:
  - `verifyCompletenessProof
        :: ByteString -> Key -> [Indirect Hash] -> ByteString -> Bool`
  - Re-export `CompletenessProof (..)` and `parseCompletenessProof`.
- [ ] **T009** Add tests to `test/CSMT/VerifySpec.hs` (or a new
  `test/CSMT/Verify/CompletenessSpec.hs` registered in `test/main.hs`):
  - `prop_completeness_roundtrip` — random tree, render→parse→verify.
  - `prop_completeness_tampered_root` — flipped root bytes ⇒ False.
  - `prop_completeness_garbage` — random bytes ⇒ False.
  - `it_empty_prefix_empty_leaves` — empty tree, `[]` leaves ⇒ True.
- [ ] **T010** Empty-leaves edge case: ensure
  `foldCompletenessProof` on `leaves = []` is well-defined or
  document the precondition. The current implementation falls
  through to `foldMergeOps` with `[]` which returns
  `Just (Map.! 0)` undefined-key — fix needed if test reveals
  this. (Inspect first; only patch if broken.)
- [ ] **T011** `nix develop --quiet -c just ci` — must pass.
- [ ] **T012** Push branch, open draft PR linking #153 and #243.
- [ ] **T013** Update PR body with stack summary (one bullet per
  commit), list of new exports, link to spec/plan/tasks files in
  the PR head.

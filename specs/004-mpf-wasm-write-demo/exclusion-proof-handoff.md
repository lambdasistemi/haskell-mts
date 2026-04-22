# MPF Exclusion Proof Parallel Handoff

## Context

This handoff is for the missing MPF exclusion-proof path needed by
[#146](https://github.com/lambdasistemi/haskell-mts/issues/146).

Dedicated blocker issue:

- [#148](https://github.com/lambdasistemi/haskell-mts/issues/148) -
  `Add MPF exclusion proof generation and verification`

Current status on this branch:

- Phase 1 is done: MPF hashes were rerouted through the pure Blake2b path.
- Phase 2 is done: `mpf-write` was split out as the pure sublibrary.
- PR checkpoint: [#147](https://github.com/lambdasistemi/haskell-mts/pull/147)
- Verified checkpoint commit: `f4d3201`

The remaining browser/WASM mirror work from `#146` is blocked on a real
MPF exclusion-proof API. The CSMT write demo protocol already expects the
response envelope to carry `ptype = 0 / 1 / 0xff`, where `1` is exclusion.

## What Already Exists

### MPF inclusion proofs

- Generator: [lib/mpf-write/MPF/Proof/Insertion.hs](/code/haskell-mts-issue-146/lib/mpf-write/MPF/Proof/Insertion.hs:115)
- Verifier: [lib/mpf-write/MPF/Proof/Insertion.hs](/code/haskell-mts-issue-146/lib/mpf-write/MPF/Proof/Insertion.hs:468)
- MTS wiring: [lib/mpf-write/MPF/MTS.hs](/code/haskell-mts-issue-146/lib/mpf-write/MPF/MTS.hs:271)

Important current behavior:

- `mkMPFInclusionProof` is membership-only.
- Missing keys do not produce a first-class witness proof.
- `verifyMPFInclusionProof` requires a value and checks membership only.

### Aiken inclusion-proof parity

The current MPF inclusion-proof wire format is not just internal CBOR. It
is intended to match the Aiken / off-chain reference bytes.

Primary codec:

- [lib/mpf-write/MPF/Hashes/Aiken.hs](/code/haskell-mts-issue-146/lib/mpf-write/MPF/Hashes/Aiken.hs:1)

Concrete format constraints already encoded there:

- Proof steps are Plutus Data constructors using CBOR tags `121`, `122`,
  `123` for branch, fork, leaf.
- The top-level proof is an indefinite-length list of steps only.
- Steps are serialized top-down, so `renderAikenProof` reverses the
  bottom-up `mpfProofSteps`.
- Branch neighbor bytes are encoded as an indefinite bytestring split into
  `2 x 64` byte chunks.

Existing parity tests:

- [test/MPF/Hashes/AikenSpec.hs](/code/haskell-mts-issue-146/test/MPF/Hashes/AikenSpec.hs:36)
- [test/MPF/ProofCompatSpec.hs](/code/haskell-mts-issue-146/test/MPF/ProofCompatSpec.hs:1)

What those tests already guarantee:

- Exact byte parity for known upstream JS vectors (`mango`, `kumquat`).
- Round-trip parse/render for all 30 fruit proofs.
- Constructor-shape preservation through parse/render.

### CSMT prior art

CSMT already has the full exclusion-proof stack:

- Proof type and CBOR codec:
  [lib/csmt-core/CSMT/Core/CBOR.hs](/code/haskell-mts-issue-146/lib/csmt-core/CSMT/Core/CBOR.hs:118)
- Write-side generator:
  [lib/csmt-write/CSMT/Proof/Exclusion.hs](/code/haskell-mts-issue-146/lib/csmt-write/CSMT/Proof/Exclusion.hs:1)
- Browser write protocol using `ptype = 0 / 1`:
  [app/csmt-write-wasm/Main.hs](/code/haskell-mts-issue-146/app/csmt-write-wasm/Main.hs:290)
  and [verifiers/browser-write/write.js](/code/haskell-mts-issue-146/verifiers/browser-write/write.js:256)

There is also an earlier product spec for CSMT exclusion proofs:

- [specs/002-exclusion-proof/spec.md](/code/haskell-mts-issue-146/specs/002-exclusion-proof/spec.md:1)

## What Is Missing

There is no MPF equivalent yet for:

- a first-class exclusion-proof type
- an exclusion-proof generator for absent keys
- a pure verifier for non-membership
- a stable serialized wire format for exclusion proofs
- MTS-level routing that can distinguish inclusion from exclusion

Right now, absent keys effectively collapse to "no proof":

- [test/MPF/Proof/InsertionSpec.hs](/code/haskell-mts-issue-146/test/MPF/Proof/InsertionSpec.hs:45)

## Research Constraint: Aiken Proof Format Parity

This work must preserve the current inclusion-proof parity story.

Non-negotiable:

- Do not change the bytes produced by `renderAikenProof` for existing
  inclusion proofs.
- Do not weaken or delete the current Aiken parity tests.

Upstream research result:

- The upstream Aiken / `merkle-patricia-forestry` stack does support
  non-membership, but not via a distinct serialized exclusion-proof type.
- It uses the same `Proof = List<ProofStep>` for both inclusion and
  exclusion, and changes semantics at verification time.

Primary upstream references:

- On-chain proof type and `miss`:
  https://github.com/aiken-lang/merkle-patricia-forestry/blob/main/on-chain/lib/aiken/merkle-patricia-forestry.ak
- Off-chain `trie.prove(key, allowMissing)` and `Proof.verify(false)`:
  https://github.com/aiken-lang/merkle-patricia-forestry/blob/main/off-chain/lib/trie.js
- Off-chain docs for missing-key proofs:
  https://github.com/aiken-lang/merkle-patricia-forestry/blob/main/off-chain/README.md

Concrete upstream behavior:

- `trie.prove(key, true)` can build a proof for a missing key.
- Off-chain, exclusion is represented by the same proof path plus an
  undefined value.
- On-chain, `miss(self, key, proof)` accepts the same `Proof` type used by
  `has`, `insert`, and `delete`.
- Upstream `toCBOR()` serializes only the proof steps. The mode
  distinction is not encoded in the proof bytes themselves.

Implication for the Haskell work:

- If we want Aiken proof-format parity, the best default assumption is not
  "invent a brand new exclusion-proof wire format".
- The stronger parity target is: generate a missing-key proof whose
  step-list serializes through the existing Aiken step codec and is
  verifiable in exclusion mode.
- The browser/WASM protocol can still carry `ptype = 1`; that tag can stay
  outside the proof bytes, matching the current demo envelope design.

## Likely Witness Material Already Present

The current inclusion proof step constructors already carry enough local
shape to make non-membership plausible:

- `ProofStepLeaf` records a neighboring leaf path and value digest
- `ProofStepFork` records neighboring branch prefix plus merkle root
- `ProofStepBranch` records sparse sibling hashes for the branch

Relevant code:

- [lib/mpf-write/MPF/Proof/Insertion.hs](/code/haskell-mts-issue-146/lib/mpf-write/MPF/Proof/Insertion.hs:47)

That does not mean an exclusion proof already exists. It only means there
is likely reusable witness structure for designing one.

## Expected Deliverables

1. Track implementation in [#148](https://github.com/lambdasistemi/haskell-mts/issues/148).
2. Define an MPF exclusion-proof type with at least:
   - empty-tree case
   - populated-tree witness case
3. Implement generation for absent keys covering:
   - empty tree
   - divergence before or within root prefix
   - missing child at a branch point
   - divergence inside a compressed prefix
   - single-leaf witness cases
4. Implement pure verification without tree access.
5. Add serialization for transport into the write/verify WASM protocol.
6. Add focused tests for success, rejection, and tamper cases.
7. Keep inclusion-proof Aiken parity green throughout.

## Acceptance Criteria For The Delegated Work

- Existing Aiken inclusion-proof tests still pass unchanged.
- Existing MPF inclusion verification still behaves the same.
- Absent keys produce a verifiable missing-key proof instead of `Nothing`.
- Present keys do not produce exclusion proofs.
- Verification succeeds for valid exclusion proofs and fails for tampered
  proofs.
- If the delegated design claims Aiken parity for exclusion too, it must
  demonstrate that the step serialization matches upstream expectations for
  missing-key proofs, not just inclusion proofs.
- `#146` can then consume the proof through the same `ptype = 1` browser
  protocol used by the CSMT demo.

## Suggested First Checks

1. Start from the upstream semantic model: same proof-step list, different
   verification mode.
2. Decide whether the Haskell API needs a separate sum type for MTS/demo
   plumbing even if the serialized proof steps stay shared.
3. Keep the Aiken parity scope narrow: inclusion parity is already proven;
   do not accidentally rewrite that format while adding exclusion support.

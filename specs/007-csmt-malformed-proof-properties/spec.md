# Feature Specification: malformed-proof properties for CSMT verification

**Issue**: https://github.com/lambdasistemi/haskell-mts/issues/172
**Sibling**: paolino/aiken-csmt#10 (merged, v0.1.0)

## User story

As someone who checks a CSMT proof I did not build, I want the Haskell verifier and the TypeScript verifier that
mirrors it to refuse every malformed proof, for a key the tree holds as well as for one it does not, and to accept
every honest proof, exactly as the Aiken verifier in paolino/aiken-csmt does since v0.1.0.

## Read from the source (to be reproduced as failing properties first)

- `CSMT.Core.Proof.foldProof` drops `length proofRootJump` bits of the proof key and hashes the root jump without
  comparing it with the key's leading bits; nothing requires the steps to consume the rest of the key.
- `stepConsumed = 0` (or a negative value) reaches `error`; a value past the remaining key is truncated by `splitAt`.
- `CSMT.Core.Exclusion.verifyExclusionProof _ _ ExclusionEmpty = True` ignores the trusted root.
- `verifiers/typescript` `computeRootHash` has the same structure and throws on `stepConsumed = 0`; its keys are
  numeric arrays, so a decoded direction other than 0 or 1 reaches the serialization (the Aiken carry defect class).

The Aiken work found two more classes that must be checked here, not assumed absent: a collapsing branch dropping key
bits (Aiken's exclusion path; the Haskell exclusion proof is a witness proof, a different construction) and non-binary
path elements aliasing a held key's path once packed.

## Requirements

| name | statement |
|---|---|
| inclusion-rejects-false-statements | `verifyInclusionProof root p` is False whenever the tree with root `root` does not hold `proofKey p -> proofValue p`, for every `p` with every field drawn over its whole domain |
| exclusion-rejects-held-key | `verifyExclusionProof root e` is False for every `e` whose target key the tree holds; `ExclusionEmpty` is accepted only for the empty root |
| refusal-not-exception | malformed proofs (any `stepConsumed`, any path value, any decoded bytes) are refused with False or a decoding `Left`, never by an exception |
| honest-proofs-accepted | every proof produced by `generateProof`/`buildInclusionProof` for random trees verifies, in Haskell and in TypeScript |
| typescript-parity | the TypeScript verifier accepts and refuses the same proofs as the Haskell one over shared vectors |
| aiken-parity | the same statements are accepted and refused by the Aiken verifier (paolino/aiken-csmt v0.1.0) over the shared vectors |
| range-check-kills-relaxation | each check added, removed or relaxed, makes a property fail; this runs in CI |
| failing-case-kept | each counterexample found is a named test |
| case-count-recorded | properties run in CI with a fixed seed and case count, recorded in the PR |

Trees hold keys of one length (the same obligation aiken-csmt documents: leaf and branch hashes are not
domain-separated). Proof formats do not change.

## Non-goals

Changing the hash format (aiken-csmt #12), packed paths (aiken-csmt #13), MPF, completeness proofs beyond what
`verifyExclusionProof`/`verifyInclusionProof` need, Singular.

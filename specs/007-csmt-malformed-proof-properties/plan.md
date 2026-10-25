# Plan: issue #172

Class: changes behaviour (proof verification). Topology: OWNER, one commit owner (Claude Sonnet 5.5 xhigh) and one
persistent mute auditor per slice. Auditor seat: Muse (muse-spark-1.3-contributor xhigh), the operator's fallback seat
after the Codex auditor stalled twice on the provider's content filter during aiken-csmt #10.

## Slices (each under two hours)

| slice | content | depends on |
|---|---|---|
| haskell-inclusion | QuickCheck generators near honest proofs over every field's domain; root jump bound to the key, full consumption, `stepConsumed` refused out of range without `error`; completeness over random trees; kept counterexamples | none |
| haskell-exclusion | `ExclusionEmpty` bound to the empty root; witness-path properties for held keys (including divergence at branch positions and keys of other lengths); exclusion completeness; Plutus-data / CBOR proof decoding refuses malformed bytes | haskell-inclusion |
| typescript-mirror | the same checks in verifiers/typescript (verify.ts, exclusion.ts, decoding), with seeded generated tests in vitest; non-binary decoded directions refused | haskell-exclusion |
| parity-vectors | `csmt-test-vectors` emits a language-neutral vector file (statement, proof, expected verdict; honest and malformed, including the counterexamples kept by aiken-csmt #10 and by this ticket) and its Aiken rendering; Haskell and TypeScript test suites read it in CI | typescript-mirror |
| relaxation-check | planted relaxations of every added check run in CI for Haskell and TypeScript; fixed seed and case count | parity-vectors |

The aiken-csmt side of `aiken-parity` (pin bump, regenerate, the Aiken rendering of the parity cases) is a small PR
in paolino/aiken-csmt after this ticket merges.

## Gate (per slice, from .github/workflows/CI.yaml)

`nix --quiet build .#mts`; `nix --quiet run .#unit-tests`; `nix --quiet run ./verifiers/typescript#test`;
`nix --quiet develop -c just format-check`; `nix --quiet develop -c just hlint`; `nix --quiet develop -c cabal check`;
each in `systemd-run --user --scope --unit=csmt-hmts-<what>-<n> -p MemoryMax=16G timeout 3600 ...`. Hosted CI on the
pushed head is the net. Rows added by later slices are "CI change in this ticket".

## Open questions for plan review

1. Is the TypeScript verifier in scope as the issue says (yes per the issue), and is a new dev dependency (fast-check)
   acceptable, or seeded generators inside vitest only? Recommendation: seeded generators, no new dependency.
2. The aiken-csmt parity follow-up PR: in this ticket's acceptance, or a separate small ticket? Recommendation: a
   separate PR owned by this ticket owner right after merge, listed in this PR's body.

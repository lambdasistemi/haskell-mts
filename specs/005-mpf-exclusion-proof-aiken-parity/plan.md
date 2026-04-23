# Implementation Plan: MPF Exclusion Proofs with Aiken Parity

## Design Choice

Use a dedicated Haskell exclusion-proof type, but keep the proof payload
itself on the existing `MPFProofStep` constructors. This preserves the
current Aiken proof-step codec while avoiding a broad `MTS.Interface`
change in the first patch.

## Proof Model

```haskell
data MPFExclusionProof a
  = MPFExclusionEmpty { mpeTargetKey :: HexKey }
  | MPFExclusionWitness
      { mpeTargetKey :: HexKey
      , mpeProofSteps :: [MPFProofStep a]
      }
```

The wire-format constraint is satisfied because the serializable payload
for populated proofs remains `mpeProofSteps`.

## Generation Strategy

Walk the existing trie while following the target key:

1. No root: return `MPFExclusionEmpty`
2. Root jump diverges: synthesize a terminal `Fork` or `Leaf` proof step
   describing the existing root as the witness
3. Existing branch path matches:
   - missing child: finish with a `Branch` step
   - child jump diverges: finish with a terminal `Fork` or `Leaf` step
   - child jump matches: recurse and append the current branch step
4. Exact existing leaf path: return `Nothing`

## Verification Strategy

Verification has two checks:

1. The target key must be structurally compatible with the proof path
2. Folding the proof in exclusion mode must reproduce the trusted root

The fold carries `Maybe a` for the target subtree:

- `Nothing` means "the target subtree is absent here"
- `Just h` means "a deeper witness subtree reconstructed to node hash `h`"

`Leaf` and `Fork` steps collapse to the single neighbor node when the
target subtree is absent. `Branch` steps rebuild the branch root with the
target position left empty.

## Scope Of First Patch

- Create spec artifacts for `#149`
- Add `MPF.Proof.Exclusion`
- Add focused unit tests in the pure backend
- Expose the new module through `mpf-write`, `mpf`, and the MPF test lib

## Deferred Work

- Widen `MTS.Interface` so inclusion and exclusion can share a top-level
  proof type
- Thread `ptype = 1` through `MPF.MTS` and the browser/WASM write path
- Add explicit upstream JS exclusion vectors once the core proof shape is
  stable

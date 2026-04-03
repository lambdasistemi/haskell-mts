# Implementation Plan: Exclusion Proof for CSMT

## How exclusion works in a CSMT

A CSMT with path compression stores `Indirect { jump, value }` at
each node. When a key is absent, traversal hits one of two cases:

1. **No root** — tree is empty, trivially excluded
2. **Jump diverges** — at some node, the jump path disagrees with
   the target key's bits. Since jumps represent path compression
   (no branching exists within a jump), the target key has nowhere
   to go — it cannot exist in the tree.

The witness is any leaf reachable from the divergence node. Its
inclusion proof authenticates the tree structure from root through
the divergence point.

## Exclusion proof structure

Minimal: just the target key plus a standard inclusion proof for
the witness. No divergence index needed — the verifier finds it
during the fold.

```
data ExclusionProof a
  = ExclusionEmpty
  | ExclusionWitness
      { epTargetKey :: Key
      , epWitnessProof :: InclusionProof a
      }
```

## Verification: single fold with divergence callback

The key insight: inclusion proof verification walks from leaf to
root, combining hashes at each step. The divergence check is
embedded into this same walk as a callback.

### Generalized proof fold

The current `computeRootHash` folds proof steps accumulating
only a hash. We generalize it to carry an extra accumulator
that a callback updates at each step:

```
foldProof
  :: Hashing a
  -> InclusionProof a
  -> (acc -> Key -> Key -> acc)
  -- ^ callback: accumulator, witness step bits, target step bits
  -> acc
  -> (a, acc)
  -- ^ (root hash, final accumulator)
```

At each step, the fold extracts the consumed key bits for both
the witness key and the target key, and passes them to the
callback. The callback can compare them to detect divergence.

### Exclusion callback

The callback tracks divergence state:

- **No divergence yet**: compare witness and target bits in the
  jump region (skip the direction bit — that's a branch boundary).
  If they diverge within the jump → record divergence.
  If they diverge at the direction bit → reject (branch boundary,
  target could exist on the other side).
- **Divergence already found**: no-op, just keep folding to
  finish the root hash computation.

### Verification result

After the fold completes:
- Root hash must match the trusted root
- Divergence must have been found within a jump
- Both conditions from the same fold

### Root jump divergence

The root jump is checked separately before the step fold starts.
If the target key diverges within the root jump, exclusion is
proven without needing any steps.

## Generation algorithm

1. Query root. If empty → `ExclusionEmpty`
2. Walk the tree following the target key path:
   - At each node, check if the jump is a prefix of the remaining
     target key bits
   - If YES: consume the jump, take the direction bit, continue
   - If NO: divergence found. Follow the tree downward from here
     (always taking left child) to find a leaf. Generate a
     standard inclusion proof for that leaf.
3. If the walk completes without divergence, the key exists →
   return Nothing

## Files to create/modify

- **Modify**: `lib/csmt/CSMT/Proof/Insertion.hs` — generalize
  `computeRootHash` into `foldProof` with callback
- **Create**: `lib/csmt/CSMT/Proof/Exclusion.hs` — exclusion
  proof type, generation, verification (using `foldProof`)
- **Create**: `test/CSMT/Proof/ExclusionSpec.hs` — tests
- **Modify**: `mts.cabal` — add new modules

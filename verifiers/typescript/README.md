# @paolino/csmt-verify

TypeScript library for verifying CSMT (Compact Sparse Merkle Tree) inclusion proofs.

## Installation

```bash
npm install @paolino/csmt-verify
```

## Usage

```typescript
import { parseProof, verifyInclusionProof, verifyProofBytes } from '@paolino/csmt-verify';

// Verify from raw CBOR bytes
const proofBytes = new Uint8Array([...]); // CBOR-encoded proof
const isValid = verifyProofBytes(proofBytes);

// Or parse and verify separately
const proof = parseProof(proofBytes);
const isValid = verifyInclusionProof(proof);
```

## API

### `verifyProofBytes(bytes: Uint8Array): boolean`

Parse and verify a CBOR-encoded inclusion proof in one call.

### `parseProof(bytes: Uint8Array): InclusionProof`

Parse a CBOR-encoded proof into a structured object.

### `verifyInclusionProof(proof: InclusionProof): boolean`

Verify that a parsed proof is valid.

### `computeRootHash(proof: InclusionProof): Hash`

Compute the root hash from a proof's components.

## Types

```typescript
type Direction = 0 | 1;  // L=0, R=1
type Key = Direction[];
type Hash = Uint8Array;  // 32 bytes (Blake2b-256)

interface Indirect {
  jump: Key;
  value: Hash;
}

interface ProofStep {
  stepConsumed: number;
  stepSibling: Indirect;
}

interface InclusionProof {
  proofKey: Key;
  proofValue: Hash;
  proofRootHash: Hash;
  proofSteps: ProofStep[];
  proofRootJump: Key;
}
```

## License

Apache-2.0

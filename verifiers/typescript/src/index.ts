/**
 * CSMT Proof Verifier
 *
 * TypeScript library for verifying Compact Sparse Merkle Tree inclusion proofs.
 *
 * @example
 * ```typescript
 * import { parseProof, verifyInclusionProof } from '@paolino/csmt-verify';
 *
 * // Parse CBOR-encoded proof
 * const proof = parseProof(proofBytes);
 *
 * // Verify the proof is internally consistent
 * const isValid = verifyInclusionProof(proof);
 *
 * // Also verify against trusted root
 * if (isValid && arraysEqual(proof.proofRootHash, trustedRootHash)) {
 *   console.log('Proof is valid!');
 * }
 * ```
 */

// Types
export type {
    Direction,
    Hash,
    InclusionProof,
    Indirect,
    Key,
    ProofStep,
} from "./types";
export { L, R } from "./types";

// CBOR parsing
import { parseProof } from "./cbor";
export { parseProof };

// Verification
import { arraysEqual, computeRootHash, verifyInclusionProof } from "./verify";
export { arraysEqual, computeRootHash, verifyInclusionProof };

// Exclusion proof verification
export type { ExclusionProof } from "./exclusion";
export { verifyExclusionProof } from "./exclusion";

// Hashing utilities (for advanced use)
export { blake2b256, combineHash, rootHash } from "./hash";

// Serialization utilities (for advanced use)
export {
    concat,
    serializeIndirect,
    serializeKey,
    serializeSizedBytes,
} from "./serialize";

/**
 * Parse and verify a proof in one call
 *
 * @param bytes - CBOR-encoded inclusion proof
 * @returns true if the proof is internally consistent, false otherwise
 * @throws Error if the bytes cannot be parsed as a valid proof
 */
export function verifyProofBytes(bytes: Uint8Array): boolean {
    const proof = parseProof(bytes);
    return verifyInclusionProof(proof);
}

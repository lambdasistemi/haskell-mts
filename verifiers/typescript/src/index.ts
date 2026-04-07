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
 * // Verify the proof against a trusted root
 * const isValid = verifyInclusionProof(trustedRootHash, proof);
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

import type { Hash } from "./types";

// CBOR parsing
import { parseExclusionProof, parseProof } from "./cbor";
export { parseExclusionProof, parseProof };

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
 * @param trustedRoot - The trusted root hash to verify against
 * @param bytes - CBOR-encoded inclusion proof
 * @returns true if the proof verifies against the trusted root
 * @throws Error if the bytes cannot be parsed as a valid proof
 */
export function verifyProofBytes(
    trustedRoot: Hash,
    bytes: Uint8Array,
): boolean {
    const proof = parseProof(bytes);
    return verifyInclusionProof(trustedRoot, proof);
}

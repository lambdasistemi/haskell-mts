/**
 * CBOR parsing for inclusion proofs
 *
 * Parses CBOR-encoded proofs matching the CDDL specification:
 * - inclusion_proof = [key, hash, hash, [* proof_step], key]
 * - proof_step = [int, indirect]
 * - indirect = [key, hash]
 * - key = [* direction]
 */
import { decode } from "cbor-x";
import type {
    Direction,
    Hash,
    InclusionProof,
    Indirect,
    Key,
    ProofStep,
} from "./types";
import { L, R } from "./types";

/**
 * Parse a Direction from a CBOR integer
 */
function parseDirection(value: unknown): Direction {
    if (value === 0) return L;
    if (value === 1) return R;
    throw new Error(`Invalid direction: ${value}`);
}

/**
 * Parse a Key from a CBOR array of directions
 */
function parseKey(value: unknown): Key {
    if (!Array.isArray(value)) {
        throw new Error("Key must be an array");
    }
    return value.map(parseDirection);
}

/**
 * Parse a Hash from a CBOR byte string
 */
function parseHash(value: unknown): Hash {
    if (!(value instanceof Uint8Array)) {
        throw new Error("Hash must be a byte string");
    }
    if (value.length !== 32) {
        throw new Error(`Hash must be 32 bytes, got ${value.length}`);
    }
    return value;
}

/**
 * Parse an Indirect from a CBOR array [key, hash]
 */
function parseIndirect(value: unknown): Indirect {
    if (!Array.isArray(value) || value.length !== 2) {
        throw new Error("Indirect must be an array of 2 elements");
    }
    return {
        jump: parseKey(value[0]),
        value: parseHash(value[1]),
    };
}

/**
 * Parse a ProofStep from a CBOR array [int, indirect]
 */
function parseProofStep(value: unknown): ProofStep {
    if (!Array.isArray(value) || value.length !== 2) {
        throw new Error("ProofStep must be an array of 2 elements");
    }
    const stepConsumed = value[0];
    if (typeof stepConsumed !== "number" || !Number.isInteger(stepConsumed)) {
        throw new Error("stepConsumed must be an integer");
    }
    return {
        stepConsumed,
        stepSibling: parseIndirect(value[1]),
    };
}

/**
 * Parse an InclusionProof from CBOR bytes
 *
 * Format: [proofKey, proofValue, proofRootHash, proofSteps, proofRootJump]
 */
export function parseProof(bytes: Uint8Array): InclusionProof {
    const decoded = decode(bytes);

    if (!Array.isArray(decoded) || decoded.length !== 5) {
        throw new Error("InclusionProof must be an array of 5 elements");
    }

    const [rawKey, rawValue, rawRootHash, rawSteps, rawRootJump] = decoded;

    if (!Array.isArray(rawSteps)) {
        throw new Error("proofSteps must be an array");
    }

    return {
        proofKey: parseKey(rawKey),
        proofValue: parseHash(rawValue),
        proofRootHash: parseHash(rawRootHash),
        proofSteps: rawSteps.map(parseProofStep),
        proofRootJump: parseKey(rawRootJump),
    };
}

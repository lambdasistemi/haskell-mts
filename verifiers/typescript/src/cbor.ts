/**
 * CBOR parsing for inclusion and exclusion proofs
 *
 * Inclusion proof CDDL:
 * - inclusion_proof = [key, hash, [* proof_step], key]
 * - proof_step = [int, indirect]
 * - indirect = [key, hash]
 * - key = [* direction]
 *
 * Exclusion proof CDDL:
 * - exclusion_proof = [0] / [1, key, inclusion_proof]
 */
import { decode } from "cbor-x";
import type { ExclusionProof } from "./exclusion";
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
 * Format: [proofKey, proofValue, proofSteps, proofRootJump]
 */
export function parseProof(bytes: Uint8Array): InclusionProof {
    const decoded = decode(bytes);

    if (!Array.isArray(decoded) || decoded.length !== 4) {
        throw new Error("InclusionProof must be an array of 4 elements");
    }

    const [rawKey, rawValue, rawSteps, rawRootJump] = decoded;

    if (!Array.isArray(rawSteps)) {
        throw new Error("proofSteps must be an array");
    }

    return {
        proofKey: parseKey(rawKey),
        proofValue: parseHash(rawValue),
        proofSteps: rawSteps.map(parseProofStep),
        proofRootJump: parseKey(rawRootJump),
    };
}

/**
 * Parse an InclusionProof from a raw CBOR-decoded array
 * (used internally by parseExclusionProof)
 */
function parseInclusionProofFromDecoded(decoded: unknown): InclusionProof {
    if (!Array.isArray(decoded) || decoded.length !== 4) {
        throw new Error("InclusionProof must be an array of 4 elements");
    }

    const [rawKey, rawValue, rawSteps, rawRootJump] = decoded;

    if (!Array.isArray(rawSteps)) {
        throw new Error("proofSteps must be an array");
    }

    return {
        proofKey: parseKey(rawKey),
        proofValue: parseHash(rawValue),
        proofSteps: rawSteps.map(parseProofStep),
        proofRootJump: parseKey(rawRootJump),
    };
}

/**
 * Parse an ExclusionProof from CBOR bytes
 *
 * Format: [0] for empty, [1, targetKey, inclusionProof] for witness
 */
export function parseExclusionProof(bytes: Uint8Array): ExclusionProof {
    const decoded = decode(bytes);

    if (!Array.isArray(decoded) || decoded.length < 1) {
        throw new Error("ExclusionProof must be a non-empty array");
    }

    const tag = decoded[0];

    if (tag === 0) {
        return { tag: "empty" };
    }

    if (tag === 1) {
        if (decoded.length !== 3) {
            throw new Error(
                "ExclusionWitness must be [1, targetKey, inclusionProof]"
            );
        }
        return {
            tag: "witness",
            targetKey: parseKey(decoded[1]),
            witnessProof: parseInclusionProofFromDecoded(decoded[2]),
        };
    }

    throw new Error(`Invalid exclusion proof tag: ${tag}`);
}

/**
 * Direction in the CSMT binary tree path
 * L (0) = left branch, R (1) = right branch
 */
export type Direction = 0 | 1;
export const L: Direction = 0;
export const R: Direction = 1;

/**
 * Key is a sequence of directions representing a path in the tree
 */
export type Key = Direction[];

/**
 * Hash is a 32-byte Blake2b-256 hash
 */
export type Hash = Uint8Array;

/**
 * Indirect reference: a jump path and a value
 * If jump is empty, value is at current node
 * If jump is non-empty, value is at descendant reached by following jump
 */
export interface Indirect {
    jump: Key;
    value: Hash;
}

/**
 * Single step in an inclusion proof
 * stepConsumed = 1 (direction) + length(stepJump)
 */
export interface ProofStep {
    /** Number of key bits consumed (1 for direction + jump length) */
    stepConsumed: number;
    /** Sibling node's indirect reference */
    stepSibling: Indirect;
}

/**
 * Complete inclusion proof with all data needed for verification
 */
export interface InclusionProof {
    /** The key being proven */
    proofKey: Key;
    /** Hash of the value at the key */
    proofValue: Hash;
    /** Steps from leaf to root */
    proofSteps: ProofStep[];
    /** Jump path at the root node */
    proofRootJump: Key;
}

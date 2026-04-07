/**
 * Exclusion proof verification
 *
 * Verifies that a target key does NOT exist in the CSMT.
 * The proof embeds a witness inclusion proof whose path
 * diverges from the target key within a jump (path
 * compression region).
 */
import type { Hash, InclusionProof, Key } from "./types";
import { verifyInclusionProof } from "./verify";

/**
 * Exclusion proof: either the tree is empty, or a witness
 * key diverges from the target within a jump.
 */
export type ExclusionProof =
    | { tag: "empty" }
    | { tag: "witness"; targetKey: Key; witnessProof: InclusionProof };

/**
 * Verify an exclusion proof against a trusted root hash.
 *
 * For empty: always true (caller should check root is empty).
 * For witness: verifies the witness inclusion proof against the
 * trusted root AND checks that the target key diverges from
 * the witness key within a jump.
 */
export function verifyExclusionProof(
    trustedRoot: Hash,
    proof: ExclusionProof,
): boolean {
    if (proof.tag === "empty") return true;

    const { targetKey, witnessProof } = proof;

    // Verify witness inclusion proof against trusted root
    if (!verifyInclusionProof(trustedRoot, witnessProof)) {
        return false;
    }

    // Check divergence is within a jump
    // Steps are leaf-to-root; reverse for root-to-leaf scan
    const consumedRootToLeaf = [...witnessProof.proofSteps]
        .reverse()
        .map((s) => s.stepConsumed);
    return checkKeyDivergence(
        targetKey,
        witnessProof.proofKey,
        witnessProof.proofRootJump,
        consumedRootToLeaf,
    );
}

/**
 * Check that the target key diverges from the witness key
 * within a jump region, not at a branch boundary.
 *
 * Branch boundaries are at positions determined by the proof
 * structure: after the root jump, each step starts with a
 * direction bit (branch) followed by a jump.
 */
function checkKeyDivergence(
    targetKey: Key,
    witnessKey: Key,
    rootJump: Key,
    consumedList: number[],
): boolean {
    // Find first divergence position
    const divPos = firstDivergence(targetKey, witnessKey);
    if (divPos === null) return false; // keys identical → key exists

    // Compute branch boundary positions
    const branchPositions = scanBranchPositions(rootJump.length, consumedList);

    // Divergence must NOT be at a branch boundary
    // and must be within the witness key's range
    return !branchPositions.includes(divPos) && divPos < witnessKey.length;
}

/**
 * Find the first position where two keys differ.
 */
function firstDivergence(a: Key, b: Key): number | null {
    const len = Math.min(a.length, b.length);
    for (let i = 0; i < len; i++) {
        if (a[i] !== b[i]) return i;
    }
    return null;
}

/**
 * Compute the branch boundary positions from the proof structure.
 * Each step's direction bit is at a branch.
 */
function scanBranchPositions(
    rootJumpLen: number,
    consumedList: number[],
): number[] {
    const positions: number[] = [];
    let pos = rootJumpLen;
    for (const consumed of consumedList) {
        positions.push(pos);
        pos += consumed;
    }
    return positions;
}

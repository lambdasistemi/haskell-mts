/**
 * Inclusion proof verification
 *
 * Implements the verification algorithm that matches the Haskell
 * computeRootHash function in CSMT.Proof.Insertion.
 */
import type { Hash, InclusionProof, Indirect } from "./types";
import { L } from "./types";
import { combineHash, rootHash } from "./hash";

/**
 * Check if two Uint8Arrays are equal
 */
export function arraysEqual(a: Uint8Array, b: Uint8Array): boolean {
    if (a.length !== b.length) return false;
    for (let i = 0; i < a.length; i++) {
        if (a[i] !== b[i]) return false;
    }
    return true;
}

/**
 * Compute the root hash from an inclusion proof
 *
 * Algorithm:
 * 1. Start with proofValue
 * 2. Take keyAfterRoot = proofKey[proofRootJump.length:]
 * 3. Reverse it for processing from leaf to root
 * 4. For each step:
 *    - Take stepConsumed bits from reversed key
 *    - First bit is direction, rest is stepJump
 *    - Combine current with sibling based on direction
 * 5. Finally wrap with proofRootJump and compute rootHash
 */
export function computeRootHash(proof: InclusionProof): Hash {
    const { proofKey, proofValue, proofSteps, proofRootJump } = proof;

    // Get the key portion after the root jump
    const keyAfterRoot = proofKey.slice(proofRootJump.length);

    // Reverse for leaf-to-root processing
    let revKey = [...keyAfterRoot].reverse();
    let acc = proofValue;

    for (const step of proofSteps) {
        // Take stepConsumed bits from the reversed key
        const consumedRev = revKey.slice(0, step.stepConsumed);
        revKey = revKey.slice(step.stepConsumed);

        // Reverse back to get the original order
        const consumed = [...consumedRev].reverse();

        // First element is direction, rest is stepJump
        const [direction, ...stepJump] = consumed;

        if (direction === undefined) {
            throw new Error(
                "Invalid proof: stepConsumed is 0 which shouldn't happen"
            );
        }

        // Create current node's indirect
        const current: Indirect = { jump: stepJump, value: acc };

        // Combine based on direction
        // L: combineHash(current, sibling)
        // R: combineHash(sibling, current)
        if (direction === L) {
            acc = combineHash(current, step.stepSibling);
        } else {
            acc = combineHash(step.stepSibling, current);
        }
    }

    // Wrap with root jump and compute final hash
    return rootHash({ jump: proofRootJump, value: acc });
}

/**
 * Verify an inclusion proof is internally consistent
 *
 * Recomputes the root hash from the proof data and checks it matches
 * the claimed root hash.
 *
 * To verify against a trusted root, compare proofRootHash with
 * your trusted value after this returns true.
 */
export function verifyInclusionProof(proof: InclusionProof): boolean {
    const computed = computeRootHash(proof);
    return arraysEqual(computed, proof.proofRootHash);
}

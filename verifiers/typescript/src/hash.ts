/**
 * Blake2b-256 hashing
 *
 * Uses blakejs library for Blake2b implementation.
 */
import { blake2b } from "blakejs";
import type { Hash, Indirect } from "./types";
import { concat, serializeIndirect } from "./serialize";

/**
 * Compute Blake2b-256 hash of input bytes
 */
export function blake2b256(data: Uint8Array): Hash {
    return blake2b(data, undefined, 32);
}

/**
 * Compute the root hash of an Indirect value
 *
 * rootHash(indirect) = blake2b256(serializeIndirect(indirect))
 */
export function rootHash(indirect: Indirect): Hash {
    return blake2b256(serializeIndirect(indirect));
}

/**
 * Combine two Indirect values into a parent hash
 *
 * combineHash(left, right) = blake2b256(serializeIndirect(left) + serializeIndirect(right))
 */
export function combineHash(left: Indirect, right: Indirect): Hash {
    return blake2b256(concat(serializeIndirect(left), serializeIndirect(right)));
}

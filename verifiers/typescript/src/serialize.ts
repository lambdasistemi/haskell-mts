/**
 * Binary serialization for hashing
 *
 * These functions produce the exact same byte sequences as the Haskell
 * putKey, putSizedByteString, and putIndirect functions. This is critical
 * for computing hashes that match the Haskell implementation.
 */
import type { Indirect, Key } from "./types";
import { R } from "./types";

/**
 * Serialize a Key to bytes
 *
 * Format:
 * - 2 bytes: big-endian length (number of directions)
 * - Variable bytes: bit-packed directions (MSB first)
 *
 * Directions are packed 8 per byte, with the first direction in bit 7,
 * second in bit 6, etc. R = 1, L = 0.
 */
export function serializeKey(key: Key): Uint8Array {
    const length = key.length;
    const numBytes = Math.ceil(length / 8);
    const result = new Uint8Array(2 + numBytes);

    // 2-byte big-endian length
    result[0] = (length >> 8) & 0xff;
    result[1] = length & 0xff;

    // Pack directions into bytes, MSB first
    for (let i = 0; i < length; i++) {
        const byteIndex = 2 + Math.floor(i / 8);
        const bitIndex = 7 - (i % 8); // MSB first: positions 7,6,5,4,3,2,1,0
        const direction = key[i];
        if (direction === R) {
            // byteIndex is always within bounds since we allocated ceil(length/8) bytes
            result[byteIndex] = (result[byteIndex] ?? 0) | (1 << bitIndex);
        }
    }

    return result;
}

/**
 * Serialize a sized byte string
 *
 * Format:
 * - 2 bytes: big-endian length
 * - Variable bytes: raw data
 */
export function serializeSizedBytes(bytes: Uint8Array): Uint8Array {
    const length = bytes.length;
    const result = new Uint8Array(2 + length);

    // 2-byte big-endian length
    result[0] = (length >> 8) & 0xff;
    result[1] = length & 0xff;

    // Copy bytes
    result.set(bytes, 2);

    return result;
}

/**
 * Serialize an Indirect to bytes
 *
 * Format: serializeKey(jump) + serializeSizedBytes(value)
 */
export function serializeIndirect(indirect: Indirect): Uint8Array {
    const jumpBytes = serializeKey(indirect.jump);
    const valueBytes = serializeSizedBytes(indirect.value);
    return concat(jumpBytes, valueBytes);
}

/**
 * Concatenate two Uint8Arrays
 */
export function concat(a: Uint8Array, b: Uint8Array): Uint8Array {
    const result = new Uint8Array(a.length + b.length);
    result.set(a, 0);
    result.set(b, a.length);
    return result;
}

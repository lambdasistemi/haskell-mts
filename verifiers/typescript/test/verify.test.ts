import { describe, expect, it } from "vitest";
import {
    arraysEqual,
    blake2b256,
    combineHash,
    computeRootHash,
    concat,
    L,
    parseExclusionProof,
    parseProof,
    R,
    rootHash,
    serializeIndirect,
    serializeKey,
    serializeSizedBytes,
    verifyExclusionProof,
    verifyInclusionProof,
    verifyProofBytes,
} from "../src";
import type { InclusionProof, Indirect } from "../src";
import fixtures from "./fixtures.json";

describe("serializeKey", () => {
    it("serializes empty key", () => {
        const result = serializeKey([]);
        expect(result).toEqual(new Uint8Array([0x00, 0x00]));
    });

    it("serializes single L direction", () => {
        const result = serializeKey([L]);
        // Length = 1, bit 7 = 0 (L) => 0x00
        expect(result).toEqual(new Uint8Array([0x00, 0x01, 0x00]));
    });

    it("serializes single R direction", () => {
        const result = serializeKey([R]);
        // Length = 1, bit 7 = 1 (R) => 0x80
        expect(result).toEqual(new Uint8Array([0x00, 0x01, 0x80]));
    });

    it("serializes [L, R] key", () => {
        const result = serializeKey([L, R]);
        // Length = 2, bit 7 = 0 (L), bit 6 = 1 (R) => 0x40
        expect(result).toEqual(new Uint8Array([0x00, 0x02, 0x40]));
    });

    it("serializes [R, L] key", () => {
        const result = serializeKey([R, L]);
        // Length = 2, bit 7 = 1 (R), bit 6 = 0 (L) => 0x80
        expect(result).toEqual(new Uint8Array([0x00, 0x02, 0x80]));
    });

    it("serializes [R, R] key", () => {
        const result = serializeKey([R, R]);
        // Length = 2, bit 7 = 1 (R), bit 6 = 1 (R) => 0xc0
        expect(result).toEqual(new Uint8Array([0x00, 0x02, 0xc0]));
    });

    it("serializes 8-direction key ending in R", () => {
        const result = serializeKey([L, L, L, L, L, L, L, R]);
        // Length = 8, bits 7-1 = 0, bit 0 = 1 => 0x01
        expect(result).toEqual(new Uint8Array([0x00, 0x08, 0x01]));
    });

    it("serializes 8-direction key ending in L", () => {
        const result = serializeKey([R, R, R, R, R, R, R, L]);
        // Length = 8, bits 7-1 = 1, bit 0 = 0 => 0xfe
        expect(result).toEqual(new Uint8Array([0x00, 0x08, 0xfe]));
    });

    it("serializes 9-direction key (requires 2 bytes)", () => {
        const result = serializeKey([L, L, L, L, L, L, L, L, L]);
        // Length = 9, first byte = 0x00, second byte bit 7 = 0 => 0x00
        expect(result).toEqual(new Uint8Array([0x00, 0x09, 0x00, 0x00]));
    });
});

describe("serializeSizedBytes", () => {
    it("serializes empty bytes", () => {
        const result = serializeSizedBytes(new Uint8Array([]));
        expect(result).toEqual(new Uint8Array([0x00, 0x00]));
    });

    it("serializes short byte string", () => {
        const result = serializeSizedBytes(new Uint8Array([0x61, 0x62, 0x63]));
        expect(result).toEqual(new Uint8Array([0x00, 0x03, 0x61, 0x62, 0x63]));
    });
});

describe("serializeIndirect", () => {
    it("serializes indirect with empty jump and value", () => {
        const indirect: Indirect = {
            jump: [],
            value: new Uint8Array([]),
        };
        const result = serializeIndirect(indirect);
        // [0x00, 0x00] for empty key + [0x00, 0x00] for empty value
        expect(result).toEqual(new Uint8Array([0x00, 0x00, 0x00, 0x00]));
    });

    it("serializes indirect with jump and value", () => {
        const indirect: Indirect = {
            jump: [L, R, L],
            value: new Uint8Array([0x64, 0x61, 0x74, 0x61]), // "data"
        };
        const result = serializeIndirect(indirect);
        // Key [L,R,L]: length=3, bits 7,6,5 = 0,1,0 => 0x40
        // Value "data": length=4, bytes
        expect(result).toEqual(
            new Uint8Array([
                0x00,
                0x03,
                0x40, // key
                0x00,
                0x04,
                0x64,
                0x61,
                0x74,
                0x61, // value
            ])
        );
    });
});

describe("concat", () => {
    it("concatenates two arrays", () => {
        const a = new Uint8Array([1, 2]);
        const b = new Uint8Array([3, 4, 5]);
        expect(concat(a, b)).toEqual(new Uint8Array([1, 2, 3, 4, 5]));
    });

    it("handles empty arrays", () => {
        const a = new Uint8Array([]);
        const b = new Uint8Array([1, 2]);
        expect(concat(a, b)).toEqual(new Uint8Array([1, 2]));
        expect(concat(b, a)).toEqual(new Uint8Array([1, 2]));
    });
});

describe("blake2b256", () => {
    it("produces 32-byte hash", () => {
        const result = blake2b256(new Uint8Array([1, 2, 3]));
        expect(result.length).toBe(32);
    });

    it("is deterministic", () => {
        const data = new Uint8Array([1, 2, 3]);
        const hash1 = blake2b256(data);
        const hash2 = blake2b256(data);
        expect(arraysEqual(hash1, hash2)).toBe(true);
    });

    it("produces different hashes for different inputs", () => {
        const hash1 = blake2b256(new Uint8Array([1, 2, 3]));
        const hash2 = blake2b256(new Uint8Array([1, 2, 4]));
        expect(arraysEqual(hash1, hash2)).toBe(false);
    });
});

describe("rootHash", () => {
    it("hashes an indirect value", () => {
        const indirect: Indirect = {
            jump: [L, R],
            value: new Uint8Array(32).fill(0xab),
        };
        const result = rootHash(indirect);
        expect(result.length).toBe(32);
    });

    it("produces different hashes for different jumps", () => {
        const hash1 = rootHash({
            jump: [L],
            value: new Uint8Array(32).fill(0),
        });
        const hash2 = rootHash({
            jump: [R],
            value: new Uint8Array(32).fill(0),
        });
        expect(arraysEqual(hash1, hash2)).toBe(false);
    });
});

describe("combineHash", () => {
    it("combines two indirect values", () => {
        const left: Indirect = { jump: [], value: new Uint8Array(32).fill(1) };
        const right: Indirect = { jump: [], value: new Uint8Array(32).fill(2) };
        const result = combineHash(left, right);
        expect(result.length).toBe(32);
    });

    it("is not commutative", () => {
        const left: Indirect = { jump: [], value: new Uint8Array(32).fill(1) };
        const right: Indirect = { jump: [], value: new Uint8Array(32).fill(2) };
        const hash1 = combineHash(left, right);
        const hash2 = combineHash(right, left);
        expect(arraysEqual(hash1, hash2)).toBe(false);
    });
});

describe("arraysEqual", () => {
    it("returns true for equal arrays", () => {
        expect(
            arraysEqual(new Uint8Array([1, 2, 3]), new Uint8Array([1, 2, 3]))
        ).toBe(true);
    });

    it("returns false for different arrays", () => {
        expect(
            arraysEqual(new Uint8Array([1, 2, 3]), new Uint8Array([1, 2, 4]))
        ).toBe(false);
    });

    it("returns false for arrays of different length", () => {
        expect(
            arraysEqual(new Uint8Array([1, 2, 3]), new Uint8Array([1, 2]))
        ).toBe(false);
    });
});

describe("parseProof", () => {
    it("parses valid CBOR proof", () => {
        const fixture = fixtures.proofs[0];
        if (!fixture) {
            throw new Error("No fixture found");
        }
        const bytes = hexToBytes(fixture.cbor);
        const proof = parseProof(bytes);

        expect(proof.proofKey.length).toBeGreaterThan(0);
        expect(proof.proofValue.length).toBe(32);
        expect(proof.proofRootHash.length).toBe(32);
        expect(proof.proofRootJump).toBeDefined();
    });

    it("throws on invalid CBOR", () => {
        expect(() => parseProof(new Uint8Array([0xff, 0xff]))).toThrow();
    });
});

describe("verifyInclusionProof", () => {
    it("verifies valid proofs from fixtures", () => {
        for (const fixture of fixtures.proofs) {
            const bytes = hexToBytes(fixture.cbor);
            const proof = parseProof(bytes);
            expect(verifyInclusionProof(proof)).toBe(true);
        }
    });

    it("rejects proof with tampered root hash", () => {
        const fixture = fixtures.proofs[0];
        if (!fixture) {
            throw new Error("No fixture found");
        }
        const bytes = hexToBytes(fixture.cbor);
        const proof = parseProof(bytes);

        const tampered: InclusionProof = {
            ...proof,
            proofRootHash: new Uint8Array(32).fill(0),
        };

        expect(verifyInclusionProof(tampered)).toBe(false);
    });

    it("rejects proof with tampered value", () => {
        const fixture = fixtures.proofs[0];
        if (!fixture) {
            throw new Error("No fixture found");
        }
        const bytes = hexToBytes(fixture.cbor);
        const proof = parseProof(bytes);

        const tampered: InclusionProof = {
            ...proof,
            proofValue: new Uint8Array(32).fill(0),
        };

        expect(verifyInclusionProof(tampered)).toBe(false);
    });

    it("rejects proof with tampered key (after rootJump)", () => {
        const fixture = fixtures.proofs[0];
        if (!fixture) {
            throw new Error("No fixture found");
        }
        const bytes = hexToBytes(fixture.cbor);
        const proof = parseProof(bytes);

        // Flip a bit after rootJump (which is used in verification)
        const tamperedKey = [...proof.proofKey];
        const idx = proof.proofRootJump.length;
        if (tamperedKey[idx] !== undefined) {
            tamperedKey[idx] = tamperedKey[idx] === L ? R : L;
        }

        const tampered: InclusionProof = {
            ...proof,
            proofKey: tamperedKey,
        };

        expect(verifyInclusionProof(tampered)).toBe(false);
    });

    it("rejects proof with tampered sibling", () => {
        const fixture = fixtures.proofs[0];
        if (!fixture) {
            throw new Error("No fixture found");
        }
        const bytes = hexToBytes(fixture.cbor);
        const proof = parseProof(bytes);

        if (proof.proofSteps.length === 0) {
            return; // Skip if no steps
        }

        const tamperedSteps = proof.proofSteps.map((step, i) =>
            i === 0
                ? {
                      ...step,
                      stepSibling: {
                          ...step.stepSibling,
                          value: new Uint8Array(32).fill(0xff),
                      },
                  }
                : step
        );

        const tampered: InclusionProof = {
            ...proof,
            proofSteps: tamperedSteps,
        };

        expect(verifyInclusionProof(tampered)).toBe(false);
    });

    it("rejects proof with tampered rootJump", () => {
        const fixture = fixtures.proofs[0];
        if (!fixture) {
            throw new Error("No fixture found");
        }
        const bytes = hexToBytes(fixture.cbor);
        const proof = parseProof(bytes);

        const tampered: InclusionProof = {
            ...proof,
            proofRootJump: [R, R, R], // Different jump
        };

        expect(verifyInclusionProof(tampered)).toBe(false);
    });
});

describe("verifyProofBytes", () => {
    it("parses and verifies valid proof bytes", () => {
        const fixture = fixtures.proofs[0];
        if (!fixture) {
            throw new Error("No fixture found");
        }
        const bytes = hexToBytes(fixture.cbor);
        expect(verifyProofBytes(bytes)).toBe(true);
    });

    it("throws on invalid bytes", () => {
        expect(() => verifyProofBytes(new Uint8Array([0xff]))).toThrow();
    });
});

describe("computeRootHash", () => {
    it("computes expected root hash for fixtures", () => {
        for (const fixture of fixtures.proofs) {
            const bytes = hexToBytes(fixture.cbor);
            const proof = parseProof(bytes);
            const computed = computeRootHash(proof);
            expect(arraysEqual(computed, proof.proofRootHash)).toBe(true);
        }
    });
});

describe("parseExclusionProof", () => {
    it("parses valid exclusion proofs from fixtures", () => {
        for (const fixture of (fixtures as any).exclusionProofs) {
            const bytes = hexToBytes(fixture.cbor);
            const proof = parseExclusionProof(bytes);
            expect(proof.tag).toBe("witness");
        }
    });

    it("throws on invalid CBOR", () => {
        expect(() => parseExclusionProof(new Uint8Array([0xff, 0xff]))).toThrow();
    });
});

describe("verifyExclusionProof", () => {
    it("verifies valid exclusion proofs from fixtures", () => {
        for (const fixture of (fixtures as any).exclusionProofs) {
            const bytes = hexToBytes(fixture.cbor);
            const proof = parseExclusionProof(bytes);
            expect(verifyExclusionProof(proof)).toBe(true);
        }
    });

    it("verifies empty exclusion proof", () => {
        expect(verifyExclusionProof({ tag: "empty" })).toBe(true);
    });

    it("rejects exclusion proof with tampered target key", () => {
        const fixture = (fixtures as any).exclusionProofs[0];
        if (!fixture) throw new Error("No exclusion fixture");
        const bytes = hexToBytes(fixture.cbor);
        const proof = parseExclusionProof(bytes);

        if (proof.tag !== "witness") throw new Error("Expected witness");

        // Replace target key with the witness key (which exists in the tree)
        const tampered = {
            ...proof,
            targetKey: proof.witnessProof.proofKey,
        };
        expect(verifyExclusionProof(tampered)).toBe(false);
    });

    it("rejects exclusion proof with tampered witness hash", () => {
        const fixture = (fixtures as any).exclusionProofs[0];
        if (!fixture) throw new Error("No exclusion fixture");
        const bytes = hexToBytes(fixture.cbor);
        const proof = parseExclusionProof(bytes);

        if (proof.tag !== "witness") throw new Error("Expected witness");

        const tampered = {
            ...proof,
            witnessProof: {
                ...proof.witnessProof,
                proofRootHash: new Uint8Array(32).fill(0),
            },
        };
        expect(verifyExclusionProof(tampered)).toBe(false);
    });
});

function hexToBytes(hex: string): Uint8Array {
    const bytes = new Uint8Array(hex.length / 2);
    for (let i = 0; i < hex.length; i += 2) {
        bytes[i / 2] = parseInt(hex.slice(i, i + 2), 16);
    }
    return bytes;
}

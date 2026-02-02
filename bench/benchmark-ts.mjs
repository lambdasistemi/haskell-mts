#!/usr/bin/env node
// TypeScript/Aiken MPF Benchmark
// Uses the aiken-lang/merkle-patricia-forestry implementation
// Run this from within the off-chain directory after npm install

import { Trie, Store } from './lib/index.js';

// Generate deterministic test data
function generateTestData(count) {
  const data = [];
  for (let i = 0; i < count; i++) {
    const key = `key-${i.toString().padStart(6, '0')}`;
    const value = `value-${i}`;
    data.push({ key, value });
  }
  return data;
}

// Benchmark helper
async function benchmark(name, fn) {
  const start = process.hrtime.bigint();
  const result = await fn();
  const end = process.hrtime.bigint();
  const durationMs = Number(end - start) / 1_000_000;
  console.log(`${name}: ${durationMs.toFixed(2)}ms`);
  return { result, durationMs };
}

async function runBenchmark(count) {
  console.log(`\n=== TypeScript MPF Benchmark (n=${count}) ===\n`);

  const testData = generateTestData(count);

  // Benchmark: Batch insert using fromList
  const { result: trie, durationMs: insertTime } = await benchmark(
    `Insert ${count} items (fromList)`,
    async () => {
      const store = new Store();
      return await Trie.fromList(testData, store);
    }
  );

  console.log(`Root hash: ${trie.hash.toString('hex')}`);

  // Benchmark: Generate proofs for all items
  const { result: proofs, durationMs: proofGenTime } = await benchmark(
    `Generate ${count} proofs`,
    async () => {
      const proofs = [];
      for (const { key } of testData) {
        const proof = await trie.prove(key);
        proofs.push(proof);
      }
      return proofs;
    }
  );

  // Benchmark: Verify all proofs
  const { durationMs: verifyTime } = await benchmark(
    `Verify ${count} proofs`,
    async () => {
      let verified = 0;
      for (let i = 0; i < proofs.length; i++) {
        const proof = proofs[i];
        // Verification is done by checking the proof's root hash matches
        if (proof.verify(trie.hash)) {
          verified++;
        }
      }
      return verified;
    }
  );

  // Summary
  console.log(`\n--- Summary ---`);
  console.log(`Insert rate: ${(count / insertTime * 1000).toFixed(0)} ops/sec`);
  console.log(`Proof gen rate: ${(count / proofGenTime * 1000).toFixed(0)} ops/sec`);
  console.log(`Verify rate: ${(count / verifyTime * 1000).toFixed(0)} ops/sec`);

  return {
    count,
    insertTime,
    proofGenTime,
    verifyTime,
  };
}

// Main
const counts = [100, 1000];
if (process.argv[2]) {
  counts.push(parseInt(process.argv[2]));
}

console.log('MPF Benchmark - TypeScript/Aiken Implementation');
console.log('================================================');

for (const count of counts) {
  await runBenchmark(count);
}

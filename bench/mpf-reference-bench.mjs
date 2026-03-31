#!/usr/bin/env node

// MPF Reference Benchmark using @aiken-lang/merkle-patricia-forestry
//
// Exercises the canonical JS implementation on LevelDB
// with the SAME key-value data as the Haskell benchmarks.
//
// IMPORTANT: The JS implementation hashes keys with Blake2b-256 internally
// via intoPath(). We pass the same raw key strings — the JS impl will hash
// them. The Haskell benchmark must also hash keys before trie insertion
// to produce identical trie paths.
//
// Operations: sequential insert, proof generation (with CBOR size), delete

import { Trie } from '../../merkle-patricia-forestry/off-chain/lib/trie.js';
import { Store } from '../../merkle-patricia-forestry/off-chain/lib/store.js';
import * as fs from 'node:fs';
import * as path from 'node:path';
import * as os from 'node:os';

// Generate IDENTICAL test data as the Haskell benchmark
// Keys: "key-00000000", "key-00000001", ...
// Values: "value-0", "value-1", ...
function generateTestData(count) {
  const data = [];
  for (let i = 0; i < count; i++) {
    const key = Buffer.from(`key-${String(i).padStart(8, '0')}`, 'utf-8');
    const value = Buffer.from(`value-${i}`, 'utf-8');
    data.push([key, value]);
  }
  return data;
}

async function timeAction(fn) {
  const start = performance.now();
  const result = await fn();
  const elapsed = (performance.now() - start) / 1000;
  return { result, elapsed };
}

function formatDir(dirPath) {
  let total = 0;
  try {
    const entries = fs.readdirSync(dirPath, { recursive: true, withFileTypes: true });
    for (const entry of entries) {
      if (entry.isFile()) {
        const fullPath = path.join(entry.parentPath || entry.path, entry.name);
        total += fs.statSync(fullPath).size;
      }
    }
  } catch { total = 0; }
  return total;
}

async function runBench(n, useLevel) {
  const label = useLevel ? 'LevelDB' : 'in-memory';
  let store;
  let tmpDir;

  let trie;
  if (useLevel) {
    tmpDir = fs.mkdtempSync(path.join(os.tmpdir(), 'mpf-ref-bench-'));
    store = new Store(path.join(tmpDir, 'db'));
    await store.ready();
    trie = await Trie.from(store);
  } else {
    trie = new Trie();
  }

  const testData = generateTestData(n);

  // Sequential insert
  const { elapsed: insertTime } = await timeAction(async () => {
    for (const [k, v] of testData) {
      trie = await trie.insert(k, v);
    }
  });

  // Proof generation + CBOR size
  let totalProofBytes = 0;
  let proofCount = 0;
  const { elapsed: proofTime } = await timeAction(async () => {
    for (const [k] of testData) {
      const proof = await trie.prove(k);
      if (proof) {
        const cborBuf = proof.toCBOR();
        totalProofBytes += cborBuf.length;
        proofCount++;
      }
    }
  });

  // Delete half
  const deleteData = testData.slice(0, Math.floor(n / 2));
  const { elapsed: deleteTime } = await timeAction(async () => {
    for (const [k] of deleteData) {
      trie = await trie.delete(k);
    }
  });

  // Database size (only for LevelDB)
  let dbSizeBytes = 0;
  if (tmpDir) {
    dbSizeBytes = formatDir(path.join(tmpDir, 'db'));
  }

  const insertRate = n / insertTime;
  const proofRate = proofCount / proofTime;
  const deleteRate = deleteData.length / deleteTime;
  const avgProofBytes = proofCount > 0 ? totalProofBytes / proofCount : 0;

  const dbSizeStr = dbSizeBytes > 0
    ? `${(dbSizeBytes / 1024).toFixed(0)} KB`
    : 'n/a';

  console.log(
    `  MPF-JS ${label.padEnd(8)} | ${Math.round(insertRate).toString().padStart(8)} ins/s | ${Math.round(proofRate).toString().padStart(8)} proof/s | ${Math.round(deleteRate).toString().padStart(8)} del/s | ${Math.round(avgProofBytes).toString().padStart(6)} bytes | ${dbSizeStr.padStart(10)}`
  );

  // Cleanup
  if (tmpDir) {
    fs.rmSync(tmpDir, { recursive: true, force: true });
  }

  return { insertRate, proofRate, deleteRate, avgProofBytes, dbSizeBytes };
}

async function main() {
  const args = process.argv.slice(2);
  const sizes = args.length > 0
    ? args.filter(a => /^\d+$/.test(a)).map(Number)
    : [1000, 10000];

  console.log('MPF Reference Benchmark (@aiken-lang/merkle-patricia-forestry)');
  console.log('===============================================================');
  console.log('Sequential insert, proof gen, delete — same data as Haskell bench');
  console.log('NOTE: JS impl internally hashes keys with Blake2b-256 (intoPath)');
  console.log('');

  for (const n of sizes) {
    console.log(`\n--- N = ${n} ---`);
    console.log(
      `  ${''.padEnd(16)} | ${'insert'.padStart(14)} | ${'proof gen'.padStart(15)} | ${'delete'.padStart(13)} | ${'proof CBOR'.padStart(11)} | ${'DB size'.padStart(10)}`
    );
    console.log('-'.repeat(100));

    await runBench(n, false);  // in-memory (no DB size)
    await runBench(n, true);   // LevelDB
  }
}

main().catch(console.error);

# WASM browser demo - MPF build, prove, verify

This page ports the MPF write path to WebAssembly. It mirrors the CSMT
write demo, but uses `mpf-write.wasm` to mutate a Merkle Patricia
Forest and `mpf-verify.wasm` to re-check Aiken-compatible proof steps
against the reported root.

<div id="demo-frame"></div>

!!! tip "Try it"
    <a href="../demo-write-mpf/index.html" target="_blank">Open the standalone demo</a>

## What is different from CSMT

- MPF proof bytes are the Aiken proof-step list only. The verifier also
  needs the query key and, for inclusion mode, the value as separate
  inputs.
- Raw browser keys are routed through the same `blake2b_256(key)` path used
  by Aiken, via `fromHexKVAikenHashes` / `aikenKeyPath`, so the write and
  verify sides agree on the trie path.
- Presence and absence share the same CBOR proof-step encoding. The
  write side distinguishes them with `ptype = 0` for inclusion and
  `ptype = 1` for exclusion.
- An empty forest returns `ptype = 0xff` and no proof payload.

That matches the merged Aiken-parity exclusion-proof work: the browser
transport reuses the canonical proof-step format instead of introducing
a second exclusion-proof encoding.

## What the demo ships

- `mpf-write.wasm` - the write entry point exported via
  `nix build .#mpf-write-wasm`
- `mpf-verify.wasm` - the pure Aiken-compatible verifier exported via
  `nix build .#mpf-verify-wasm`
- `index.html` + `write.js` - the static page that runs both modules
  under `@bjorn3/browser_wasi_shim`

## Workflow

1. Insert or delete key/value pairs and watch the MPF root update.
2. Prove a key. Present keys return the current value plus inclusion
   proof bytes; absent keys return an exclusion witness.
3. Verify the proof against the root via `mpf-verify.wasm`.
4. Tamper and verify to confirm the verifier rejects modified proof
   bytes.
5. Reload the page and continue from the persisted IndexedDB state.

## Stdin protocol

`mpf-write-wasm` uses the same state-and-ops envelope as the CSMT write
demo:

| Field           | Payload |
|-----------------|---------|
| `slen state`    | prior `MPFInMemoryDB` blob (empty to start) |
| `n`             | number of ops |
| `n x op`        | opcode-tagged: `0 klen key vlen value` for insert, `1 klen key` for delete |
| `qlen queryKey` | key to prove or disprove |

The stdout response is:

| Field        | Payload |
|--------------|---------|
| `slen state` | updated `MPFInMemoryDB` blob |
| `root`       | 32-byte post-mutation root hash |
| `vlen value` | queried key's value, empty if absent |
| `ptype`      | 1 byte: `0` inclusion, `1` exclusion, `0xff` none |
| `plen proof` | Aiken-compatible CBOR proof-step bytes |

`mpf-verify-wasm` consumes a richer stdin because the proof bytes alone
do not carry the query context:

| Field        | Payload |
|--------------|---------|
| `opcode`     | `0` inclusion or `1` exclusion |
| `root`       | 32-byte trusted root |
| `klen key`   | raw query key bytes |
| `vlen value` | raw value bytes, empty for exclusion |
| `plen proof` | Aiken-compatible CBOR proof-step bytes |

## Build it yourself

```bash
nix build .#mpf-write-wasm
nix build .#mpf-verify-wasm
nix build .#mpf-wasm-write-demo
PORT=8002 nix run .#mpf-wasm-write-demo
```

The result is a static directory containing `index.html`, `write.js`,
`mpf-write.wasm`, and `mpf-verify.wasm`. The docs site composes that
same bundle via `nix build .#docs`.

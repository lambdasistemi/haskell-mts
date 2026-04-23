# WASM browser demo - build, prove, verify

The companion to the read-only `csmt-verify` demo: this page
cross-compiles the write path (`csmt-write-wasm`) to WebAssembly too,
so everything - building the tree, generating proofs, and re-verifying
them against the root - happens inside sandboxed WASM.

<div id="demo-frame"></div>

!!! tip "Try it"
    <a href="../demo-write/index.html" target="_blank">Open the standalone demo</a>

## What the demo ships

- `csmt-write.wasm` - the write entry point produced by
  `wasm32-wasi-cabal` via `nix build .#csmt-write-wasm`. It takes
  a prior `InMemoryDB` blob, a batch of inserts/deletes, and a query
  key; it returns the updated blob, the post-mutation root, and either
  an inclusion proof or an exclusion proof.
- `csmt-verify.wasm` - the existing verifier, reused by the page to
  independently re-check each proof it produces, exported via
  `nix build .#csmt-verify-wasm`.
- `index.html` + `write.js` - the static page that drives both modules
  under `@bjorn3/browser_wasi_shim`.

State is persisted to IndexedDB so anything you build survives a page
reload. A capped undo/redo stack is persisted alongside it, and the
page supports Cmd/Ctrl-Z shortcuts.

## Workflow

1. Insert key/value pairs and watch the 32-byte Blake2b-256 root update.
2. Prove a key to get an inclusion proof if present or an exclusion
   proof if absent.
3. Verify the returned proof against the root via `csmt-verify.wasm`.
4. Tamper and verify to demonstrate rejection of corrupted proof bytes.
5. Delete a key, then prove it again to observe the exclusion witness.

## Stdin protocol

`csmt-write-wasm` consumes the entire stdin as one envelope. All
4-byte lengths are big-endian:

| Field           | Payload |
|-----------------|---------|
| `slen state`    | prior `InMemoryDB` blob (empty to start) |
| `n`             | number of ops |
| `n x op`        | opcode-tagged: `0 klen key vlen value` for insert, `1 klen key` for delete |
| `qlen queryKey` | key to prove or disprove |

The stdout response is:

| Field        | Payload |
|--------------|---------|
| `slen state` | updated `InMemoryDB` blob |
| `root`       | 32-byte post-mutation root hash |
| `vlen value` | queried key's value, empty if absent |
| `ptype`      | 1 byte: `0` inclusion, `1` exclusion, `0xff` none |
| `plen proof` | CBOR-encoded proof of the declared type |

The browser forwards `ptype` straight through as the verifier opcode, so
no host-side translation is needed.

## Build it yourself

```bash
nix build .#csmt-write-wasm
nix build .#csmt-verify-wasm
nix build .#csmt-wasm-write-demo
PORT=8001 nix run .#csmt-wasm-write-demo
```

The output is a plain tree of static files suitable for any static
host. The docs site here is built from the same flake output via
`nix build .#docs`.

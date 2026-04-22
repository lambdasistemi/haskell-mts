# WASM browser demo — build, prove, verify

The companion to the read-only `csmt-verify` demo: this page
cross-compiles the **write** path (`csmt-write-wasm`) to
WebAssembly too, so everything — building the tree, generating
proofs, re-verifying them against the root — happens in the
sandboxed WebAssembly module. No server, no Haskell runtime on the
page.

<div id="demo-frame"></div>

!!! tip "Try it"
    [Open the standalone demo](demo-write/index.html){:target="_blank"}

## What the demo ships

- `csmt-write.wasm` — the write entry point produced by
  `wasm32-wasi-cabal` via `nix build .#csmt-wasm-write-demo`.
  Takes a prior `InMemoryDB` blob + a batch of inserts/deletes +
  a query key; returns the updated blob, the post-mutation root,
  and either an inclusion proof (key present) or an exclusion
  proof (key absent).
- `csmt-verify.wasm` — the existing verifier, re-used by the page
  to independently re-check every proof it produces.
- `index.html` + `write.js` — the static page that drives both
  modules under `@bjorn3/browser_wasi_shim`.

State is persisted to **IndexedDB** (not `localStorage` — it is
binary, async, and >5 MB) so anything you build survives a page
reload. A capped undo / redo stack is persisted alongside it; ⌘Z,
⌘⇧Z and Ctrl-Y work as keyboard shortcuts.

## Workflow

1. **Insert** key / value pairs. The 32-byte Blake2b-256 root
   hash updates live after each mutation.
2. **Prove** a key: if the key is in the tree you get an
   inclusion proof (key + value + Merkle path); otherwise an
   exclusion proof (witness leaf + divergence path).
3. **Verify** the returned proof against the root via
   `csmt-verify.wasm`. The verifier knows nothing about the
   write-side state — it only trusts the root.
4. **Tamper & verify** flips a byte of the proof to demonstrate
   the verifier rejecting a corrupted witness.
5. **Delete**, then **Prove** the same key again — the exclusion
   proof picks up the new state.

## Stdin protocol

`csmt-write-wasm` consumes the entire stdin as a single envelope
(all 4-byte lengths are big-endian):

| Field              | Payload                                    |
|--------------------|--------------------------------------------|
| `slen state`       | prior `InMemoryDB` blob (empty to start)   |
| `n`                | number of ops                              |
| `n × op`           | opcode-tagged: `0 klen key vlen value` for insert, `1 klen key` for delete |
| `qlen queryKey`    | key to prove / disprove                    |

The response envelope on stdout is:

| Field          | Payload                                          |
|----------------|--------------------------------------------------|
| `slen state`   | updated `InMemoryDB` blob                        |
| `root`         | 32-byte post-mutation root hash                  |
| `vlen value`   | queried key's value (empty if absent)            |
| `ptype`        | 1 byte: `0` inclusion, `1` exclusion, `0xff` none |
| `plen proof`   | CBOR-encoded proof of the declared type          |

The browser forwards `ptype` directly as the opcode to
`csmt-verify.wasm`, which accepts `0` / `1` with the same meaning,
so no host-side translation is needed.

## Build it yourself

```bash
nix build .#csmt-wasm-write-demo
```

The output is a plain tree of static files (HTML + both `.wasm`
blobs + `write.js`) suitable for copying into any static host. The
docs site here is one such host — it is built from the same flake
output, composed into the MkDocs site via `nix build .#docs`.

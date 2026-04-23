# WASM browser demo

The `csmt-verify` sublibrary is cross-compiled to WebAssembly via
the GHC WASM backend. The resulting `csmt-verify.wasm` module runs
in any WASI-capable host, including a browser using the
`@bjorn3/browser_wasi_shim` polyfill.

<div id="demo-frame"></div>

!!! tip "Try it"
    <a href="../demo/index.html" target="_blank">Open the standalone demo</a>

The demo ships:

- `csmt-verify.wasm` — the actual binary produced by
  `wasm32-wasi-cabal` via `nix build .#csmt-verify-wasm`
- `index.html` + `verify.js` — a minimal static page that loads
  the module, pipes a byte-level stdin into it, and reports the
  exit code
- `fixtures.json` — the same proof fixtures used by the TypeScript
  verifier tests, so the page can exercise inclusion and exclusion
  proofs out of the box

The `Tamper root` / `Tamper proof` buttons flip a byte so you can
watch the verifier reject a corrupted proof in real time — no
server round-trip, the sandboxed WebAssembly module does the work.

## Stdin protocol

The WASM executable reads the entire stdin and returns via exit
code:

| Bytes     | Meaning                                  |
|-----------|------------------------------------------|
| 1         | opcode: `0` = inclusion, `1` = exclusion |
| 32        | trusted root hash (raw bytes)            |
| remainder | CBOR-encoded proof                       |

Exit code `0` means the proof verifies against the supplied root;
exit code `1` means it does not (or the input was malformed).

## Build it yourself

```bash
nix build .#csmt-verify-wasm
nix build .#csmt-verify-wasm-demo
PORT=8000 nix run .#csmt-verify-wasm-demo
```

The output is a plain tree of static files suitable for copying
into any static host.

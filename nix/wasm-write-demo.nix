# Static browser demo for the full CSMT build + prove + verify
# loop running entirely in WebAssembly.
#
# Ships an index.html + write.js that load BOTH .wasm artefacts
# produced by ./wasm.nix and drive them under the
# browser_wasi_shim polyfill. Tree state is persisted to
# IndexedDB, so whatever the user builds survives a page reload
# without any server component.
{ pkgs, wasm }:
pkgs.runCommand "csmt-wasm-write-demo"
  {
    preferLocalBuild = true;
    nativeBuildInputs = [ pkgs.coreutils ];
  }
  ''
    mkdir -p $out
    cp ${../verifiers/browser-write/write.js} $out/write.js
    cp ${wasm}/csmt-verify.wasm               $out/csmt-verify.wasm
    cp ${wasm}/csmt-write.wasm                $out/csmt-write.wasm

    # Content-hash the JS + both WASMs into the script tag so
    # browsers never serve a stale module after a build.
    version=$(sha256sum \
        $out/write.js \
        $out/csmt-write.wasm \
        $out/csmt-verify.wasm \
      | sha256sum | cut -c1-16)
    sed "s/@VERSION@/$version/" \
      ${../verifiers/browser-write/index.html} > $out/index.html
  ''

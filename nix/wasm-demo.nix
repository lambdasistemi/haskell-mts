# Static browser demo for csmt-verify.wasm.
#
# Ships an index.html + verify.js that load the WASM artifact
# produced by ./wasm.nix and run it under the browser_wasi_shim
# polyfill. The output is a plain tree of static files suitable
# for copying into a docs site or any static host.
{ pkgs, wasm, fixtures }:
pkgs.runCommand "csmt-verify-wasm-demo" {
  preferLocalBuild = true;
  nativeBuildInputs = [ pkgs.coreutils ];
} ''
  mkdir -p $out
  cp ${../verifiers/browser/verify.js}  $out/verify.js
  cp ${fixtures}                        $out/fixtures.json
  cp ${wasm}/csmt-verify.wasm           $out/csmt-verify.wasm

  # Content-hash the JS + WASM into the script tag so browsers
  # never serve a stale module after a build.
  version=$(sha256sum $out/verify.js $out/csmt-verify.wasm \
    | sha256sum | cut -c1-16)
  sed "s/@VERSION@/$version/" \
    ${../verifiers/browser/index.html} > $out/index.html
''

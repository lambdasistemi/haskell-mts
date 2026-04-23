# Static browser demo for the MPF build + prove + verify loop running
# entirely in WebAssembly.
{ pkgs, wasm }:
pkgs.runCommand "mpf-wasm-write-demo" {
  preferLocalBuild = true;
  nativeBuildInputs = [ pkgs.coreutils ];
} ''
  mkdir -p $out
  cp ${../verifiers/browser-write-mpf/write.js} $out/write.js
  cp ${wasm}/mpf-verify.wasm                   $out/mpf-verify.wasm
  cp ${wasm}/mpf-write.wasm                    $out/mpf-write.wasm

  version=$(sha256sum \
      $out/write.js \
      $out/mpf-write.wasm \
      $out/mpf-verify.wasm \
    | sha256sum | cut -c1-16)
  sed "s/@VERSION@/$version/" \
    ${../verifiers/browser-write-mpf/index.html} > $out/index.html
''

{ pkgs, src, mkdocsAssets, fixtures, wasmBuild }:
let
  mkWasmModule = pname: file:
    pkgs.runCommand pname { preferLocalBuild = true; } ''
      mkdir -p $out
      cp ${wasmBuild.wasm}/${file} $out/${file}
    '';

  verifyDemo = import ./wasm-demo.nix {
    inherit pkgs fixtures;
    wasm = wasmBuild.wasm;
  };
  writeDemo = import ./wasm-write-demo.nix {
    inherit pkgs;
    wasm = wasmBuild.wasm;
  };
  mpfWriteDemo = import ./mpf-wasm-write-demo.nix {
    inherit pkgs;
    wasm = wasmBuild.wasm;
  };
  docs = import ./docs.nix {
    inherit pkgs src;
    inherit mkdocsAssets;
    inherit verifyDemo writeDemo mpfWriteDemo;
  };
in {
  wasm-artifacts = wasmBuild.wasm;
  wasm-artifacts-deps = wasmBuild.deps;

  csmt-verify-wasm = mkWasmModule "csmt-verify-wasm" "csmt-verify.wasm";
  csmt-write-wasm = mkWasmModule "csmt-write-wasm" "csmt-write.wasm";
  mpf-verify-wasm = mkWasmModule "mpf-verify-wasm" "mpf-verify.wasm";
  mpf-write-wasm = mkWasmModule "mpf-write-wasm" "mpf-write.wasm";

  csmt-verify-wasm-demo = verifyDemo;
  csmt-wasm-write-demo = writeDemo;
  mpf-wasm-write-demo = mpfWriteDemo;
  inherit docs;
}

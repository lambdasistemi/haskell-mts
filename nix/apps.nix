{ pkgs, demos }:
let
  mkStaticSiteApp = name: root:
    let
      server = pkgs.writeShellApplication {
        name = "serve-${name}";
        runtimeInputs = [ pkgs.python3 ];
        text = ''
          host=''${HOST:-127.0.0.1}
          port=''${PORT:-8000}
          echo "Serving ${name} at http://$host:$port/"
          exec python -m http.server "$port" --bind "$host" --directory ${root}
        '';
      };
    in {
      type = "app";
      program = pkgs.lib.getExe server;
    };
in {
  docs = mkStaticSiteApp "docs" demos.docs;
  csmt-verify-wasm-demo =
    mkStaticSiteApp "csmt-verify-wasm-demo" demos.csmt-verify-wasm-demo;
  csmt-wasm-write-demo =
    mkStaticSiteApp "csmt-wasm-write-demo" demos.csmt-wasm-write-demo;
  mpf-wasm-write-demo =
    mkStaticSiteApp "mpf-wasm-write-demo" demos.mpf-wasm-write-demo;
}

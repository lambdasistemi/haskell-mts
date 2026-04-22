{
  description = "MTS, Merkle tree store with pluggable trie implementations";
  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs = {
      follows = "haskellNix/nixpkgs-unstable";
    };
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";
    mkdocs.url = "github:paolino/dev-assets?dir=mkdocs";
    asciinema.url = "github:paolino/dev-assets?dir=asciinema";
    ghc-wasm-meta = {
      url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
      flake = true;
    };
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-utils,
      haskellNix,
      mkdocs,
      asciinema,
      ghc-wasm-meta,
      ...
    }:
    let
      lib = nixpkgs.lib;
      version = self.dirtyShortRev or self.shortRev;

      perSystem =
        system:
        let
          pkgs = import nixpkgs {
            overlays = [
              haskellNix.overlay # some functions
            ];
            inherit system;
          };
          rewrite-libs = import ./CI/rewrite-libs/rewrite-libs.nix {
            inherit system;
            inherit (inputs) nixpkgs flake-utils haskellNix;
          };
          project = import ./nix/project.nix {
            indexState = "2025-08-07T00:00:00Z";
            inherit pkgs;
            mkdocs = mkdocs.packages.${system};
            asciinema = asciinema.packages.${system};
          };

          linux-artifacts = import ./nix/linux-artifacts.nix { inherit pkgs version project; };
          macos-artifacts = import ./nix/macos-artifacts.nix {
            inherit pkgs project version;
            rewrite-libs = rewrite-libs.packages.default;
          };

          docker-image = import ./nix/docker-image.nix {
            inherit pkgs;
            inherit version;
            inherit project;
          };
          docker.packages = { inherit docker-image; };
          info.packages = { inherit version; };

          # WASM build of csmt-verify. Only wired on x86_64-linux
          # because ghc-wasm-meta ships binary toolchains for that
          # platform; darwin users can consume the same .wasm from
          # CI or via a Linux builder. See nix/wasm.nix.
          wasmBuild =
            if system == "x86_64-linux" then
              import ./nix/wasm.nix {
                inherit pkgs;
                src = ./.;
                ghcWasmToolchain = ghc-wasm-meta.packages.${system}.all_9_12;
                # Bump whenever the WASM dep set changes. First-run
                # Nix reports the real hash via a fixed-output hash
                # mismatch — replace this literal.
                dependenciesHash = "sha256-wjsKzZxunTciN3YkY+0f9v3OA2e0qF9W1VdxNCFDHWQ=";
              }
            else
              null;

          wasmPackages =
            if wasmBuild != null then
              let
                demo = import ./nix/wasm-demo.nix {
                  inherit pkgs;
                  wasm = wasmBuild.wasm;
                  fixtures = ./verifiers/typescript/test/fixtures.json;
                };
              in
              {
                csmt-verify-wasm = wasmBuild.wasm;
                csmt-verify-wasm-deps = wasmBuild.deps;
                csmt-verify-wasm-demo = demo;
              }
            else
              { };

          fullPackages = lib.mergeAttrsList [
            project.packages
            linux-artifacts.packages
            macos-artifacts.packages
            info.packages
            docker.packages
            wasmPackages
          ];

        in
        {

          packages = fullPackages // {
            default = fullPackages.mts;
          };
          inherit (project) devShells;
        };

    in
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-darwin" ] perSystem;
}

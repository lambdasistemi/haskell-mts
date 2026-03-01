{ indexState, pkgs, mkdocs, asciinema, ... }:

let
  libOverlay = { lib, pkgs, ... }: { };

  shell = { pkgs, ... }: {
    tools = {
      cabal = { index-state = indexState; };
      cabal-fmt = { index-state = indexState; };
      haskell-language-server = { index-state = indexState; };
      hoogle = { index-state = indexState; };
      fourmolu = { index-state = indexState; };
      hlint = { index-state = indexState; };
      implicit-hie = { index-state = indexState; };
    };
    withHoogle = true;
    buildInputs = [
      pkgs.just
      pkgs.nixfmt-classic
      pkgs.shellcheck
      pkgs.mkdocs
      mkdocs.from-nixpkgs
      mkdocs.asciinema-plugin
      mkdocs.markdown-callouts
      mkdocs.markdown-graphviz
      asciinema.compress
      asciinema.resize
      pkgs.asciinema
    ];
    shellHook = ''
      echo "Entering shell for MTS development"
    '';
  };

  fullyStaticOptions = { pkgs, ... }:
    let libs = with pkgs; [ zlib openssl libffi gmp6 ];
    in {
      enableShared = false;
      enableStatic = true;
      configureFlags = map (l: "--ghc-option=-optl=-L${l}/lib") (libs);
    };
  musl = { pkgs, ... }: {
    packages.mts.components.exes.mts = (fullyStaticOptions { inherit pkgs; });
    doHaddock = false;
  };
  mkProject = ctx@{ lib, pkgs, ... }: {
    name = "mts";
    src = ./..;
    compiler-nix-name = "ghc984";
    shell = shell { inherit pkgs; };
    modules = [ libOverlay ];
  };

  project = pkgs.haskell-nix.cabalProject' mkProject;

in {
  devShells.default = project.shell;
  inherit project;
  packages.mts = project.hsPkgs.mts.components.exes.mts;
  packages.bench = project.hsPkgs.mts.components.benchmarks.bench;
  packages.unit-tests = project.hsPkgs.mts.components.tests.unit-tests;
  musl64 = project.projectCross.musl64.hsPkgs;
}

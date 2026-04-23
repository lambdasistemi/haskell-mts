# Compose the official docs site (MkDocs Material) with the WASM
# browser demos staged in.
#
# `nix build .#docs` produces the full site tree ready to publish to
# any static host. The demo derivations remain independent flake
# outputs; this derivation assembles them into the final MkDocs site so
# CI can treat docs publication as one artifact.
{ pkgs, src, mkdocsAssets, verifyDemo, writeDemo, mpfWriteDemo, }:
let
  asciinemaPlugin =
    pkgs.callPackage "${mkdocsAssets}/nix/asciinema-plugin.nix" { };

  mkdocsEnv = pkgs.python3.withPackages (ps:
    with ps; [
      mkdocs
      mkdocs-material
      pymdown-extensions
      asciinemaPlugin
    ]);
in pkgs.runCommand "haskell-mts-docs" {
  preferLocalBuild = true;
  nativeBuildInputs = [ mkdocsEnv ];
} ''
  cp -r ${src}/docs docs
  cp ${src}/mkdocs.yml mkdocs.yml
  chmod -R u+w docs mkdocs.yml

  mkdir -p docs/demo docs/demo-write docs/demo-write-mpf
  cp -L ${verifyDemo}/* docs/demo/
  cp -L ${writeDemo}/* docs/demo-write/
  cp -L ${mpfWriteDemo}/* docs/demo-write-mpf/
  chmod -R u+w docs

  mkdocs build --strict --site-dir $out
''

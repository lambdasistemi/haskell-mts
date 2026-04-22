# Compose the official docs site (MkDocs Material) with the two
# WASM browser demos staged in.
#
# `nix build .#docs` produces the full /_site/ tree ready to
# publish to any static host. The two demo derivations
# (`csmt-verify-wasm-demo` and `csmt-wasm-write-demo`) are
# independent flake outputs; this derivation composes them into
# the final MkDocs site so CI can collapse "build WASM, stage
# into docs/, run mkdocs" into a single `nix build .#docs`.
{
  pkgs,
  src,
  mkdocsAssets,
  verifyDemo,
  writeDemo,
}:
let
  # asciinema-player plugin isn't in nixpkgs; reuse the package
  # shipped by paolino/dev-assets (which the GitHub Pages deploy
  # workflow already depends on).
  asciinemaPlugin =
    pkgs.callPackage "${mkdocsAssets}/nix/asciinema-plugin.nix" { };

  mkdocsEnv = pkgs.python3.withPackages (
    ps: with ps; [
      mkdocs
      mkdocs-material
      pymdown-extensions
      asciinemaPlugin
    ]
  );
in
pkgs.runCommand "haskell-mts-docs"
  {
    preferLocalBuild = true;
    nativeBuildInputs = [ mkdocsEnv ];
  }
  ''
    # Snapshot doc sources + config into a writable working copy.
    cp -r ${src}/docs docs
    cp ${src}/mkdocs.yml mkdocs.yml
    chmod -R u+w docs mkdocs.yml

    # Stage the two demo bundles at the locations the markdown
    # pages link to.
    mkdir -p docs/demo docs/demo-write
    cp -L ${verifyDemo}/* docs/demo/
    cp -L ${writeDemo}/*  docs/demo-write/
    chmod -R u+w docs

    # --strict fails on any broken intra-docs link.
    mkdocs build --strict --site-dir $out
  ''

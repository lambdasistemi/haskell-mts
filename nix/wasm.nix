# Build csmt-verify to WASM using GHC's WASM backend.
#
# Two-phase strategy, adapted from cardano-addresses's WASM recipe
# but stripped of the crypton / ram / WASI-mmap bits — csmt-verify
# has no C dependencies, so only the pure Haskell graph needs to
# reach the WASI target:
#
#   1. Fetch + truncate Hackage at a pinned index-state
#      (deterministic, via haskell.nix's nix-tools).
#   2. Bootstrap the cabal package cache (mkLocalHackageRepo +
#      cabal v2-update).
#   3. Download package tarballs offline via
#      wasm32-wasi-cabal --only-download (Nix FOD).
#   4. Build WASM offline from the cached deps in a regular
#      derivation.
#
# The single source-repository-package dep is cborg (Hackage's
# 0.2.10.0 is broken on GHC 9.12 WASM).
{
  pkgs,
  ghcWasmToolchain,
  src,
  dependenciesHash,
}:

let
  haskell-nix = pkgs.haskell-nix;
  projectFile = "cabal-wasm.project";

  # Must match cabal-wasm.project. The truncate-index boundary cuts
  # at midnight so project's index-state should be ~1 day before
  # this to guarantee that all intended entries are included.
  # Cap is the latest index-state known to the pinned haskell.nix.
  hackageIndexState = "2026-01-12T00:00:00Z";

  truncatedHackageIndex = pkgs.fetchurl {
    name = "01-index.tar.gz-at-${hackageIndexState}";
    url = "https://hackage.haskell.org/01-index.tar.gz";
    downloadToTemp = true;
    postFetch = ''
      ${haskell-nix.nix-tools}/bin/truncate-index \
        -o $out -i $downloadedFile -s '${hackageIndexState}'
    '';
    outputHashAlgo = "sha256";
    outputHash = (import haskell-nix.indexStateHashesPath).${hackageIndexState};
  };

  mkLocalHackageRepo = haskell-nix.mkLocalHackageRepo;

  bootstrappedHackage =
    pkgs.runCommand "cabal-bootstrap-hackage.haskell.org"
      {
        nativeBuildInputs = [
          haskell-nix.nix-tools.exes.cabal
        ]
        ++ haskell-nix.cabal-issue-8352-workaround;
      }
      ''
        HOME=$(mktemp -d)
        mkdir -p $HOME/.cabal/packages/hackage.haskell.org
        cat <<EOF > $HOME/.cabal/config
        repository hackage.haskell.org
          url: file:${
            mkLocalHackageRepo {
              name = "hackage.haskell.org";
              index = truncatedHackageIndex;
            }
          }
          secure: True
          root-keys: aaa
          key-threshold: 0
        EOF
        cabal v2-update hackage.haskell.org
        cp -r $HOME/.cabal/packages/hackage.haskell.org $out
      '';

  dotCabal =
    pkgs.runCommand "dot-cabal-wasm"
      {
        nativeBuildInputs = [ pkgs.xorg.lndir ];
      }
      ''
        mkdir -p $out/packages/hackage.haskell.org
        lndir ${bootstrappedHackage} $out/packages/hackage.haskell.org

        cat > $out/config <<EOF
        repository hackage.haskell.org
          url: http://hackage.haskell.org/
          secure: True

        executable-stripping: False
        shared: True
        EOF
      '';

  # Deterministic source-repository-package clones.
  cborg-src = pkgs.fetchgit {
    url = "https://github.com/well-typed/cborg.git";
    rev = "72a0e736e24c864b5a9b95d90adb37a9e8e6d761";
    hash = "sha256-SDzMk6gWXelE3OH6gCC6XSn+h5VbrKpaisyza9bCtVM=";
  };

  # Cabal metadata slice used to plan the dep graph without pulling
  # in the native sources (tests, benches, rocksdb stuff).
  srcMetadata = pkgs.lib.cleanSourceWith {
    inherit src;
    filter =
      name: type:
      let
        baseName = baseNameOf (toString name);
      in
      type == "directory" || pkgs.lib.hasSuffix ".cabal" baseName || baseName == projectFile;
  };

  deps = pkgs.stdenv.mkDerivation {
    pname = "csmt-verify-wasm-deps";
    version = "0.1.0";
    src = srcMetadata;

    nativeBuildInputs = [
      ghcWasmToolchain
      pkgs.cacert
      pkgs.git
      pkgs.curl
    ];

    buildPhase = ''
      export HOME=$NIX_BUILD_TOP/home
      mkdir -p $HOME
      export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
      export CURL_CA_BUNDLE=$SSL_CERT_FILE

      export CABAL_DIR=$NIX_BUILD_TOP/cabal
      mkdir -p $CABAL_DIR
      cp -rL ${dotCabal}/* $CABAL_DIR/
      chmod -R u+w $CABAL_DIR

      wasm32-wasi-cabal --project-file=${projectFile} build \
        --only-download csmt-verify-wasm
    '';

    installPhase = ''
      mkdir -p $out
      cp -r $CABAL_DIR/* $out/

      find $out -name 'hackage-security-lock' -delete
      find $out -name '01-index.timestamp' -delete
    '';

    outputHashMode = "recursive";
    outputHash = dependenciesHash;
  };

  wasm = pkgs.stdenv.mkDerivation {
    pname = "csmt-verify-wasm";
    version = "0.1.0";
    inherit src;

    nativeBuildInputs = [
      ghcWasmToolchain
      pkgs.git
    ];

    configurePhase = ''
      export HOME=$NIX_BUILD_TOP/home
      mkdir -p $HOME

      export CABAL_DIR=$NIX_BUILD_TOP/cabal
      mkdir -p $CABAL_DIR
      cp -rL ${deps}/* $CABAL_DIR/
      chmod -R u+w $CABAL_DIR

      # Replace the source-repository-package block with a packages
      # list that points at the pre-fetched nix stores.
      cp ${projectFile} ${projectFile}.orig
      sed -i '/^source-repository-package/,/^$/d' ${projectFile}
      cat >> ${projectFile} <<EOF

      packages:
        mts.cabal
        ${cborg-src}/cborg/cborg.cabal
      EOF
    '';

    buildPhase = ''
      export CABAL_DIR=$NIX_BUILD_TOP/cabal
      wasm32-wasi-cabal --project-file=${projectFile} build csmt-verify-wasm
    '';

    installPhase = ''
      mkdir -p $out
      find dist-newstyle -name "csmt-verify-wasm.wasm" -type f \
        -exec cp {} $out/csmt-verify.wasm \;
    '';
  };

in
{
  inherit deps wasm;
}

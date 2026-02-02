{
  description = "Aiken MPF - Merkle Patricia Forestry reference implementation";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    mpf-src = {
      url = "github:aiken-lang/merkle-patricia-forestry";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, mpf-src }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        nodejs = pkgs.nodejs_22;
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            nodejs
            pkgs.nodePackages.npm
          ];

          shellHook = ''
            echo "Aiken MPF - Merkle Patricia Forestry"
            echo "Reference: https://github.com/aiken-lang/merkle-patricia-forestry"
            echo ""
            echo "Source is available at: ${mpf-src}"
            echo ""

            # Link source if not present
            if [ ! -d "off-chain" ]; then
              echo "Linking off-chain source..."
              ln -sf ${mpf-src}/off-chain .
            fi

            echo "Commands:"
            echo "  cd off-chain && npm install  - Install dependencies"
            echo "  npm test                     - Run tests"
            echo "  npm run build                - Build package"
          '';
        };

        packages.test = pkgs.writeShellApplication {
          name = "mpf-aiken-test";
          runtimeInputs = [ nodejs pkgs.nodePackages.npm ];
          text = ''
            WORKDIR=$(mktemp -d)
            cp -r ${mpf-src}/off-chain/* "$WORKDIR/"
            cd "$WORKDIR"
            export HOME="$WORKDIR"
            npm ci
            npm test
          '';
        };

        # Expose the source for reference
        packages.src = mpf-src;
      });
}

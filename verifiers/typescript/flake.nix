{
  description = "CSMT Proof Verifier - TypeScript library";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        nodejs = pkgs.nodejs_22;
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            nodejs
            pkgs.nodePackages.npm
          ];

          shellHook = ''
            echo "CSMT TypeScript Verifier Development Shell"
            echo "Node.js $(node --version)"
            echo ""
            echo "Commands:"
            echo "  npm install   - Install dependencies"
            echo "  npm test      - Run tests"
            echo "  npm run build - Build package"
          '';
        };

        packages.default = pkgs.buildNpmPackage {
          pname = "csmt-verify";
          version = "0.1.0";
          src = ./.;
          npmDepsHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
          npmBuildScript = "build";
          installPhase = ''
            mkdir -p $out
            cp -r dist $out/
            cp package.json $out/
          '';
        };

        packages.test =
          let
            src = pkgs.lib.cleanSource ./.;
          in
          pkgs.writeShellApplication {
            name = "csmt-verify-test";
            runtimeInputs = [ nodejs ];
            text = ''
              WORKDIR=$(mktemp -d)
              cp -r ${src}/* "$WORKDIR/"
              cd "$WORKDIR"
              export HOME="$WORKDIR"
              npm ci
              npm test
            '';
          };
      }
    );
}

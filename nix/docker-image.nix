{ pkgs, project, version, ... }:

pkgs.dockerTools.buildImage {
  name = "ghcr.io/paolino/mts/mts";
  tag = version;
  config = { EntryPoint = [ "mts" ]; };
  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths = [ project.packages.mts.package.components.exes.mts ];
  };
}

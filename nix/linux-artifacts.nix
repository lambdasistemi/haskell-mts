{ pkgs, version, project, ... }:
let
  mts = project.musl64.mts.components.exes.mts;
  tarball-derivation = pkgs.stdenv.mkDerivation rec {
    pname = "mts";
    inherit version;
    unpackPhase = ''
      mkdir -p $out/unpacked
      cp ${mts}/bin/mts $out/unpacked
      chmod -R +w $out/unpacked/*
    '';
    installPhase = ''
      tar -C $out/unpacked -czvf $out/$pname-$version-linux64.tar.gz .
      rm -rf $out/unpacked
    '';
  };
in { packages.linux64.tarball = tarball-derivation; }

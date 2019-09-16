{ pkgs ? import <nixpkgs> {} }:

let
  haskell = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) { inherit pkgs; };

  cabalPatch = pkgs.fetchpatch {
    url = "https://patch-diff.githubusercontent.com/raw/haskell/cabal/pull/6055.diff";
    sha256 = "145g7s3z9q8d18pxgyngvixgsm6gmwh1rgkzkhacy4krqiq0qyvx";
    stripLen = 1;
  };

  pkgSet = haskell.mkCabalProjectPkgSet {
    plan-pkgs = import ./pkgs.nix;
    pkg-def-extras = [];
    modules = [{
      packages.Cabal.patches = [ cabalPatch ];
      packages.happy.package.buildType = pkgs.lib.mkOverride 10 "Custom";
      packages.happy.package.setup-depends = pkgs.lib.mkOverride 10 [ pkgSet.config.hsPkgs.Cabal ];
    }];
  };

in
  pkgSet.config.hsPkgs

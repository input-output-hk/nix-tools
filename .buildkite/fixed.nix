{ haskellNix ? import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {}
, nixpkgs ? haskellNix.sources.nixpkgs-default }:

let
  pkgs = import nixpkgs haskellNix.nixpkgsArgs;

  pkgSet = pkgs.haskell-nix.mkCabalProjectPkgSet {
    plan-pkgs = import ./pkgs.nix;
    pkg-def-extras = [];
    modules = [];
  };

in
  pkgSet.config.hsPkgs

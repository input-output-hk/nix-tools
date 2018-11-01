{ args ? { crossSystem = (import <nixpkgs> {}).lib.systems.examples.mingwW64; }
, pkgs ? import <nixpkgs> args }:
let
 hsPkgs = import ./pkgs.nix { inherit pkgs; };
in
 hsPkgs // { nix-tools-all-execs = pkgs.symlinkJoin
           { name = "nix-tools";
             paths = builtins.attrValues hsPkgs.nix-tools.components.exes; }; }

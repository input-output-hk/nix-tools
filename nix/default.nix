{ pkgs ? import <nixpkgs> {} }:

(pkgs.lib.fix (self: with self; {
  inherit (pkgs) lib stdenv;
  ghc = pkgs.haskell.compiler.${stackage.compiler.nix-name};
  haskellLib = import ./lib.nix { inherit lib haskellLib; };
  hackage = import ../hackage.nix-master;
  stackage = import ../stackage.nix-master/lts-11.7.nix hackage.exprs;
  new-builder = pkgs.callPackage ./new-builder.nix {
    inherit haskellLib ghc;
    inherit (hackage) hashes;
  };
  cabal = import ./cabal-os-arch-comp.nix;

  compiler = cabal.compiler // {
    isGhc = true;
    version = lib.mapAttrs (_: f: v: f (builtins.compareVersions stackage.compiler.version v)) {
      eq = c: c == 0;
      gt = c: c > 0;
      ge = c: c >= 0;
      lt = c: c < 0;
      le = c: c <= 0;
    };
  };
  system = let
    os = {
      "linux" = "Linux";
      "darwin" = "OSX";
    }.${pkgs.hostPlatform.parsed.kernel.name};
    arch = {
      "x86_64" = "X86_64";
    }.${pkgs.hostPlatform.parsed.cpu.name};
  in cabal.os // { "is${os}" = true; }
    // cabal.arch // { "is${arch}" = true; };

  configs = lib.mapAttrs (_: f: import f {
    inherit hsPkgs pkgconfPkgs compiler system;
    pkgs = pkgs // {
      z = pkgs.zlib;
    };
  }) stackage.packages;

  pkgconfPkgs = pkgs;
  hsPkgs = lib.mapAttrs (_: _: null) stackage.compiler.packages
    // lib.mapAttrs (_: new-builder) configs;
}))

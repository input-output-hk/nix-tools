{ pkgs, lib, haskellLib, ghc, weakCallPackage, plan }:

lib.fix (hsPkgs: pkgs.callPackage "${pkgs.path}/pkgs/development/haskell-modules" {
  haskellLib = pkgs.haskell.lib;
  inherit ghc;
  buildHaskellPackages = hsPkgs;
  compilerConfig = _: _: lib.mapAttrs (_: _: null) plan.compiler.packages;
  initialPackages = self: _:
    lib.mapAttrs
      (_: pkg: self.callPackage ./driver.nix { expr = lib.fix pkg; inherit lib; })
      (plan.packages self);
  packageSetConfig = plan.overlay pkgs;
})

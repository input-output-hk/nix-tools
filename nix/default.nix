{ pkgs ? import <nixpkgs> {}
, planFunc ? import ../stackage/lts-11.4.nix
, hackage ? import ../hackage
}:

(pkgs.lib.fix (self: with self; {
  inherit (pkgs) lib stdenv;
  ghc = pkgs.haskell.compiler.${plan.compiler.nix-name};

  # Avoid pkgs.callPackage for now. It does a lot of nonsense with OOP
  # style programming that we should avoid until we know we want it.
  weakCallPackage = scope: f: args:
    let f' = if lib.isFunction f then f else import f;
        args' = scope // args;
    in f' (builtins.intersectAttrs (builtins.functionArgs f') args');

  haskellLib = import ./lib.nix { inherit lib haskellLib; };

  plan =
    let p = planFunc hackageConfigs;
    in p // {
      packages = lib.mapAttrs
        (_: pkg: self:
          lib.recursiveUpdate (pkg.revision self) { flags = pkg.flags or {}; }
        ) p.packages;
    };
  hackageConfigs =
    let
      args = {
        inherit hsPkgs compiler system pkgconfPkgs;
        pkgs = adjustedPkgs;
      };
      handleVer = vdata:
        let
          revs = builtins.removeAttrs vdata ["sha256"];
          rev2HashedConfig = name: rev: { name = rev.cabalSha256; value = revConfigs.${name}; };
          revConfigs = lib.mapAttrs (_: rev2config vdata.sha256) revs;
        in revConfigs // lib.mapAttrs' rev2HashedConfig revs;
      rev2config = sha256: rev: self: import rev (args // { inherit (self) flags; }) // {
        inherit sha256;
        inherit (rev) cabalFile;
      };
    in lib.mapAttrs (_: lib.mapAttrs (_: handleVer)) hackage;

  new-builder = weakCallPackage pkgs ./new-builder.nix {
    inherit haskellLib ghc weakCallPackage;
  };
  cabal = import ./cabal-os-arch-comp.nix;

  compiler = cabal.compiler // {
    isGhc = true;
    version = lib.mapAttrs (_: f: v: f (builtins.compareVersions plan.compiler.version v)) {
      eq = c: c == 0;
      gt = c: c > 0;
      ge = c: c >= 0;
      lt = c: c < 0;
      le = c: c <= 0;
    };
  };
  system = let
    hostMap = import ./host-map.nix pkgs.stdenv;
  in cabal.os // { "is${hostMap.os}" = true; }
    // cabal.arch // { "is${hostMap.arch}" = true; };

  adjustedPkgs = pkgs // {
    pthread = null;
    "stdc++" = null;
    ssl = pkgs.openssl.dev;
    crypto = pkgs.openssl.dev;
    z = pkgs.zlib;
    GL = pkgs.libGL;
    GLU = pkgs.libGLU;
    alut = pkgs.freealut;
    X11 = pkgs.xorg.libX11;
    Xrandr = pkgs.xorg.libXrandr;
    Xext = pkgs.xorg.libXext;
    Xi = pkgs.xorg.libXi;
    Xxf86vm = pkgs.xorg.libXxf86vm;
    Xcursor = pkgs.xorg.libXcursor;
    Xinerama = pkgs.xorg.libXinerama;
    mysqlclient = pkgs.mysql;
    Imlib2 = pkgs.imlib2;
    asound = pkgs.alsaLib;
    ffi = null;
  };
  pkgconfPkgs = pkgs // {
    cairo-pdf = pkgs.cairo;
    cairo-ps = pkgs.cairo;
    cairo-svg = pkgs.cairo;
    xft = pkgs.xorg.libXft;
    xau = pkgs.xorg.libXau;
    libR = pkgs.R;
    fftw3f = pkgs.fftwFloat;
    fftw3 = pkgs.fftw;
  };

  configs = lib.mapAttrs (_: lib.fix) (plan.packages // {
    stack = self: let super = plan.packages.stack self; in super // { flags = super.flags // { hide-dependency-versions = true; }; };
  });

  hsPkgs = adjustedPkgs
    // lib.mapAttrs (_: _: null) (plan.compiler.packages // { hsc2hs = "0.68.2"; })
    // lib.mapAttrs (_: new-builder) configs;
}))

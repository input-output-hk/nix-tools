{ pkgs ? import <nixpkgs> {}
, planFile ? ../stackage/lts-11.11.nix
}:

(pkgs.lib.fix (self: with self; {
  inherit (pkgs) lib stdenv;
  ghc = pkgs.haskell.compiler.${stackage.compiler.nix-name};
  haskellLib = import ./lib.nix { inherit lib haskellLib; };
  hackage = import ../hackage;
  plan = import planFile (lib.mapAttrs (_: p:
    lib.mapAttrs (ver: vdata:
      let revs = builtins.removeAttrs vdata ["revision" "sha256"];
      in vdata
        // lib.mapAttrs (_: rev: vdata // {revision = rev;}) revs
        // lib.mapAttrs' (_: rev:
            {name = rev.cabalSha256; value = vdata // {revision = rev;};}
          ) revs
    ) p
  ) hackage);
  new-builder = pkgs.callPackage ./new-builder.nix {
    inherit haskellLib ghc;
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

  configs = lib.mapAttrs (_: f: import f.revision {
    inherit hsPkgs compiler system;
    pkgs = pkgs // {
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
  } // {
    inherit (f) sha256;
    cabalFile = if f.revision == f.r0 then null else f.revision.cabalFile;
  }) plan.packages;

  hsPkgs = lib.mapAttrs (_: _: null) (plan.compiler.packages // { hsc2hs = "0.68.2"; })
    // lib.mapAttrs (_: new-builder) configs;
}))

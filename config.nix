{ allowUnsupportedSystem = true;
  wine.build = "wine64";
  packageOverrides = ps: with ps; rec {
    haskell = lib.recursiveUpdate ps.haskell {
      compiler.ghc843 = (ps.haskell.compiler.ghc843.override {
        ghcFlavour = if ps.stdenv.targetPlatform == ps.stdenv.hostPlatform
                     then "perf"
                     else "perf-cross-ncg";
        enableShared = ps.stdenv.targetPlatform == ps.stdenv.hostPlatform;
        enableIntegerSimple = false;
      }).overrideAttrs (drv: {
        dontStrip = true;
        hardeningDisable = [ "stackprotector" "format" ];
        patches = (drv.patches or []) ++ [
          ./patches/ghc/move-iserv-8.4.2.patch
          ./patches/ghc/hsc2hs-8.4.2.patch
          ./patches/ghc/various-8.4.2.patch
          ./patches/ghc/lowercase-8.4.2.patch
          ./patches/ghc/cabal-exe-ext-8.4.2.patch
          ./patches/ghc/dll-loader-8.4.2.patch
          ./patches/ghc/outputtable-assert-8.4.2.patch
          ./patches/ghc/0001-Stop-the-linker-panic.patch
          ./patches/ghc/ghc-8.4.3-Cabal2201-SMP-test-fix.patch
          ./patches/ghc/ghc-8.4.3-Cabal2201-no-hackage-tests.patch
          ./patches/ghc/ghc-8.4.3-Cabal2201-allow-test-wrapper.patch
          ./patches/ghc/ghc-8.4.3-Cabal2201-response-file-support.patch
        ];
        postPatch = (drv.postPath or "") + ''
        autoreconf
        '';
      });
    };
  };
}

{ system
, compiler
, flags
, pkgs
, hsPkgs
, pkgconfPkgs
, ... }:
  {
    flags = {
      development = false;
    };
    package = {
      specVersion = "1.10";
      identifier = {
        name = "cardano-prelude-test";
        version = "0.1.0.0";
      };
      license = "MIT";
      copyright = "2018 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Utility types and functions for testing Cardano";
      description = "Utility types and functions for testing Cardano";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends  = [
          (hsPkgs.base)
          (hsPkgs.aeson)
          (hsPkgs.attoparsec)
          (hsPkgs.base16-bytestring)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-prelude)
          (hsPkgs.cryptonite)
          (hsPkgs.formatting)
          (hsPkgs.hedgehog)
          (hsPkgs.hspec)
          (hsPkgs.pretty-show)
          (hsPkgs.QuickCheck)
          (hsPkgs.quickcheck-instances)
          (hsPkgs.text)
          (hsPkgs.time)
        ];
      };
    };
  } // {
    src = pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-prelude";
      rev = "d3a3dfc451725cc3e17f47677739ba3dc7d9bbee";
      sha256 = "0sh665nsvk42bzp7azpg53ka31y2kpinjn48m164b3fkhcjn4mj3";
    };
    postUnpack = "sourceRoot+=/test; echo source root reset to \$sourceRoot";
  }
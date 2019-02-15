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
        name = "cardano-prelude";
        version = "0.1.0.0";
      };
      license = "MIT";
      copyright = "2018 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "A Prelude replacement for the Cardano project";
      description = "A Prelude replacement for the Cardano project";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends  = [
          (hsPkgs.base)
          (hsPkgs.aeson)
          (hsPkgs.base16-bytestring)
          (hsPkgs.bytestring)
          (hsPkgs.canonical-json)
          (hsPkgs.cborg)
          (hsPkgs.containers)
          (hsPkgs.formatting)
          (hsPkgs.hashable)
          (hsPkgs.mtl)
          (hsPkgs.protolude)
          (hsPkgs.tagged)
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
  }
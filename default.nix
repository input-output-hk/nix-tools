{ haskellNixSrc ? builtins.fetchTarball {
      url = "https://github.com/input-output-hk/haskell.nix/archive/fc31134a3de7a82a9c14a70426a7fda68a26f41a.tar.gz";
      sha256 = "17nhnxj7lg92yb8ngylmqd2ly2vkgi0mg6h5pnhgn4w4mxs17rly";
    }
, nixpkgs ? (import haskellNixSrc {}).sources.nixpkgs-default
, pkgs ? import nixpkgs (import haskellNixSrc {}).nixpkgsArgs
, haskellCompiler ? "ghc865"
}:
let
  project = pkgs.haskell-nix.cabalProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; name = "nix-tools"; };
    ghc = pkgs.haskell-nix.compiler.${haskellCompiler};
    modules = [{
     nonReinstallablePkgs= [ "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
      "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
      # ghcjs custom packages
      "ghcjs-prim" "ghcjs-th"
      "ghc-boot"
      "ghc" "Win32" "array" "binary" "bytestring" "containers"
      "directory" "filepath" "ghc-boot" "ghc-compact" "ghc-prim"
      # "ghci" "haskeline"
      "hpc"
      "mtl" "parsec" "process" "text" "time" "transformers"
      "unix" "xhtml"
      # "stm" "terminfo"
     ];
    }];
  };
in
  project // {
    shell = project.shellFor {
      buildInputs = [ (pkgs.haskell-nix.hackage-package { name = "cabal-install"; version = "3.2.0.0";
    modules = [{
     nonReinstallablePkgs= [ "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
      "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
      # ghcjs custom packages
      "ghcjs-prim" "ghcjs-th"
      "ghc-boot"
      "ghc" "Win32" "array" "binary" "bytestring" "containers"
      "directory" "filepath" "ghc-boot" "ghc-compact" "ghc-prim"
      # "ghci" "haskeline"
      "hpc"
      "mtl" "parsec" "process" "text" "time" "transformers"
      "unix" "xhtml"
      # "stm" "terminfo"
     ];
    }];
      }).components.exes.cabal ];
    };
  }


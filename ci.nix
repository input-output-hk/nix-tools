builtins.mapAttrs (k: _v:
  let
    haskell-nix = builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/83e4d87e6e31662f0a731cb6cf0d0fb9126c7687.tar.gz";
    url = "https://github.com/NixOS/nixpkgs/archive/2255f292063ccbe184ff8f9b35ce475c04d5ae69.tar.gz";
    pkgs = import (builtins.fetchTarball url) {
      system = k;
      overlays = import "${haskell-nix}/overlays";
      config = import "${haskell-nix}/config.nix";
    };
  in
  pkgs.recurseIntoAttrs {
    # These two attributes will appear in your job for each platform.
    nix-tools = pkgs.haskell-nix.cabalProject { src = ./.; };
  }
) {
  x86_64-linux = {};

  # Uncomment to test build on macOS too
  # x86_64-darwin = {};
}
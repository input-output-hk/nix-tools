builtins.mapAttrs (k: _v:
  let
    url = "https://github.com/NixOS/nixpkgs/archive/2255f292063ccbe184ff8f9b35ce475c04d5ae69.tar.gz";
    pkgs = import (builtins.fetchTarball url) { system = k; };
  in
  pkgs.recurseIntoAttrs {
    # These two attributes will appear in your job for each platform.
    nix-tools = pkgs.runCommand "nix-tools-via-cabal-install" {
      buildInputs = with pkgs; [ ghc cabal-install zlib.dev ];
    } ''
        export HOME=$TMP
        mkdir $HOME/.cabal
        touch $HOME/.cabal/config
        cabal new-update
        cabal new-build
    '';
  }
) {
  x86_64-linux = {};
  # Uncomment to test build on macOS too
  # x86_64-darwin = {};
}
{}:

let
  pkgs = import (builtins.fetchGit {
    name = "nixos-unstable-2020-11-20";
    url = "https://github.com/nixos/nixpkgs-channels/";
    ref = "refs/heads/nixos-unstable";
    rev = "84d74ae9c9cbed73274b8e4e00be14688ffc93fe";
  }) {};

in
  (pkgs.haskell.packages.ghc884.callPackage (import ./default.nix) {}).env

{}:

let
  pkgs = import (builtins.fetchGit {
    name = "nixos-unstable-2020-12-25";
    url = "https://github.com/nixos/nixpkgs";
    ref = "refs/heads/nixos-unstable";
    rev = "257cbbcd3ab7bd96f5d24d50adc807de7c82e06d";
  }) {};

in
  (pkgs.haskellPackages.callPackage (import ./default.nix) {}).env

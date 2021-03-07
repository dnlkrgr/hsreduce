{}:

let
  pkgs = import (builtins.fetchGit {
    name = "nixos-unstable-2020-12-25";
    url = "https://github.com/nixos/nixpkgs";
    ref = "refs/heads/nixos-unstable";
    rev = "102eb68ceecbbd32ab1906a53ef5a7269dc9794a";
  }) {};

in
  (pkgs.haskellPackages.callPackage (import ./default.nix) {}).env

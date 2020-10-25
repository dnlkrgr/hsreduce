{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc884", doBenchmark ? false }:

let
  pkgs = import (builtins.fetchGit {
    name = "nixos-unstable-2020-10-25";
    url = "https://github.com/nixos/nixpkgs-channels/";
    ref = "refs/heads/nixpkgs-unstable";
    rev = "502845c3e31ef3de0e424f3fcb09217df2ce6df6";
  }) {};

  haskellPackages = pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage (import ./default.nix) {});
in
  if pkgs.lib.inNixShell then drv.env else drv

{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc884", doBenchmark ? false }:

let
  pkgs = import (builtins.fetchGit {
    name = "nixos-unstable-2020-09-26";
    url = "https://github.com/nixos/nixpkgs-channels/";
    ref = "refs/heads/nixos-unstable";
    rev = "2e6844040e38148a2414e9226a594a2320262596";
  }) {};

  haskellPackages = pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage (import ./default.nix) {});
in
  if pkgs.lib.inNixShell then drv.env else drv

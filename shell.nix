{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc884", doBenchmark ? false }:

let

  # inherit (nixpkgs) pkgs;
  pkgs = import (builtins.fetchGit {
    # Descriptive name to make the store path easier to identify
    name = "nixos-unstable-2020-07-29";
    url = "https://github.com/nixos/nixpkgs-channels/";
    # Commit hash for nixos-unstable as of 2018-09-12
    # `git ls-remote https://github.com/nixos/nixpkgs-channels nixos-unstable`
    ref = "refs/heads/nixos-unstable";
    rev = "28fce082c8ca1a8fb3dfac5c938829e51fb314c8";
  }) {};

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage (import ./default.nix) {});

in

  if pkgs.lib.inNixShell then drv.env else drv

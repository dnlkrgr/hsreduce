{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc881", doBenchmark ? false }:

let
  pkgs = import (builtins.fetchGit {
    name ="nixos-20.03-small";
    url = https://github.com/nixos/nixpkgs-channels/;
    ref = "nixos-20.03-small";
    rev = "afeaca75cf7bd6510699821202c25cbaf778b1ef";
  }) {};

  f = { mkDerivation, base, ghc, hashable, haskell-names
      , haskell-src-exts, stdenv, syb, text, mtl, transformers
      , bytestring, temporary, filepath, directory, ghc-paths, ghc-lib-parser, aeson, random, time, ormolu
      }:
      mkDerivation {
        pname = "hsreduce";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base ghc hashable haskell-names haskell-src-exts syb text mtl transformers bytestring temporary filepath directory ghc-lib-parser aeson random time ormolu
        ];
        homepage = "dnlkrgr.com";
        description = "Minimizing Haskell programs for easier debugging of GHC bugs";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in
  if pkgs.lib.inNixShell then drv.env else drv
{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  # inherit (nixpkgs) pkgs;
  pkgs = import (builtins.fetchGit {
    # Descriptive name to make the store path easier to identify
    name = "nixos-unstable-2018-09-12";
    url = "https://github.com/nixos/nixpkgs-channels/";
    # Commit hash for nixos-unstable as of 2018-09-12
    # `git ls-remote https://github.com/nixos/nixpkgs-channels nixos-unstable`
    ref = "refs/heads/nixos-unstable";
    rev = "029a5de08390bb03c3f44230b064fd1850c6658a";
  }) {};

  f = { mkDerivation, aeson, async, base, bytestring, Cabal
      , containers, directory, extra, filepath, generic-deriving, ghc
      , ghc-boot-th, ghc-paths, hashable, haskell-names, haskell-src-exts
      , hie-bios, hse-cpp, megaparsec, monad-par, MonadRandom, mtl, path
      , process, QuickCheck, random, regex, stdenv, syb, temporary, text
      , time, transformers, uniplate, unix, word8
      }:
      mkDerivation {
        pname = "hsreduce";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson async base bytestring Cabal containers directory extra
          filepath generic-deriving ghc ghc-boot-th ghc-paths hashable
          haskell-names haskell-src-exts hie-bios hse-cpp megaparsec
          monad-par MonadRandom mtl path process QuickCheck random regex syb
          temporary text time transformers uniplate unix word8
        ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv

{ nixpkgs ? import <nixpkgs> {} }:

let
  pkgs = import (builtins.fetchGit {
    name ="nixos-20.03-small";
    url  = https://github.com/nixos/nixpkgs-channels/;
    ref  = "nixos-20.03-small";
    rev  = "afeaca75cf7bd6510699821202c25cbaf778b1ef";
  }) {};

  haskellPackages = pkgs.haskell.packages.ghc865;

  drv = 
    with haskellPackages;
    mkDerivation {
      pname = "hsreduce";
      version = "0.1.0.0";
      src = ./.;
      buildDepends = [
        MonadRandom
        QuickCheck
        aeson
        base 
        bytestring
        Cabal
        cabal-install
        containers 
        directory 
        extra 
        filepath
        ghc 
        ghc-boot-th
        ghc-paths 
        pkgs.ghcid
        hashable
        haskell-names
        haskell-src-exts
        megaparsec
        mtl
        random
        syb
        syb 
        temporary
        text 
        time
        transformers
        uniplate
        unix
        monad-par
        regex
        regex-base
        regex-tdfa
      ];
      homepage    = "dnlkrgr.com";
      description = "Minimizing Haskell programs for easier debugging of GHC crashes";
      license     = stdenv.lib.licenses.bsd3;
    };
in 
  drv.env
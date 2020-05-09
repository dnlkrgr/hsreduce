{ nixpkgs ? import <nixpkgs> {} }:

# let
#   pkgs = import (builtins.fetchGit {
#     name ="nixos-20.03-small";
#     url  = https://github.com/nixos/nixpkgs-channels/;
#     ref  = "nixos-20.03-small";
#     rev  = "afeaca75cf7bd6510699821202c25cbaf778b1ef";
#   }) {};

let
  pkgs = import (builtins.fetchGit {
    name = "nixos-unstable";
    url = "https://github.com/nixos/nixpkgs-channels/";
    ref = "refs/heads/nixos-unstable";
    rev = "fce7562cf46727fdaf801b232116bc9ce0512049";
  }) {};

  haskellPackages = pkgs.haskell.packages.ghc8101;

  drv =
    with haskellPackages;
    mkDerivation {
      pname = "hsreduce";
      version = "0.1.0.0";
      src = ./.;
      buildDepends = [
        word8
        MonadRandom
        QuickCheck
        aeson
        async
        base
        bytestring
        Cabal
        # cabal-install
        containers
        directory
        extra
        path
        ghc-boot-th
        ghc-paths
        pkgs.ghcid
        hashable
        megaparsec
        mtl
        random
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

{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let
  pkgs = let
    hostPkgs = import <nixpkgs> {};
    pinnedVersion = hostPkgs.lib.importJSON ./nixpkgs-version.json;
    pinnedPkgs = hostPkgs.fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs-channels";
      inherit (pinnedVersion) rev sha256;
    };
  in import pinnedPkgs {};

  f = { mkDerivation, base, ghc, hashable, haskell-names
      , haskell-src-exts, stdenv, syb, text, mtl, transformers
      , bytestring, temporary, filepath, directory
      }:
      mkDerivation {
        pname = "hsreduce";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base ghc hashable haskell-names haskell-src-exts syb text mtl transformers bytestring temporary filepath directory
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

{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, async, base, bytestring, Cabal
      , cassava, containers, directory, ekg, extra, filepath
      , generic-deriving, ghc, ghc-boot-th, ghc-paths, hashable, hie-bios
      , hse-cpp, hspec, megaparsec, microlens-platform, monad-par
      , MonadRandom, mtl, path, process, QuickCheck, random, regex, split
      , stdenv, stm, syb, temporary, text, time, transformers, uniplate
      , unix, word8
      }:
      mkDerivation {
        pname = "hsreduce";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson async base bytestring Cabal cassava containers directory ekg
          extra filepath generic-deriving ghc ghc-boot-th ghc-paths hashable
          hie-bios hse-cpp hspec megaparsec microlens-platform monad-par
          MonadRandom mtl path process QuickCheck random regex split stm syb
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

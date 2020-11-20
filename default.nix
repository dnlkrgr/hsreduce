{ mkDerivation, aeson, array, base, bytestring, Cabal, cassava
, containers, Diff, edit-distance, ghc, ghc-boot-th, ghc-exactprint
, ghc-paths, hashable, hie-bios, hspec, HUnit, katip, lifted-async
, lifted-base, megaparsec, microlens-platform, monad-control
, MonadRandom, mtl, optparse-generic, path, path-io, process, regex
, split, stdenv, stm-lifted, syb, text, time, transformers-base
, uniplate, unordered-containers, vector, word8
}:
mkDerivation {
  pname = "hsreduce";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array base bytestring Cabal cassava containers Diff
    edit-distance ghc ghc-boot-th ghc-exactprint ghc-paths hashable
    hie-bios HUnit katip lifted-async lifted-base megaparsec
    microlens-platform monad-control MonadRandom mtl optparse-generic
    path path-io process regex split stm-lifted syb text time
    transformers-base uniplate unordered-containers vector word8
  ];
  executableHaskellDepends = [
    aeson array base bytestring Cabal cassava containers Diff
    edit-distance ghc ghc-boot-th ghc-exactprint ghc-paths hashable
    hie-bios HUnit katip lifted-async lifted-base megaparsec
    microlens-platform monad-control MonadRandom mtl optparse-generic
    path path-io process regex split stm-lifted syb text time
    transformers-base uniplate unordered-containers vector word8
  ];
  testHaskellDepends = [
    aeson array base bytestring Cabal cassava containers Diff
    edit-distance ghc ghc-boot-th ghc-exactprint ghc-paths hashable
    hie-bios hspec HUnit katip lifted-async lifted-base megaparsec
    microlens-platform monad-control MonadRandom mtl optparse-generic
    path path-io process regex split stm-lifted syb text time
    transformers-base uniplate unordered-containers vector word8
  ];
  doHaddock = false;
  license = stdenv.lib.licenses.bsd3;
}

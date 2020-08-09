{ mkDerivation, aeson, async, base, bytestring, Cabal, cassava
, containers, directory, edit-distance, ghc, ghc-boot-th, ghc-paths
, hashable, hie-bios, hspec, megaparsec, microlens-platform
, MonadRandom, mtl, path, process, regex, split, stdenv, stm, syb
, temporary, text, time, uniplate, unix, word8
}:
mkDerivation {
  pname = "hsreduce";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base bytestring Cabal cassava containers directory ghc
    ghc-boot-th ghc-paths megaparsec microlens-platform MonadRandom mtl
    path process regex split stm syb temporary text time uniplate unix
    word8
  ];
  executableHaskellDepends = [
    aeson base bytestring Cabal cassava containers directory
    edit-distance ghc ghc-paths hashable hie-bios microlens-platform
    MonadRandom mtl path process stm temporary text time uniplate unix
    word8
  ];
  testHaskellDepends = [
    aeson base bytestring Cabal cassava containers directory ghc
    ghc-paths hspec microlens-platform MonadRandom mtl path process stm
    temporary text time uniplate unix word8
  ];
  doHaddock = false;
  license = stdenv.lib.licenses.bsd3;
}

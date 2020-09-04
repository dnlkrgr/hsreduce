{ mkDerivation, aeson, async, base, bytestring, Cabal, cassava
, containers, directory, edit-distance, ghc, ghc-boot-th
, ghc-exactprint, ghc-paths, hashable, hie-bios, hspec, megaparsec
, microlens-platform, MonadRandom, mtl, optparse-generic, path
, process, regex, split, stdenv, stm, syb, temporary, text, time
, uniplate, unix, word8
}:
mkDerivation {
  pname = "hsreduce";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base bytestring Cabal cassava containers directory
    edit-distance ghc ghc-boot-th ghc-exactprint ghc-paths hashable
    hie-bios megaparsec microlens-platform MonadRandom mtl
    optparse-generic path process regex split stm syb temporary text
    time uniplate unix word8
  ];
  executableHaskellDepends = [
    aeson async base bytestring Cabal cassava containers directory
    edit-distance ghc ghc-boot-th ghc-exactprint ghc-paths hashable
    hie-bios megaparsec microlens-platform MonadRandom mtl
    optparse-generic path process regex split stm syb temporary text
    time uniplate unix word8
  ];
  testHaskellDepends = [
    aeson async base bytestring Cabal cassava containers directory
    edit-distance ghc ghc-boot-th ghc-exactprint ghc-paths hashable
    hie-bios hspec megaparsec microlens-platform MonadRandom mtl
    optparse-generic path process regex split stm syb temporary text
    time uniplate unix word8
  ];
  doHaddock = false;
  license = stdenv.lib.licenses.bsd3;
}

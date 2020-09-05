{ mkDerivation, aeson, async, base, bytestring, Cabal, cassava
, containers, edit-distance, ghc, ghc-boot-th, ghc-exactprint
, ghc-paths, hashable, hie-bios, hspec, megaparsec
, microlens-platform, MonadRandom, mtl, optparse-generic, path
, path-io, process, regex, split, stdenv, stm, syb, text, time
, uniplate, unix, word8
}:
mkDerivation {
  pname = "hsreduce";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base bytestring Cabal cassava containers edit-distance
    ghc ghc-boot-th ghc-exactprint ghc-paths hashable hie-bios
    megaparsec microlens-platform MonadRandom mtl optparse-generic path
    path-io process regex split stm syb text time uniplate unix word8
  ];
  executableHaskellDepends = [
    aeson async base bytestring Cabal cassava containers edit-distance
    ghc ghc-boot-th ghc-exactprint ghc-paths hashable hie-bios
    megaparsec microlens-platform MonadRandom mtl optparse-generic path
    path-io process regex split stm syb text time uniplate unix word8
  ];
  testHaskellDepends = [
    aeson async base bytestring Cabal cassava containers edit-distance
    ghc ghc-boot-th ghc-exactprint ghc-paths hashable hie-bios hspec
    megaparsec microlens-platform MonadRandom mtl optparse-generic path
    path-io process regex split stm syb text time uniplate unix word8
  ];
  doHaddock = false;
  license = stdenv.lib.licenses.bsd3;
}

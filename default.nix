{ mkDerivation , Cabal , Diff , HUnit , MonadRandom , aeson , array , base , bytestring , cassava , containers , edit-distance , ghc , ghc-boot-th
, ghc-exactprint , ghc-paths , hashable , haskell-language-server , hie-bios , hspec , katip , lifted-async , lifted-base , megaparsec
, microlens-platform , monad-control , mtl , optparse-generic , path , path-io , process , random-shuffle , regex , split , stdenv , stm-lifted
, syb , text , time , transformers-base , uniplate , unliftio , unordered-containers , vector , word8
}:
let commonPackages = 
      [ aeson Cabal Diff HUnit MonadRandom array base bytestring cassava containers edit-distance ghc ghc-boot-th ghc-exactprint ghc-paths hashable
        haskell-language-server hie-bios katip lifted-async lifted-base megaparsec microlens-platform monad-control mtl optparse-generic
        path path-io process random-shuffle regex split stm-lifted syb text time transformers-base uniplate unliftio unordered-containers
        vector word8
      ];
in mkDerivation {
  pname                     = "hsreduce";
  version                   = "0.1.0.0";
  src                       = ./.;
  executableHaskellDepends  = commonPackages;
  license                   = stdenv.lib.licenses.bsd3;
}

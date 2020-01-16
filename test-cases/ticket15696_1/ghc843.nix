with import (fetchTarball https://github.com/NixOS/nixpkgs/archive/18.09.tar.gz) {};
let ghc = haskell.compiler.ghc843;
in mkShell {
    inherit ghc;
    name = "myEnv";
    buildInputs = [ ghc ];
}

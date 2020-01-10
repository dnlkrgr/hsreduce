with import (fetchTarball https://github.com/NixOS/nixpkgs/archive/18.09.tar.gz) {};
let ghc = haskell.compiler.ghc861;
in mkShell {
    inherit ghc;
    name = "myEnv";
    buildInputs = [ ghc ];
}

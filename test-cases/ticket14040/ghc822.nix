with import (fetchTarball https://github.com/NixOS/nixpkgs/archive/18.03.tar.gz) {}; 

let ghc = haskell.compiler.ghc822;
in mkShell {
    inherit ghc;
    name = "myEnv";
    buildInputs = [ ghc ];
}

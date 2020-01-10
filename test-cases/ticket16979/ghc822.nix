with import (fetchTarball https://github.com/NixOS/nixpkgs/archive/18.09.tar.gz) {};
let ghc = haskell.compiler.ghc822;
in haskell.lib.buildStackProject {
#in mkShell {
    inherit ghc;
    name = "myEnv";
    buildInputs = [ ];
}

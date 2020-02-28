with import (fetchTarball https://github.com/NixOS/nixpkgs/archive/83b35508c6491103cd16a796758e07417a28698b.tar.gz) {};
let ghc = haskell.compiler.ghc841; in mkShell { inherit ghc; name = "myEnv"; buildInputs = [ ghc ]; }

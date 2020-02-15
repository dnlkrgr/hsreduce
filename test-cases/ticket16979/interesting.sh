#!/nix/store/lb3hli8d9536g45mndwfwyi6fpny0blh-bash-interactive-4.4-p23/bin/bash
echo "with import (fetchTarball https://github.com/NixOS/nixpkgs/archive/18.09.tar.gz) {}; let ghc = haskell.compiler.ghc822; in mkShell { inherit ghc; name = \"myEnv\"; buildInputs = [ ghc ]; }" > ghc822.nix
nix-shell ghc822.nix --run "ghc -O -dcore-lint -g -c Ticket16979.hs" |& grep "Core Lint errors : in result of Simplifier"

#!/run/current-system/sw/bin/bash
TICKET=Ticket15696_1
GHC_VERSION=ghc843
RELEASE_OR_COMMIT=18.09
echo "with import (fetchTarball https://github.com/NixOS/nixpkgs/archive/${RELEASE_OR_COMMIT}.tar.gz) {};" > $GHC_VERSION.nix
echo "let ghc = haskell.compiler.${GHC_VERSION}; in mkShell { inherit ghc; name = \"myEnv\"; buildInputs = [ ghc ]; }" >> $GHC_VERSION.nix
COMMAND="ghc -O2 -fforce-recomp ${TICKET}.hs && ./${TICKET}" 

nix-shell $GHC_VERSION.nix --run "${COMMAND}" |& grep "Bin T2 Tip Tip"

rm $TICKET $TICKET.o $TICKET.hi $GHC_VERSION.nix
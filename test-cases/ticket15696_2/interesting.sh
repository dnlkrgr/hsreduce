#!/run/current-system/sw/bin/bash
TICKET=Bug
GHC_843=ghc843
GHC_861=ghc861
RELEASE_OR_COMMIT=18.09

echo "with import (fetchTarball https://github.com/NixOS/nixpkgs/archive/${RELEASE_OR_COMMIT}.tar.gz) {};" > $GHC_843.nix
echo "let ghc = haskell.compiler.${GHC_843}; in mkShell { inherit ghc; name = \"myEnv\"; buildInputs = [ ghc ]; }" >> $GHC_843.nix

echo "with import (fetchTarball https://github.com/NixOS/nixpkgs/archive/${RELEASE_OR_COMMIT}.tar.gz) {};" > $GHC_861.nix
echo "let ghc = haskell.compiler.${GHC_861}; in mkShell { inherit ghc; name = \"myEnv\"; buildInputs = [ ghc ]; }" >> $GHC_861.nix

COMMAND="ghc -O2 -fforce-recomp ${TICKET}.hs && ./${TICKET}" 

nix-shell $GHC_861.nix --run "${COMMAND} |& grep 'Bin T2 Tip (Bin T2 Tip Tip)'" 
# && nix-shell $GHC_843.nix --run "${COMMAND} |& grep 'Bin T2 Tip Tip'"

# rm $TICKET $TICKET.o $TICKET.hi $GHC_861.nix

#!/run/current-system/sw/bin/bash
TICKET=Ticket15753
GHC_VERSION=ghc861
RELEASE_OR_COMMIT=18.09
ERROR=error.txt
echo "with import (fetchTarball https://github.com/NixOS/nixpkgs/archive/${RELEASE_OR_COMMIT}.tar.gz) {};" > $GHC_VERSION.nix
echo "let ghc = haskell.compiler.${GHC_VERSION}; in mkShell { inherit ghc; name = \"myEnv\"; buildInputs = [ ghc ]; }" >> $GHC_VERSION.nix
COMMAND="ghc ${TICKET}.hs"

nix-shell $GHC_VERSION.nix --run "${COMMAND}" 2> $ERROR

grep "Pattern match(es) are non-exhaustive" $ERROR && 
grep "In an equation for ‘mapInsertWithNonEmpty1’:" $ERROR &&
grep "Patterns not matched: _ _ _ (SMkMap _) Refl Refl" $ERROR

rm $TICKET.o $TICKET.hi $ERROR $GHC_VERSION.nix
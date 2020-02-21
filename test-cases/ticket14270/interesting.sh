#!/run/current-system/sw/bin/bash
TICKET=Ticket14270
OUTPUT=output.txt
ERROR=error.txt
GHC_VERSION=ghc822
RELEASE_OR_COMMIT=18.03
echo "with import (fetchTarball https://github.com/NixOS/nixpkgs/archive/${RELEASE_OR_COMMIT}.tar.gz) {};" > $GHC_VERSION.nix
echo "let ghc = haskell.compiler.${GHC_VERSION}; in mkShell { inherit ghc; name = \"myEnv\"; buildInputs = [ ghc ]; }" >> $GHC_VERSION.nix
COMMAND="ghc -O1 -fforce-recomp ${TICKET}.hs -dcore-lint"

nix-shell $GHC_VERSION.nix --run "${COMMAND}" > $OUTPUT 2> $ERROR

grep "Compilation had errors" $ERROR &&
grep "*** Core Lint errors : in result of Float out(FOS {Lam = Just 0," $OUTPUT

rm $OUTPUT $ERROR $GHC_VERSION.nix
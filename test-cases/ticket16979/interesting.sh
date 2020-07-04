#!/run/current-system/sw/bin/bash
TICKET=Bug
GHC_VERSION=ghc822
RELEASE_OR_COMMIT=18.09
ERROR=error.txt
OUTPUT=output.txt
echo "with import (fetchTarball https://github.com/NixOS/nixpkgs/archive/${RELEASE_OR_COMMIT}.tar.gz) {};" > $GHC_VERSION.nix
echo "let ghc = haskell.compiler.${GHC_VERSION}; in mkShell { inherit ghc; name = \"myEnv\"; buildInputs = [ ghc ]; }" >> $GHC_VERSION.nix
COMMAND="ghc -O -dcore-lint -g -c ${TICKET}.hs"

nix-shell $GHC_VERSION.nix --run "${COMMAND}" > $OUTPUT 2> $ERROR

grep "Compilation had errors" $ERROR &&
grep "*** Core Lint errors : in result of Simplifier ***" $OUTPUT

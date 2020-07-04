#!/run/current-system/sw/bin/bash
TICKET=Bug
OUTPUT=output.txt
ERROR=error.txt
RELEASE_OR_COMMIT=83b35508c6491103cd16a796758e07417a28698b
GHC_VERSION=ghc841
echo "with import (fetchTarball https://github.com/NixOS/nixpkgs/archive/${RELEASE_OR_COMMIT}.tar.gz) {};" > $GHC_VERSION.nix
echo "let ghc = haskell.compiler.${GHC_VERSION}; in mkShell { inherit ghc; name = \"myEnv\"; buildInputs = [ ghc ]; }" >> $GHC_VERSION.nix
COMMAND="ghc -O -dcore-lint -g -c ${TICKET}.hs"

nix-shell $GHC_VERSION.nix --run "${COMMAND}" > $OUTPUT 2> $ERROR

grep "Compilation had errors" $ERROR &&
grep "*** Core Lint errors : in result of Simplifier ***" $OUTPUT &&
grep "The type of this binder is unlifted:" $OUTPUT

# rm $OUTPUT $ERROR $GHC_VERSION.nix

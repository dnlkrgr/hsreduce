#!/run/current-system/sw/bin/bash

OUTPUT=output.txt
ERROR=error.txt

ghc -O -dcore-lint -g -c Bug.hs > $OUTPUT 2> $ERROR

grep "Compilation had errors" $ERROR &&
grep "*** Core Lint errors : in result of Simplifier ***" $OUTPUT &&
grep "The type of this binder is unlifted:" $OUTPUT

# rm $OUTPUT $ERROR $GHC_VERSION.nix

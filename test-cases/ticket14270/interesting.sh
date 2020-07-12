#!/run/current-system/sw/bin/bash
TICKET=Bug
OUTPUT=output.txt
ERROR=error.txt
COMMAND="ghc -O1 -fforce-recomp ${TICKET}.hs -dcore-lint"

${COMMAND} > $OUTPUT 2> $ERROR

grep "Compilation had errors" $ERROR &&
grep "*** Core Lint errors : in result of Float out(FOS {Lam = Just 0," $OUTPUT


#!/run/current-system/sw/bin/bash

ghc Bug.hs 2> error.txt

grep "error:ghc: panic! (the 'impossible' happened)" error.txt &&
grep "No skolem info:" error.txt &&
grep "pprPanic, called at compiler/typecheck/TcErrors.hs" error.txt

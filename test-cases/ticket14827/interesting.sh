#!/run/current-system/sw/bin/bash
A=$({ time nix-shell ghc822.nix --run "echo main | ghci Bug.hs" ; } |& grep real | cut -f2 | sed -r 's/0m([0-9]*)\..*/\1/')

nix-shell ghc822.nix --run "ghc -O1 Bug.hs"
B=$({ time ./Bug ; } |& grep real | cut -f2 | sed -r 's/0m([0-9]*)\..*/\1/')

RATIO=$(expr $B / $A)


[[ $RATIO -ge 2 ]]

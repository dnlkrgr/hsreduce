#!/run/current-system/sw/bin/bash

nix-shell ghc881.nix --run "ghc -O Bug.hs"
A=$({ time --format=%e ./Bug ; } |& grep real | cut -f2)
# A=$({ time ./Bug ; } |& grep real | cut -f2 | sed -r 's/0m([0-9]*)\..*/\1/')

nix-shell ghc881.nix --run "ghc -O2 Bug.hs"
B=$({ time --format=%e ./Bug ; } |& grep real | cut -f2)

echo $A
echo $B
# RATIO=$(expr $B / $A)
# echo $RATIO

# if [ $RATIO -ge 3 ];
# then exit 0;
# else exit 1;
# fi 

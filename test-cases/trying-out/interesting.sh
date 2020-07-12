#!/run/current-system/sw/bin/bash
nix-shell -p ghc --run 'ghc Bug.hs'

#!/nix/store/y2bifi1q9i1kwz0y4vzqrqmzw90rdrvl-system-path/bin/bash
nix-shell ghc841.nix --run 'ghc -O -dcore-lint -g -c Ticket14779.hs' |& grep '*** Core Lint errors : in result of Simplifier ***'

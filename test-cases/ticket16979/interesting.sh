#!/nix/store/lb3hli8d9536g45mndwfwyi6fpny0blh-bash-interactive-4.4-p23/bin/bash
nix-shell ghc822.nix --run "ghc -O -dcore-lint -g -c Ticket16979.hs" > normal_output.txt || grep "Core Lint errors : in result of Simplifier" normal_output.txt

#!/nix/store/lb3hli8d9536g45mndwfwyi6fpny0blh-bash-interactive-4.4-p23/bin/bash
nix-shell ghc822.nix --run "ghc -O1 -fforce-recomp Ticket14270.hs -dcore-lint" |& grep "Core Lint errors"

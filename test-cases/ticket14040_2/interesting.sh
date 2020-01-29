#!/nix/store/lb3hli8d9536g45mndwfwyi6fpny0blh-bash-interactive-4.4-p23/bin/bash
nix-shell ghc822.nix --run "ghc Ticket14040_2.hs" |& grep "panic! (the 'impossible' happened)"

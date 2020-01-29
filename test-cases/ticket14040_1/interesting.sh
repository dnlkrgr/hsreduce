#!/nix/store/lb3hli8d9536g45mndwfwyi6fpny0blh-bash-interactive-4.4-p23/bin/bash
nix-shell ghc802.nix --run "ghc Ticket14040_1.hs" |& grep "panic! (the 'impossible' happened)"

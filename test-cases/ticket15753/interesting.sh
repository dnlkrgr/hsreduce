#!/nix/store/lb3hli8d9536g45mndwfwyi6fpny0blh-bash-interactive-4.4-p23/bin/bash
nix-shell ghc861.nix --run "ghc Ticket15753.hs" |& grep "Pattern match(es) are non-exhaustive"

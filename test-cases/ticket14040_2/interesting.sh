#!/nix/store/lb3hli8d9536g45mndwfwyi6fpny0blh-bash-interactive-4.4-p23/bin/bash
nix-shell ghc822.nix --run "ghc Ticket14040.hs" > normal_output.txt 2> error_output.txt || grep "panic" error_output.txt && grep "No skolem info:" error_output.txt

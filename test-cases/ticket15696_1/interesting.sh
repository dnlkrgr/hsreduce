#!/nix/store/lb3hli8d9536g45mndwfwyi6fpny0blh-bash-interactive-4.4-p23/bin/bash
nix-shell ghc843.nix --run "ghc -O2 -fforce-recomp Ticket15696.hs && ./Ticket15696" > normal_output.txt && grep "Bin T2 Tip Tip" normal_output.txt
rm Ticket15696 Ticket15696.o Ticket15696.hi

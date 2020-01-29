#!/nix/store/lb3hli8d9536g45mndwfwyi6fpny0blh-bash-interactive-4.4-p23/bin/bash
nix-shell ghc843.nix --run "ghc -O2 -fforce-recomp Ticket15696_1.hs && ./Ticket15696_1" |& grep "Bin T2 Tip Tip" 
rm Ticket15696_1 Ticket15696_1.o Ticket15696_1.hi

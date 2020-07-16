#!/run/current-system/sw/bin/bash
START1=$(date +"%s")
echo main | ghci Bug.hs > /dev/null
END1=$(date +"%s")
A=$(expr $END1 - $START1)
 

ghc -O1 Bug.hs
START2=$(date +"%s")
./Bug > /dev/null
END2=$(date +"%s")
B=$(expr $END2 - $START2)

RATIO=$(expr $B / $A)



[[ $RATIO -ge 7 ]]

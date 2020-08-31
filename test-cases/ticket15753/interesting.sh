#!/usr/bin/env bash
TICKET=Bug
ERROR=error.txt

timeout 1s ghci Bug.hs > $ERROR 2>&1

grep "Pattern match(es) are non-exhaustive" $ERROR && 
grep "In an equation for ‘mapInsertWithNonEmpty1’:" $ERROR &&
grep "Patterns not matched: _ _ _ (SMkMap _) Refl Refl" $ERROR

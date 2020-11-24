module Expr where

import Control.Monad

main = do
    putStrLn a
    when undefined $ pure ()

a = case compare 3 4 of
    GT -> b
    EQ -> undefined
    
b = "arst"

c p n = [p, p + p .. n]

d = [ undefined | (c, _) <- undefined ]

e = (\_ -> "arst") 42

f = id "arst"

g = \h -> 3 * h + 5 - 512

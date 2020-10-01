module Expr where

main = do
    putStrLn brst

brst = case compare 3 4 of
    GT -> crst
    EQ -> undefined
    
crst = "arst"

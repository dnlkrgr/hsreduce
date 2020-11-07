module Params where

brst = arst '1' 2 "3"

arst :: Char -> Int -> String -> ()
arst '4' _ "6" = undefined

crst = undefined <@@> [3]
_ <@@> rhs = undefined

toListOf l = foldrOf l undefined ()
foldrOf l _ _ = undefined . l

class Arst a where
    drst :: Char -> a -> ()

instance Arst Int where
    drst '4' _ = undefined

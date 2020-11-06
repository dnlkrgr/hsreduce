module Arst () where

data Arst a b = Arst { _first :: a,  _second :: b}
-- data Arst a b = Arst a b

e :: Arst Int Char -> Arst () ()
e (Arst 3 'a') = Arst undefined undefined


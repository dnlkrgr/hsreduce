module Arst () where

data Arst a b = Arst { _first :: a,  _second :: b}
-- data Arst a b = Arst a b

e :: Arst () () -> Arst () ()
e (Arst _ _) = Arst undefined undefined


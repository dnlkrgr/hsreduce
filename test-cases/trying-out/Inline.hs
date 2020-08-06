module Inline where

data Arst = Arst String

type Brst = Int

f :: Arst -> ()
f (Arst "arst") = ()

g :: Brst -> ()
g 3 = ()

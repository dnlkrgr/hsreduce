{-# language TypeFamilies #-}

import GHC.Generics

main = undefined

type family F a b where
  F a b = a

arst :: F Int Char -> String
arst = undefined

type family G a b where
  G a b = String

brst :: G Int Char -> String
brst = undefined

type family Zip a b where
  Zip (_ s) (_ m t) = M1 () m (Zip s t)

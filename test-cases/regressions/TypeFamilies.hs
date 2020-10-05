{-# language TypeFamilies, DataKinds, PolyKinds, UndecidableInstances #-}

import GHC.Generics
import GHC.TypeLits

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

type family IfEq a b t f where
  IfEq a a t _ = t

type family LookupParam (a :: k) (p :: Nat) :: Maybe Nat where
  LookupParam (a (_ (m))) n = IfEq m n ('Just 0) ('Just 1)

type family MaybeAdd b where
  MaybeAdd b = 'Just (b)

type family AnotherLookupParam (p :: Nat) :: Maybe Nat where
  AnotherLookupParam n = MaybeAdd 1

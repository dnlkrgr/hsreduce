{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DeriveGeneric, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, InstanceSigs, PolyKinds, RankNTypes, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
import GHC.Generics
import GHC.TypeLits
arst :: Int -> String
arst = undefined
brst :: String
brst = undefined
type family F a b where
  F a b = a
type family G a b where
  G a b = String
type family MaybeAdd b where
  MaybeAdd b = 'Just (b)
type family AnotherLookupParam (p :: Nat) :: Maybe Nat where
  AnotherLookupParam n = MaybeAdd 1
type family LookupParam (p :: Nat) :: Maybe Nat where
  LookupParam n = ('Just 0)
type family IfEq (b :: k) (t :: l) (f :: l) :: l where
  IfEq a t _ = t
main = undefined

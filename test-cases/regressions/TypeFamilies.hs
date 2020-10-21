{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

import GHC.Generics
import GHC.TypeLits

arst :: F Int Char -> String
arst = undefined

brst :: G Int Char -> String
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
  LookupParam n = IfEq n ('Just 0) ('Just 1)

type family IfEq (b :: k) (t :: l) (f :: l) :: l where
  IfEq a t _ = t

main = undefined

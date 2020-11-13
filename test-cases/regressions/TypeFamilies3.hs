{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances           #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}

import GHC.Generics
import GHC.TypeLits

type family LookupParam (p :: Nat) :: Maybe Nat where
  LookupParam n = IfEq n ('Just 0) ('Just 1)

type family IfEq (b :: k) (t :: l) (f :: l) :: l where
  IfEq a t _ = t

main = undefined

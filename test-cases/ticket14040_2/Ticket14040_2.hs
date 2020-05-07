{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
module Eliminator (
    ) where
import Data.Kind
data family Sing (a :: k)
type family Apply f (x :: k1)
type a @@ b = Apply a b
data FunArrow = (:->)
class FunType arr where
  type Fun k1 arr k2
class () => AppType arr where
  type App k1 arr k2 (f :: Fun k1 arr k2) (x :: k1) :: k2
type FunApp arr = (FunType arr, AppType arr)
instance FunType (:->) where
  type Fun k1 (:->) k2 = k1 -> k2
instance AppType (:->) where
  type App k1 (:->) k2 f x = f x
listElimTyFun ::
  Sing l
  -> p @@ '[]
     -> (forall x xs. Sing x -> Sing xs -> p @@ xs -> p @@ (x : xs))
        -> p @@ l
listElimTyFun = listElimPoly @(:->) @() @() @()
listElimPoly ::
  FunApp arr =>
  Sing l
  -> App [a] arr Type p '[]
     -> (forall x xs.
         Sing x
         -> Sing xs -> App [a] arr Type p xs -> App [a] arr Type p (x : xs))
        -> App [a] arr Type p l
listElimPoly _ _ _ = undefined

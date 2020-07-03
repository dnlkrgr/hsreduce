{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
module Eliminator where

import Data.Kind

data Sing b

type a @@ b =  a

data   (:->)

class FunType arr  where
  type Fun c  arr h

class AppType arr  where
  type App c arr h (f :: Fun c arr h) (g :: c) :: h

instance FunType (:->) where
  type Fun c (:->) h = c -> h

instance AppType (:->) where
  type App c (:->) h f  g = f g

type (-?>) c  h  arr  = Fun c arr h

listElimTyFun :: forall  (p :: Type) (l :: a).
                 Sing l
              -> p @@ '[]
              -> (forall (c :: a) (d :: a). Sing c -> Sing d -> p @@ d -> p @@ d)
              -> p @@ l

listElimTyFun = e @(:->) @a @l

e :: forall  (a :: Type) (p :: ([a] -?> Type) arr) (l :: [a]).
                Sing l
             -> App [a] arr Type p '[]
             -> (forall  (d :: [a]).       App [a] arr Type p d)
             -> App [a] arr Type p l
e = f

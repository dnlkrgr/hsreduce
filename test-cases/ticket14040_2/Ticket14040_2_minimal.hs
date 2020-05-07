{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
module Eliminator where

import Data.Kind

data (:->)

class FunType arr  where
  type Fun c  arr h

class AppType arr  where
  type App c arr h (f :: Fun c arr h) (g :: c) :: h

instance FunType (:->) where
  type Fun c (:->) h = c -> h

instance AppType (:->) where
  type App c (:->) h f g = f g

listElimTyFun :: forall  p (l :: a).
                 ()
              -> ()
              -> (forall c d. () -> () -> () -> p )
              -> p
listElimTyFun = e @(:->) @() @l

e :: forall  a (p :: Fun [a] arr Type).
                ()
             -> ()
             -> (forall  d. App [a] arr Type p '[])
             -> App [a] arr Type p '[]
e = f

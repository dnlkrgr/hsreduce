{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}

module Bug where

import Data.Kind
import Data.Type.Equality ((:~:)(..))
import Data.Void

data family Sing :: c  

data instance Sing :: Bool -> Type where
  SFalse :: Sing False
  STrue  :: Sing True

data instance Sing :: [a] -> Type where
  SCons :: Sing f -> Sing  -> Sing (f:d)

data instance Sing :: (a, b) -> Type where
  STuple2 :: Sing f -> e -> Sing '(f, h)

newtype Map k s = MkMap [(k, s)]

data instance Sing :: Map k s -> Type where
  SMkMap :: Sing f -> Sing (MkMap f)

type family MapEmpty where

class PEq a where
  type (f :: a) == (e :: a) :: Bool

class SEq a where
  (%==) :: forall (f :: a) (e :: a).
           Sing f -> Sing e -> Sing (f == e)

mapInsertWithNonEmpty1 :: forall  (g :: [(c,j)])
                                     (k :: c)  (l :: Map c j).
                          SEq c
                       => Sing  -> Sing k -> Sing  -> Sing l
                       -> l :~: MkMap g
                       -> l :~: MapEmpty
                       -> Void
mapInsertWithNonEmpty1 m i q (SMkMap n) Refl Refl
  | STuple2 o r `SCons` p <- n
  , SFalse <- o %== i
  = case i of

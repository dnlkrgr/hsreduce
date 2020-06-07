{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}

{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}

{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module Bug (hm) where

import Control.Applicative
import Data.Kind
import Data.Monoid
import GHC.TypeLits

data Poly (a :: Type) (b :: Type)
  = PCons (Poly b a)

class GenericN (a :: Type) where
  type RepN (a :: Type) :: Type -> Type
  toN :: RepN a x -> a
  fromN :: a -> RepN a x

instance GenericN (Poly a b) where
  type RepN (Poly a b) = Rec (Poly (Param 0) (Param 1)) (Poly b a)
  fromN (PCons x)  = Rec x
  toN (Rec x) = PCons x

hm :: (String -> Const (Endo [String]) String)
   -> Poly Int String -> Const (Endo [String]) (Poly Int String)
hm = param @0

class HasParam (p :: Nat) s a | p s a -> s, p s -> a where
  param :: Applicative g => (a -> g a) -> s -> g s

instance
  ( x ~ ArgAt (Poly a b) n
  , GHasParam n (RepN (Poly a b)) x
  ) => HasParam n (Poly a b) x where
  param = confusing (\f s -> toN <$> gparam @n f (fromN s))
  {-# INLINE param #-}

confusing :: Applicative f => Traversal s a -> (a -> f a) -> s -> f s
confusing t = \f -> lowerYoneda . lowerCurried . t (liftCurriedYoneda . f)
{-# INLINE confusing #-}

newtype Yoneda f a = Yoneda { runYoneda :: forall b. (a -> b) -> f b }

instance Functor (Yoneda f) where
  fmap f m = Yoneda (\k -> runYoneda m (k . f))

instance Applicative f => Applicative (Yoneda f) where
  pure a = Yoneda (\f -> pure (f a))
  Yoneda m <*> Yoneda n = Yoneda (\f -> m (f .) <*> n id)

newtype Curried f a =
  Curried (forall r. f (a -> r) -> f r)

instance Functor f => Functor (Curried f) where
  fmap f (Curried g) = Curried (g . fmap (.f))
  {-# INLINE fmap #-}

instance (Functor f) => Applicative (Curried f) where
  pure a = Curried (fmap ($ a))
  {-# INLINE pure #-}
  Curried mf <*> Curried ma = Curried (ma . mf . fmap (.))
  {-# INLINE (<*>) #-}

lowerYoneda :: Yoneda f a -> f a
lowerYoneda (Yoneda f) = f id

lowerCurried :: Applicative f => Curried f a -> f a
lowerCurried (Curried f) = f (pure id)

liftCurriedYoneda :: Applicative f => f a -> Curried (Yoneda f) a
liftCurriedYoneda fa = Curried (`yap` fa)
{-# INLINE liftCurriedYoneda #-}

yap :: Applicative f => Yoneda f (a -> b) -> f a -> Yoneda f b
yap (Yoneda k) fa = Yoneda (\ab_r -> k (ab_r .) <*> fa)
{-# INLINE yap #-}

type Traversal s a
  = forall f. Applicative f => (a -> f a) -> s -> f s

class GHasParam (p :: Nat) s a where
  gparam :: forall g (x :: Type).  Applicative g => (a -> g a) -> s x -> g (s x)

instance {-# OVERLAPPABLE #-}
  ( GHasParamRec (LookupParam si p) s a
  ) => GHasParam p (Rec si s) a where
  gparam f (Rec x) = Rec <$> gparamRec @(LookupParam si p) f x

class GHasParamRec (param :: Maybe Nat) s a | param s a -> s where
  gparamRec :: forall g.  Applicative g => (a -> g a) -> s -> g s

instance GHasParamRec 'Nothing a c where
  gparamRec _ = pure

instance (HasParam n s a) => GHasParamRec ('Just n) s a where
  gparamRec = param @n

type family LookupParam (a :: k) (p :: Nat) :: Maybe Nat where
  LookupParam (param (n :: Nat)) m = 'Nothing
  LookupParam (a (_ (m :: Nat))) n = IfEq m n ('Just 0) (MaybeAdd (LookupParam a n) 1)
  LookupParam (a _) n = MaybeAdd (LookupParam a n) 1
  LookupParam a _ = 'Nothing

type family MaybeAdd (a :: Maybe Nat) (b :: Nat) :: Maybe Nat where
  MaybeAdd 'Nothing _  = 'Nothing
  MaybeAdd ('Just a) b = 'Just (a + b)

type family IfEq (a :: k) (b :: k) (t :: l) (f :: l) :: l where
  IfEq a a t _ = t
  IfEq _ _ _ f = f

type family ArgAt (t :: k) (n :: Nat) :: j where
  ArgAt (t a) 0 = a
  ArgAt (t a) n = ArgAt t (n - 1)

type family Param :: Nat -> k where
newtype Rec (p :: Type) a x = Rec a


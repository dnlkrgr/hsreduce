{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
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
module Bug  where

import Data.Coerce
import Data.Kind
import GHC.Generics
import GHC.TypeLits

data Poly a b
  = PNil
  | PCons a (Poly b a)
  deriving Generic
d = PCons 0 PNil
f = x (e @0) d
x h = h ( . )

class HasParam p  s t a b |  p s b -> t  where
  e :: Applicative g => (a -> g b) -> s -> g t

instance
  ( GenericN s
  , GenericN t
  , t ~ Infer s (P n a 'PTag) b
  , a ~ ArgAt s n
  , GHasParam n (RepN s) (RepN t) a b
  ) => HasParam n s t a b where
  e = aa (\f s -> q <$> ab @n f (ac s))
aa t = \f -> ad . ae . t (u . f)

newtype Yoneda f a = Yoneda { v :: forall b. (a -> b) -> f b }
instance Functor (Yoneda f)    
instance Applicative (Yoneda f)
newtype Curried f a =          
  Curried { w :: forall r. f (a -> r) -> f r }
instance Functor f => Functor (Curried f) where
  fmap f (Curried g) = Curried (g . fmap (.f))
instance Functor f => Applicative (Curried f) where
  Curried af <*> Curried ag = Curried (ag . af . fmap (.))
ad (Yoneda f) = f id                                               
ae (Curried f) = f (pure id)                                       
u ah = Curried (`ai` ah)                                           
ai (Yoneda k) ah = Yoneda (\y -> k (y .) <*> ah)
class GHasParam p  s t a b where                                   
  ab :: Applicative g => (a -> g b) -> s x -> g (t x)
instance (GHasParam p h h' a b, GHasParam p r r' a b) => GHasParam p (h :*: r) (h' :*: r') a b where
  ab f (h :*: r) = (:*:) <$> ab @p f h <*> ab @p f r
instance (GHasParam p h h' a b, GHasParam p r r' a b) => GHasParam p (h :+: r) (h' :+: r') a b where
  ab f (R1 r) = R1 <$> ab @p f r                                   
instance GHasParam p U1 U1 a b                                     
instance GHasParam p s t a b => GHasParam p (M1 m aj s) (M1 m aj t) a b where
  ab f (M1 x) = M1 <$> ab @p f x                                   
instance                                                           
   GHasParamRec (LookupParam ak p) s t a b
   => GHasParam p (Rec ak s) (Rec al t) a b where
  ab f (Rec (K1 x)) = Rec . K1 <$> am @(LookupParam ak p) f x
class GHasParamRec e  s t a b |  e s b -> t where
  am :: Applicative g => (a -> g b) -> s -> g t
instance GHasParamRec 'Nothing a a c d 
instance HasParam n s t a b => GHasParamRec ('Just n) s t a b where 
  am = e @n                                                        
type family LookupParam (a :: k) (p :: Nat) :: Maybe Nat where
  LookupParam (e (n :: Nat)) m = 'Nothing
  LookupParam (a (o m )) n = IfEq m n ('Just 0) (MaybeAdd (LookupParam a n) 1)
type family MaybeAdd a  b  where                                   
  MaybeAdd ('Just a) b = 'Just b                                   
type family IfEq a  k t  f  where
  IfEq a a t _ = t                                                 
  IfEq _ _ _ f = f                                                 

data Sub where                                                     
  Sub :: Nat -> k -> Sub                                           
type family ReplaceArg (t :: k) (an :: Nat) (ao :: j) :: k where
  ReplaceArg (t a) 0 ao = t ao                                     
  ReplaceArg (t a) an ao =  t   a
type family ReplaceArgs t  ap  where
  ReplaceArgs t '[] = t                                            
  ReplaceArgs t ('Sub n arg ': ss) = ReplaceArgs (ReplaceArg t n arg) ss
type family ArgAt (t :: k) (n :: u) :: j where
  ArgAt (t a) 0 = a                                                
  ArgAt (t a) n = ArgAt t 0                                        
type family Unify a  k where                                       
  Unify (p n o 'PTag) a' = '[ 'Sub n a']
type family Infer s  a'  b  where
  Infer (s a) a' b        
    = ReplaceArgs (s a) (Unify a' b)
data PTag = PTag           
type family P :: u -> k -> PTag -> k
type family Param where     
type family Indexed (t :: k) (i :: u) :: k where
  Indexed (t a) i = Indexed t 1 (Param i)
  Indexed t o     = t        
newtype Rec p  a v = Rec { y :: K1 R a v }
type family Zip a  b  where   
  Zip (w mt m s) (w mt m t)   
    = w mt m (Zip s t)        
  Zip (l r) (l' :+: r')       
    = l' :+: Zip r r'         
  Zip (l :*: r) (l' :*: r')   
    = Zip l l' :*: Zip r r'   
  Zip (aq p) (aq a)           
    = Rec p a                 
class                         
  GenericN a  where           
  type RepN a  :: Type -> Type
  type RepN a = Zip (Rep (Indexed a 0)) (Rep a)
  q :: RepN a v -> a           
  ac :: a -> RepN a v          
instance                       
  ( Coercible (Rep a) (RepN a) 
  , Generic a                  
  ) => GenericN a where        
  ac :: forall v. a -> RepN a v
  ac = coerce (from :: a -> Rep a v)

{-# language AllowAmbiguousTypes #-}
module Typeclasses where

import Data.Coerce

class Arst a where
  arst :: a -> String
  brst :: a

instance Arst Int where
  arst = show
  brst = undefined

class Profunctor p where
  dimap :: ()
  (#.) :: Coercible c b => q b c -> p a b -> p a c
instance Profunctor (->) where
  dimap = undefined
  (#.) _ = coerce
  {-# INLINE (#.) #-}


{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
module MultiParams where

class Arst a b where
  inRelation :: a -> b -> Bool

instance Arst a a where
  inRelation _ _ = True

-- instance Arst [a] (Maybe a) where
--   inRelation

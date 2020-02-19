{-# LANGUAGE MagicHash #-}
module Main where

import qualified Data.Foldable as Foldable
import GHC.Exts (dataToTag#, tagToEnum#, (==#), (<#))

main :: IO ()
main | not_ordered a b = print $ Foldable.foldl' (flip wumbo) (singleton a) b
     | otherwise       = pure ()
  where
    {-# NOINLINE f #-}
    f () = T2
    {-# NOINLINE a #-}
    a = f ()
    {-# NOINLINE b #-}
    b = [f ()]

data T = T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9
  deriving (Eq, Show)

instance Ord Main.T where
  compare a b
    = case dataToTag# a of
        a' -> case dataToTag# b of
                b' -> if tagToEnum# (a' <# b') :: Bool then
                          LT
                      else
                          if tagToEnum# (a' ==# b') :: Bool then
                              EQ
                          else
                              GT

data Set a = Bin !a !(Set a) !(Set a)
           | Tip
  deriving Show

not_ordered :: Ord a => a -> [a] -> Bool
not_ordered _ [] = False
not_ordered x (y : _) = x >= y

wumbo :: Ord a => a -> Set a -> Set a
wumbo x0 = go x0 x0
  where
    go :: Ord a => a -> a -> Set a -> Set a
    go orig _ Tip = singleton orig
    go orig x t@(Bin y l r) = case compare x y of
        LT -> error "not used here"
        GT -> Bin y l (go orig x r)
        EQ -> t
{-# INLINE wumbo #-}

singleton :: a -> Set a
singleton x = Bin x Tip Tip


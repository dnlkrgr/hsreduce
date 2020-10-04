module Typeclasses where

main = do
  putStrLn $ arst (3 :: Int)

class Arst a where
  arst :: a -> String
  brst :: a

instance Arst Int where
  arst = show
  brst = undefined
  {-# INLINE brst #-}

{-# LANGUAGE MagicHash #-}

import Data.Foldable  
import GHC.Exts 

main = print $ foldl' (flip id) (singleton a) b
  where
    f () = T2
    a = f ()
    b = []

data T = T2
  deriving (Eq, Show)

instance Ord Main.T where
  compare a b
    = case dataToTag# a of
        a' -> GT

data Set a = Bin a (Set a) (Set a)
           | Tip
  deriving Show

singleton c = Bin c Tip Tip
{-# LANGUAGE MagicHash #-}
import Data.Foldable  
import GHC.Exts 
main = print $ foldl' (flip wumbo) (singleton a) b
  where
    f () = T2
    a = f ()
    b = []
data T = T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9
  deriving (Eq, Show)
instance Ord Main.T where
  compare a b
    = case dataToTag# a of
        a' -> GT
data Set a = Bin a (Set a) (Set a)
           | Tip
  deriving Show
wumbo  b = b
singleton c = Bin c Tip Tip

import Data.Foldable  

main = print $ foldl' (flip wumbo) (singleton T2) [T2]

data T = T2
  deriving (Eq, Show)

instance Ord T where
  compare a b = GT

data Set a = Bin a (Set a) (Set a)
           | Tip
  deriving Show

wumbo b = c b b
  where
    c d h Tip = singleton d
    c d e (Bin f i g) = Bin f i (c d e g)

singleton e = Bin e Tip Tip

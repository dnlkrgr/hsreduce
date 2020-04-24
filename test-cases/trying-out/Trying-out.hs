{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language Rank2Types, RankNTypes #-}
module TryingOut where 

import Data.Data (gmapT)

data T = T {-# UNPACK #-} !Float

data Dumb = Dumb | AlsoDumb | MoreDumb | T4 | T5

data DumbProduct = StillDumb Int String

data DumbRec = DumbRec {
  a :: Int
  , b :: String
  }

main :: IO ()
main = do
  let arst = myFun Nothing
  return ()

myFun :: Maybe Int -> Int
myFun (Just y) = 
  let a = 2
      b = 3
      c = 5
      d = 7
      e = 9
  in 2 * y + a + c + d + e
myFun Nothing = 42
  where x = 43

instance Show Dumb where
  show Dumb = "Dumb"


brst :: a -> a
brst = undefined

crst
  | undefined > 0 = 3
  | otherwise = undefined

drst
  | undefined > 0 = 3
  | 2 < 3 = 4
  | otherwise = undefined


erst a b c d = case a of
  Nothing ->
    if b
      then c
      else d


frst = grst Nothing

grst Nothing = 2 * n + 3 / 5
  where n = 5

{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language Rank2Types, RankNTypes #-}
module TryingOut () where 

data Dumb = Dumb | AlsoDumb

data DumbProduct = StillDumb Int String

main :: IO ()
main = return ()

myFun :: Maybe Int -> Int
myFun (Just y) = 
  let a = 2
      b = 3
      c = 5
      d = 7
      e = 9
  in 2 * y + a + c + d + e
myFun Nothing = 42

instance Show Dumb where
  show Dumb = "Dumb"

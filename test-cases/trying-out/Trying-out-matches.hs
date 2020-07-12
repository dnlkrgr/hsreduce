{-# language OverloadedStrings #-}
{-# language TypeFamilies #-}
module Matches where



data Dumb = Dumb { a :: Int, b :: String } 


manyMatches :: Int -> Int
manyMatches 0 = 1
manyMatches 1 = 2
manyMatches 2 = 3
manyMatches 3 = 4

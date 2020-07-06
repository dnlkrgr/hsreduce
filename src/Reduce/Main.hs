module Main where

import System.Environment
import Reduce.HsReduce

main :: IO ()
main = do
    x:y:z:_        <- getArgs
    hsreduce x y z

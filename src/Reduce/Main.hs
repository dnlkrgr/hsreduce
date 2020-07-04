module Main where

import System.Environment
import Reduce.HsReduce

main :: IO ()
main = do
    n:x:y:z:_        <- getArgs
    hsreduce (read n) x y z

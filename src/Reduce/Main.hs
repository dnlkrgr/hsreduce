module Main where

import System.Environment
import Reduce.HsReduce

main :: IO ()
main = getArgs >>= \case
    x:y:z:_     -> hsreduce x y z
    _           -> putStrLn "Example usage: \"hsreduce /home/me/hsreduce-test-cases/ticket1234 interesting.sh Bug.hs"

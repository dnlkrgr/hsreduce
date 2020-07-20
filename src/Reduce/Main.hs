module Main where

import System.Environment
import Reduce.HsReduce

main :: IO ()
main = getArgs >>= \case
    n:x:y:z:_     -> hsreduce (read n) x y z
    _           -> putStrLn "Example usage: \"hsreduce /home/me/hsreduce-test-cases/ticket1234 interesting.sh Bug.hs"

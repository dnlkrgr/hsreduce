module Main where

import System.Environment
import HsReduce

main :: IO ()
main = getArgs >>= \case
    n:x:y:z:_   -> hsreduce (read n) x y z
    _           -> putStrLn $ unlines 
                        [ "Usage: hsreduce <# of threads> <dir of test-case> <rel path to test> <rel path to hs file>"
                        , "Example usage: hsreduce /home/me/hsreduce-test-cases/ticket1234 interesting.sh Bug.hs"
                        ]


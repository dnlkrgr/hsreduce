module Main where

import System.Environment
import Reduce.Driver

main :: IO ()
main = getArgs >>= \case
    n:y:z:_   -> hsreduce (read n) y z Nothing
    _           -> putStrLn $ unlines 
                        [ "Usage: hsreduce <# of threads> <rel path to test> <rel path to hs file>"
                        , "Example usage: hsreduce 1 interesting.sh Bug.hs"
                        ]
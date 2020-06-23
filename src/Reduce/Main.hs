module Main where

import Path 
import System.Environment
import Data.Maybe
import Reduce.HsReduce

main :: IO ()
main = do
    x:y:z:_        <- getArgs
    let root       = fromJust $ parseAbsDir x
    let test       = fromJust $ parseRelFile y
    let sourceFile = fromJust $ parseRelFile z

    hsreduce root test sourceFile

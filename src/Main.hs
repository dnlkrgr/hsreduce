module Main where

import Data.Maybe
import Path
import System.Environment
import System.Directory
import Reduce.HsReduce

main :: IO ()
main = do
    x:y:z:_        <- getArgs
    let root       = fromJust $ parseAbsDir x
    let test       = fromJust $ parseRelFile y
    let sourceFile = fromJust $ parseRelFile z

    hsreduce root test sourceFile

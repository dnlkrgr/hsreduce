module Main where

import System.Environment
import Reduce.Driver
import Util.Types
import Options.Generic

main :: IO ()
main = do
    CLIOptions {..} <- unwrapRecord "hsreduce"
    hsreduce numberOfThreads test sourceFile Nothing
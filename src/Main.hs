module Main where

import Reduce.Driver
import Merge.HsAllInOne
import Util.Types
import Options.Generic

main :: IO ()
main = do
    unwrapRecord "hsreduce" >>= \case
        Reduce {..} -> hsreduce numberOfThreads test sourceFile Nothing
        Merge {..} -> hsmerge sourceFile
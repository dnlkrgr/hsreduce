module Main where

import Merge.HsAllInOne
import Options.Generic

import Reduce.Driver
import Reduce.Passes
import Util.Types

main :: IO ()
main =
    unwrapRecord "hsreduce" >>= \case
        Reduce {..} -> hsreduce allActions numberOfThreads test sourceFile
        Merge {..} -> hsmerge sourceFile
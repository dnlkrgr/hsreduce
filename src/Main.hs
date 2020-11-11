module Main where

import Merge.HsAllInOne
import Options.Generic

import Reduce.Driver
import Reduce.Passes
import Util.Types
import qualified Data.Text.IO as TIO
import Path.IO
import Path
import Parser.Parser
import Reduce.Passes.Cabal
import Distribution.PackageDescription.Parsec
import Distribution.Verbosity
import Distribution.Types.GenericPackageDescription

main :: IO ()
main =
    unwrapRecord "hsreduce" >>= \case
        Reduce {..} -> do
            testAbs <- resolveFile' test
            filePathAbs <- resolveFile' sourceFile
            -- 1. parse the test case once at the beginning so we can work on the AST
            -- 2. record all the files in the current directory
            -- 3. record the starting time
            fileContent <- TIO.readFile $ fromAbsFile filePathAbs
            beginState <- parse filePathAbs
            hsreduce allActions numberOfThreads testAbs filePathAbs fileContent beginState
        PackageDesc {..} -> do
            testAbs <- resolveFile' test
            filePathAbs <- resolveFile' sourceFile
            -- 1. parse the test case once at the beginning so we can work on the AST
            -- 2. record all the files in the current directory
            -- 3. record the starting time
            fileContent <- TIO.readFile $ fromAbsFile filePathAbs
            p <- readGenericPackageDescription normal $ fromAbsFile filePathAbs
            let beginState = CabalState False emptyStats p
            hsreduce cabalActions numberOfThreads testAbs filePathAbs fileContent beginState
        Merge {..} -> hsmerge sourceFile
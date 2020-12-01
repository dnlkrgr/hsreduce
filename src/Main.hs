module Main where

import Merge.Merge
import Options.Generic

import Reduce.Driver
import Reduce.Passes
import Util.Types
import qualified Data.Text.IO as TIO
import Path.IO
import Path
import Reduce.Passes.Cabal
import Distribution.PackageDescription.Parsec
import Distribution.Verbosity
import Util.Parser
import Util.Util

main :: IO ()
main =
    unwrapRecord "hsreduce" >>= \case
        Reduce {..} -> case customPassOrdering of
            -- Just (useP passP -> Right myPass) -> 
            --     let newPasses = filterOutPass myPass
            --     in hsreduce newPasses numberOfThreads test sourceFile recordStatistics timeOut debug dontUsePass
            Just (useP passesP -> Right myPasses) -> 
                let newPasses = [mapM_ runPass myPasses]
                in hsreduce newPasses numberOfThreads test sourceFile recordStatistics timeOut debug Nothing
            _ -> hsreduce allActions numberOfThreads test sourceFile recordStatistics timeOut debug Nothing

        Merge {..} -> hsmerge sourceFile
        PackageDesc {..} -> do
            testAbs <- resolveFile' test
            filePathAbs <- resolveFile' sourceFile
            -- 1. parse the test case once at the beginning so we can work on the AST
            -- 2. record all the files in the current directory
            -- 3. record the starting time
            fileContent <- TIO.readFile $ fromAbsFile filePathAbs
            p <- readGenericPackageDescription normal $ fromAbsFile filePathAbs
            let beginState = CabalState False emptyStats p
            hsreduce' cabalActions numberOfThreads testAbs filePathAbs fileContent beginState True timeOut True Nothing
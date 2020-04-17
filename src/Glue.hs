module Glue where

import System.Environment (getArgs)
import System.FilePath.Posix
import System.IO
import Merge.HsAllInOne
import Reduce.HsReduce

glue :: IO ()
glue = do
  hSetBuffering stdout NoBuffering
  putStrLn "this might take 1 - 2 hours"
  myArgs <- getArgs
  if length myArgs /= 3
    then printUsage
    else do
      let [isProject, test, filePath] = myArgs
      if takeExtension test == ".sh" && takeExtension filePath == ".hs" then
        case isProject of
          "--cabal"    -> do
            hsAllInOne filePath
            hsreduce test (fst (splitFileName filePath) ++ "AllInOne.hs")
          "--no-cabal" -> hsreduce test filePath
          _ -> printUsage
      else printUsage

printUsage :: IO ()
printUsage =
    putStrLn . unwords $
      [ "Usage:",
        "Reduce",
        "(--cabal | --no-cabal)" ,
        "<test-file>",
        "(<cabal-file> | <hs-source-file>)"
      ]

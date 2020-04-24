module Glue where

import Path
import System.Environment (getArgs)
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
      let [isProject, t, f] = myArgs
      test <- parseAbsFile t
      filePath <- parseAbsFile f
      if fileExtension test == ".sh" && fileExtension filePath == ".hs" then
        case isProject of
          "--cabal"    -> do
            fileName <- parseRelFile "AllInOne.hs"
            hsAllInOne filePath
            hsreduce test (parent filePath </> fileName)
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

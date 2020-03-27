module Main (main) where

import System.Environment (getArgs)
import HsReduce (hsreduce)
import System.FilePath.Posix
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  myArgs <- getArgs
  if length myArgs /= 3
    then printUsage
    else do
      let [isProject, test, filePath] = myArgs
      if takeExtension test == ".sh" && takeExtension filePath == ".hs" then
        case isProject of
          "--cabal" -> hsreduce True test filePath
          "--no-cabal" -> hsreduce False test filePath
      else printUsage

printUsage :: IO ()
printUsage = 
    putStrLn . unwords $
      [ "Usage:",
        "Reduce",
        "<test-file>",
        "<hs-source-file>"
      ]
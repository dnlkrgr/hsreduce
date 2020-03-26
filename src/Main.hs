module Main (main) where

import System.Environment (getArgs)
import Reduce.Reduce (hsreduce)
import System.FilePath.Posix
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  myArgs <- getArgs
  if length myArgs /= 2 
    then printUsage
    else do
      let [test, filePath] = myArgs
      if takeExtension test == ".sh" && takeExtension filePath == ".hs" then
        hsreduce test filePath
      else printUsage

printUsage :: IO ()
printUsage = 
    putStrLn . unwords $
      [ "Usage:",
        "Reduce",
        "<test-file>",
        "<hs-source-file>"
      ]
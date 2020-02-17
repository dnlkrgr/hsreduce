module Main (main) where

import System.Environment (getArgs)
import Reduce (hsreduce)
import System.FilePath.Posix

main :: IO ()
main = do
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
        "hsreduce",
        "<test-file>",
        "<hs-source-file>"
      ]
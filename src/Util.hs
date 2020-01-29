
module Util (runTest) where

import System.Process
import System.Exit
import System.Timeout
import System.FilePath.Posix

import Types

-- | run the interestingness test on a timeout of 30 seconds
runTest :: FilePath -> IO Interesting
runTest test = do
  let (dirName, testName) = splitFileName test
  maybeExitcode <- timeout (30 * 1000 * 1000) (readCreateProcessWithExitCode ((shell $ "./" ++ testName) {cwd = Just dirName}) "")
  case maybeExitcode of
    Nothing -> return Uninteresting
    Just (exitCode, _, _) ->
      case exitCode of
        ExitFailure _ -> return Uninteresting
        ExitSuccess -> return Interesting
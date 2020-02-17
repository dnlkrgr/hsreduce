
module Util (runTest, debug, debugPrint) where

import System.Process
import System.Exit
import System.Timeout
import System.FilePath.Posix
import Control.Monad.IO.Class

import Types

isInProduction :: Bool
isInProduction = False

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

debug :: MonadIO m => (a -> m ()) -> a -> m ()
debug f s 
  | isInProduction = return ()
  | otherwise = f s

debugPrint :: MonadIO m => String -> m ()
debugPrint = debug (liftIO . putStrLn . ("[debug] " ++))
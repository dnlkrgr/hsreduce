{-# LANGUAGE LambdaCase #-}

module Util where

import System.Process
import System.Exit
import System.Timeout
import System.FilePath.Posix
import Control.Monad.IO.Class
import Ormolu.Parser.Result as OPR (ParseResult, prParsedSource)
import qualified Data.Text.IO as TIO (writeFile)
import Control.Monad.Reader
import Ormolu.Printer (printModule)

import Types

isInProduction :: Bool
isInProduction = True

-- | run the interestingness test on a timeout of 30 seconds
runTest :: FilePath -> IO Interesting
runTest test = do
  let (dirName, testName) = splitFileName test
  timeout (30 * 1000 * 1000) (readCreateProcessWithExitCode ((shell $ "./" ++ testName) {cwd = Just dirName}) "") >>=
    \case
      Nothing -> return Uninteresting
      Just (exitCode, stdout, stderr) ->
        case exitCode of
          ExitFailure errCode -> do
            -- errorPrint $ "stdout: " ++ stdout
            -- errorPrint $ "stderr: " ++ stderr
            return Uninteresting
          ExitSuccess -> return Interesting

writeOrmolu2FileAndTest :: OPR.ParseResult -> ReaderT StubState IO Interesting
writeOrmolu2FileAndTest newOrmolu = do
  StubState test sourceFile oldOrmolu <- ask
  liftIO $ TIO.writeFile sourceFile . printModule $ newOrmolu
  liftIO $ runTest test

debug :: MonadIO m => (a -> m ()) -> a -> m ()
debug f s 
  | isInProduction = return ()
  | otherwise = f s

debugPrint :: MonadIO m => String -> m ()
debugPrint = debug (liftIO . putStrLn . ("[debug] " ++))

errorPrint :: MonadIO m => String -> m ()
errorPrint = debug (liftIO . putStrLn . ("[error] " ++))
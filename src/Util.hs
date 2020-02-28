{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}

module Util where

import Control.Monad.State.Strict
import Data.Data
import Data.Aeson (FromJSON, decode)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Maybe
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Text.IO as TIO (writeFile)
import qualified Data.Text as T
import Ormolu.Parser.Pragma as OPP (Pragma (PragmaLanguage))
import Ormolu.Parser.Result as OPR (ParseResult, prExtensions, prParsedSource)
import Ormolu.Printer (printModule)
import System.Exit
import System.FilePath.Posix
import System.Process
import System.Timeout
import Types

-- | run ghc with -Wunused-binds -ddump-json and delete decls that are mentioned there
runGhc :: FilePath -> FilePath -> OPR.ParseResult -> IO (Maybe [UnusedBindingName])
runGhc test sourceFile oldOrmolu = do
  debugPrint $ "Size of old ormolu: " ++ (show . T.length $ printModule oldOrmolu)
  -- BUG: Ormolu is printing type level lists wrong, example: Unify (p n _ 'PTag) a' = '[ 'Sub n a']
  TIO.writeFile sourceFile (printModule oldOrmolu)
  let extensions = prExtensions oldOrmolu
      maybeLanguagePragmas = fmap concat . traverse getPragmaStrings . filter isLanguagePragma $ extensions
  case maybeLanguagePragmas of
    Nothing -> do
      errorPrint ""
      return Nothing
    Just languagePragmas -> do
      --debugPrint $ "Running `ghc -Wunused-binds -ddump-json` on file: " ++ sourceFile
      let (dirName, fileName) = splitFileName sourceFile
          command = "ghc -Wunused-binds -ddump-json " ++ unwords (("-X" ++) <$> languagePragmas) ++ " " ++ fileName
      timeout (30 * 1000 * 1000) (readCreateProcessWithExitCode ((shell command) {cwd = Just dirName}) "")
        >>= \case
          Nothing -> do
            errorPrint "Process timed out."
            return Nothing
          Just (exitCode, stdout, stderr) -> case exitCode of
            ExitFailure errCode -> do
              TIO.writeFile ("/home/daniel/workspace/hsreduce/debug/debug_" ++ fileName) (printModule oldOrmolu)
              errorPrint $ "Failed running `" ++ command ++ "` with error code " ++ show errCode
              errorPrint "stdout: "
              let tempGhcOutput = map (decode . pack) . drop 1 $ lines stdout :: [Maybe GhcOutput]
              forM_ (map doc $ catMaybes tempGhcOutput) (\s -> putStrLn "" >> putStrLn s)
              errorPrint $ "stderr: " ++ stderr
              return Nothing
            ExitSuccess ->
              if stdout /= ""
                then do
                  -- dropping first line because it doesn't fit into our JSON schema
                  let maybeOutput = map (decode . pack) . drop 1 $ lines stdout :: [Maybe GhcOutput]
                  if Nothing `elem` maybeOutput
                    then do
                      errorPrint "Unable to parse some of the ghc output to JSON."
                      errorPrint $ "Unparsable Output: " ++ stdout
                      return Nothing
                    else do
                      let unusedBindingNames =
                            map (takeWhile (/= '’') . drop 1 . dropWhile (/= '‘') . doc)
                              . filter (isPrefixOf "Opt_WarnUnused" . reason)
                              . map fromJust
                              $ maybeOutput
                      return $ Just unusedBindingNames
                else return Nothing


 

mkT :: (Typeable a, Typeable b) => (b -> b) -> a -> a
mkT f = fromMaybe id (cast f)

everywhereT :: Data a => (forall b. Data b => b -> b) -> a -> a 
everywhereT f x = f (gmapT (everywhereT f) x)


mkM :: (Typeable a, Typeable b, Typeable (m a), Typeable (m b), Monad m) => (b -> m b) -> a -> m a
mkM f = fromMaybe return (cast f)

everywhereM :: (Monad m, Data a) => (forall b. Data b => b -> m b) -> a -> m a
everywhereM f x = do
  x' <- gmapM (everywhereM f) x
  f x'

isInProduction :: Bool
isInProduction = False

-- | run the interestingness test on a timeout of 30 seconds
runTest :: FilePath -> IO Interesting
runTest test = do
  let (dirName, testName) = splitFileName test
  timeout (10 * 1000 * 1000) (readCreateProcessWithExitCode ((shell $ "./" ++ testName) {cwd = Just dirName}) "")
    >>= \case
      Nothing -> do
        errorPrint "runTest: timed out"
        return Uninteresting
      Just (exitCode, stdout, stderr) ->
        case exitCode of
          ExitFailure errCode -> return Uninteresting
          ExitSuccess -> return Interesting

writeOrmolu2FileAndTest :: OPR.ParseResult -> StateT IterState IO Interesting
writeOrmolu2FileAndTest newOrmolu = do
  IterState test sourceFile oldOrmolu <- get
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

isLanguagePragma :: OPP.Pragma -> Bool
isLanguagePragma (PragmaLanguage _) = True
isLanguagePragma _ = False

getPragmaStrings :: OPP.Pragma -> Maybe [String]
getPragmaStrings (PragmaLanguage ss) = Just ss
getPragmaStrings _ = Nothing

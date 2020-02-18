{-# LANGUAGE DeriveGeneric #-}

module Passes.RemoveUnused
  ( reduce,
  )
where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson (FromJSON, decode)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Maybe
import GHC.Generics (Generic)
import HsSyn
import Ormolu.Parser.Result as OPR (ParseResult, prParsedSource)
import Outputable (ppr, showSDocUnsafe)
import SrcLoc (GenLocated (..), Located (..), getLoc, noLoc, unLoc)
import System.Exit
import System.FilePath.Posix
import System.Process
import System.Timeout
import Types
import Util
import System.Random
import qualified Data.Text.IO as TIO (readFile, writeFile)
import Ormolu.Printer (printModule)

newtype Span
  = Span
      { file :: String
      }
  deriving (Eq, Generic, Show)

instance FromJSON Span

data GhcOutput
  = GhcOutput
      { span :: Span,
        doc :: String,
        severity :: String,
        reason :: String
      }
  deriving (Eq, Generic, Show)

instance FromJSON GhcOutput

-- | run ghc with -Wunused-binds -ddump-json and delete decls that are mentioned there
reduce :: FilePath -> FilePath -> OPR.ParseResult -> IO OPR.ParseResult
reduce test sourceFile oldOrmolu = do
  -- TODO: write ormolu to file
  TIO.writeFile sourceFile (printModule oldOrmolu)
  debugPrint $ "Running `ghc -Wunused-binds -ddump-json` on file: " ++ sourceFile
  let (dirName, fileName) = splitFileName sourceFile
  maybeExitcode <- timeout (30 * 1000 * 1000) (readCreateProcessWithExitCode ((shell $ "ghc -Wunused-binds -ddump-json " ++ fileName) {cwd = Just dirName}) "")
  case maybeExitcode of
    Nothing -> do
      errorPrint "Process timed out."
      return oldOrmolu
    Just (exitCode, stdout, stderr) -> case exitCode of
      ExitFailure errCode -> do
        errorPrint $ "Failed running `ghc -Wunused-binds -ddump-json " ++ fileName ++ "` with error code " ++ show errCode
        errorPrint $ "stdout: " ++ stdout
        errorPrint $ "stderr: " ++ stderr
        return oldOrmolu
      ExitSuccess -> do
        -- dropping first line because it doesn't fit into our JSON schema
        let maybeOutput = map (decode . pack) . drop 1 $ lines stdout :: [Maybe GhcOutput]
        if Nothing `elem` maybeOutput
          then do
            errorPrint "Unable to parse some of the ghc output to JSON."
            return oldOrmolu
          else do
            let unUsedBindingNames =
                  map (takeWhile (/= '’') . drop 1 . dropWhile (/= '‘') . doc)
                    . filter ((== "Opt_WarnUnusedTopBinds") . reason)
                    . map fromJust
                    $ maybeOutput
            let allDecls = hsmodDecls . unLoc . prParsedSource $ oldOrmolu
            debugPrint "unUsedBindingNames:"
            mapM_ putStrLn unUsedBindingNames
            runReaderT (tryAllDecls allDecls unUsedBindingNames) (StubState test sourceFile oldOrmolu)

-- TODO: move common traversal in Util
tryAllDecls :: [LHsDecl GhcPs] -> [String] -> ReaderT StubState IO OPR.ParseResult
tryAllDecls [] _ = do
  StubState _ _ oldOrmolu <- ask
  return oldOrmolu
tryAllDecls (declToRemove : rest) unUsedBindingNames = do
  oldConfig@(StubState test sourceFile oldOrmolu) <- ask
  case declToRemove of
    -- TODO: add handling for Type and Data declarations
    L declLoc (ValD _ (FunBind _ funId (MG _ (L _ matches) _) _ _)) -> do
      debugPrint $ "funBind: " ++ showSDocUnsafe (ppr $ unLoc funId)
      if showSDocUnsafe (ppr $ unLoc funId) `elem` unUsedBindingNames
        then do
          let parsedSource = prParsedSource oldOrmolu
              oldModule = unLoc parsedSource
              allDecls = hsmodDecls oldModule
              modifiedDecls = filter (\(L iterLoc iterDecl) -> iterLoc /= declLoc) allDecls
              newOrmolu = oldOrmolu {prParsedSource = L (getLoc parsedSource) (oldModule {hsmodDecls = modifiedDecls})}
          interesting <- writeOrmolu2FileAndTest newOrmolu
          case interesting of
            Uninteresting -> tryAllDecls rest unUsedBindingNames
            Interesting -> do
              debugPrint $ "removing " ++ showSDocUnsafe (ppr funId)
              local (const (oldConfig {_ormolu = newOrmolu})) $ tryAllDecls rest unUsedBindingNames
        else tryAllDecls rest unUsedBindingNames
    _ -> tryAllDecls rest unUsedBindingNames

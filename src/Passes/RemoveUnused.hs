{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Passes.RemoveUnused
  ( reduce,
  )
where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson (FromJSON, decode)
import Data.ByteString.Lazy.Char8 (pack)
import Data.List (isPrefixOf)
import Data.Maybe
import qualified Data.Text.IO as TIO (readFile, writeFile)
import Debug.Trace
import GHC.Generics (Generic)
import HsSyn
import Ormolu.Parser.Result as OPR (ParseResult, prParsedSource, prExtensions)
import Ormolu.Parser.Pragma as OPR (Pragma(PragmaLanguage))
import Ormolu.Printer (printModule)
import Outputable (ppr, showSDocUnsafe)
import SrcLoc (GenLocated (..), Located (..), getLoc, noLoc, unLoc)
import System.Exit
import System.FilePath.Posix
import System.Process
import System.Random
import System.Timeout
import Types
import Util

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
  putStrLn "removing unused bindings"
  -- BUG: Ormolu is printing type level lists wrong, example: Unify (p n _ 'PTag) a' = '[ 'Sub n a']
  TIO.writeFile sourceFile (printModule oldOrmolu)
  let extensions = prExtensions oldOrmolu
  debugPrint $ show extensions
  let maybeLanguagePragmas = fmap concat . traverse languagePragma2String . filter isLanguagePragma $ extensions
  case maybeLanguagePragmas of
    Nothing -> error "oopsie"
    Just languagePragmas -> do
      --debugPrint $ "Running `ghc -Wunused-binds -ddump-json` on file: " ++ sourceFile
      let (dirName, fileName) = splitFileName sourceFile
          command = "ghc -Wunused-binds -ddump-json " ++ unwords (("-X" ++) <$> languagePragmas) ++ " " ++ fileName
      timeout (30 * 1000 * 1000) (readCreateProcessWithExitCode ((shell command) {cwd = Just dirName}) "")
        >>= \case
          Nothing -> do
            errorPrint "Process timed out."
            return oldOrmolu
          Just (exitCode, stdout, stderr) -> case exitCode of
            ExitFailure errCode -> do
              TIO.writeFile ("/home/daniel/workspace/hsreduce/debug/debug_" ++ fileName) (printModule oldOrmolu)
              errorPrint $ "Failed running `" ++ command ++ "` with error code " ++ show errCode
              errorPrint "stdout: "
              let tempGhcOutput = map (decode . pack) . drop 1 $ lines stdout :: [Maybe GhcOutput]
              forM_ (map doc $ catMaybes tempGhcOutput) (\s -> putStrLn "" >> putStrLn s)
              errorPrint $ "stderr: " ++ stderr
              error "aborting"
              return oldOrmolu
            ExitSuccess ->
              if stdout /= ""
                then do
                  -- dropping first line because it doesn't fit into our JSON schema
                  let maybeOutput = map (decode . pack) . drop 1 $ lines stdout :: [Maybe GhcOutput]
                  if Nothing `elem` maybeOutput
                    then do
                      errorPrint "Unable to parse some of the ghc output to JSON."
                      return oldOrmolu
                    else do
                      let unUsedBindingNames =
                            map (takeWhile (/= '’') . drop 1 . dropWhile (/= '‘') . doc)
                              . filter (isPrefixOf "Opt_WarnUnused" . reason)
                              . map fromJust
                              $ maybeOutput
                      let allDecls = hsmodDecls . unLoc . prParsedSource $ oldOrmolu
                      debugPrint "unUsedBindingNames:"
                      mapM_ putStrLn unUsedBindingNames
                      runReaderT (tryAllDecls allDecls unUsedBindingNames) (StubState test sourceFile oldOrmolu)
                else return oldOrmolu

-- TODO: move common allDecls traversal in Util
tryAllDecls :: [LHsDecl GhcPs] -> [String] -> ReaderT StubState IO OPR.ParseResult
tryAllDecls [] _ = do
  StubState _ _ oldOrmolu <- ask
  return oldOrmolu
tryAllDecls (L declLoc (ValD _ (FunBind _ funId (MG _ (L _ matches) _) _ _)) : rest) unUsedBindingNames = do
  oldConfig@(StubState test sourceFile oldOrmolu) <- ask
  debugPrint $ "funBind: " ++ showSDocUnsafe (ppr $ unLoc funId)
  if showSDocUnsafe (ppr $ unLoc funId) `elem` unUsedBindingNames
    then do
      let parsedSource = prParsedSource oldOrmolu
          oldModule = unLoc parsedSource
          allDecls = hsmodDecls oldModule
          modifiedDecls = filter (\(L iterLoc iterDecl) -> iterLoc /= declLoc) allDecls
          newOrmolu = oldOrmolu {prParsedSource = L (getLoc parsedSource) (oldModule {hsmodDecls = modifiedDecls})}
      writeOrmolu2FileAndTest newOrmolu
        >>= \case
          Uninteresting -> tryAllDecls rest unUsedBindingNames
          Interesting -> do
            debugPrint $ "removing " ++ showSDocUnsafe (ppr funId)
            local (const (oldConfig {_ormolu = newOrmolu})) $ tryAllDecls rest unUsedBindingNames
    else tryAllDecls rest unUsedBindingNames
-- TODO: handling for typeclass declaration
tryAllDecls (L declLoc (TyClD _ (DataDecl _ tyId tyVars tyFixity (HsDataDefn _ nd ctxt cType kindSig constructors derivs))) : rest) unUsedBindingNames = do
  oldConfig@(StubState _ _ oldOrmolu) <- ask
  let newConstructors = filter (constructorIsUsed unUsedBindingNames) constructors
      newDecl = 
        TyClD NoExt (DataDecl NoExt tyId tyVars tyFixity (HsDataDefn NoExt nd ctxt cType kindSig newConstructors derivs))
      parsedSource = prParsedSource oldOrmolu
      oldModule = unLoc parsedSource
      allDecls = hsmodDecls oldModule
      modifiedDecls = 
        map (\(L iterLoc iterDecl) -> if iterLoc == declLoc then L declLoc newDecl else L iterLoc iterDecl) allDecls
      newOrmolu = oldOrmolu {prParsedSource = L (getLoc parsedSource) (oldModule {hsmodDecls = modifiedDecls})}
  debugPrint $ "newDecl: " ++ showSDocUnsafe (ppr newDecl)
  debugPrint $ "trying to remove: " ++ showSDocUnsafe (ppr tyId)
  if length newConstructors /= length constructors then 
    writeOrmolu2FileAndTest newOrmolu
      >>= \case
        Uninteresting -> tryAllDecls rest unUsedBindingNames
        Interesting -> do
          debugPrint $ "removing data constructors from " ++ showSDocUnsafe (ppr tyId)
          debugPrint $ "length newConstructors == length constructors - 1 " ++ show (length newConstructors == length constructors - 1)
          local (const (oldConfig {_ormolu = newOrmolu})) $ tryAllDecls rest unUsedBindingNames
  else tryAllDecls rest unUsedBindingNames
tryAllDecls (_ : rest) unUsedBindingNames = tryAllDecls rest unUsedBindingNames

constructorIsUsed :: [String] -> LConDecl GhcPs -> Bool
constructorIsUsed unUsedBindingNames (L _ (ConDeclH98 _ (L _ rdrName) _ _ _ _ _)) = (showSDocUnsafe . ppr $ rdrName) `notElem` unUsedBindingNames
constructorIsUsed unUsedBindingNames (L _ (ConDeclGADT _ names _ _ _ _ _ _)) =
  let debugMsg = "[debug] names of GADT data constructor: " ++ unwords (map (showSDocUnsafe . ppr) names)
      result = all (\(L _ rdrName) -> (showSDocUnsafe . ppr $ rdrName) `notElem` unUsedBindingNames) (traceShow debugMsg names)
  in traceShow ("result: " ++ show result) result

isLanguagePragma :: OPR.Pragma -> Bool
isLanguagePragma (PragmaLanguage _ ) = True
isLanguagePragma _ = False

languagePragma2String :: OPR.Pragma -> Maybe [String]
languagePragma2String (PragmaLanguage ss) = Just ss
languagePragma2String _ = Nothing
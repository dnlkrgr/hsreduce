module Reduce.Passes.RemoveUnused.Imports where

import Data.Foldable
import Control.Monad.State.Strict
import qualified Data.Text as T
import "ghc-lib-parser" HsSyn
import "ghc-lib-parser" Module
import "ghc-lib-parser" SrcLoc
import Ormolu.Parser.Result as OPR (ParseResult, prParsedSource)
import Ormolu.Printer (printModule)
import Util.Types
import Util.Util

-- | run ghc with -Wunused-binds -ddump-json and delete decls that are mentioned there
reduce :: OPR.ParseResult -> R OPR.ParseResult
reduce oldOrmolu = do
  liftIO $ putStrLn "\n***Removing Imports***"
  debugPrint $ "Size of old ormolu: " ++ (show . T.length $ printModule oldOrmolu)
  let oldImports = hsmodImports . unLoc . prParsedSource $ oldOrmolu
  getGhcWarnings oldOrmolu Imports
    >>= \case
      Nothing -> do
          traverse_ removeImport oldImports
          gets _ormolu
      Just unusedBindingNames -> do
          traverse_ (removeUnusedImport unusedBindingNames) oldImports
          gets _ormolu

removeImport :: LImportDecl GhcPs -> R ()
removeImport (ImportName importName) =
    changeImports 
                  (filter (\(ImportName iterName) -> importName /= iterName))
    . _ormolu <$> get
  >>= testAndUpdateState
removeImport _ = return ()

removeUnusedImport :: [BindingName] -> LImportDecl GhcPs -> R ()
removeUnusedImport unusedBindingNames imp@(ImportName importName)
  | moduleNameString importName `elem` unusedBindingNames = removeImport imp
  | otherwise = return ()
removeUnusedImport _ (L _ (XImportDecl _)) = return ()
removeUnusedImport _ (L _ ImportDecl {}) = return ()

pattern ImportName :: forall l pass. ModuleName -> GenLocated l (ImportDecl pass)
pattern ImportName iterName <- L _ (ImportDecl _ _ (L _ iterName) _ _ _ _ _ _ _)
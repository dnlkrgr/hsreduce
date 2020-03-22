module Reduce.Passes.RemoveUnused.Imports where

import Data.Foldable
import Control.Monad.State.Strict
import qualified Data.Text as T
import HsSyn
import Module
import Ormolu.Parser.Result as OPR (ParseResult, prParsedSource)
import Ormolu.Printer (printModule)
import SrcLoc
import Reduce.Types
import Reduce.Util

-- | run ghc with -Wunused-binds -ddump-json and delete decls that are mentioned there
reduce :: OPR.ParseResult -> ReduceM OPR.ParseResult
reduce oldOrmolu = do
  liftIO $ putStrLn "\n***Removing Imports***"
  debugPrint $ "Size of old ormolu: " ++ (show . T.length $ printModule oldOrmolu)
  let oldImports = hsmodImports . unLoc . prParsedSource $ oldOrmolu
  runGhc oldOrmolu Imports
    >>= \case
      Nothing -> do
          traverse_ removeImport oldImports
          _ormolu <$> get
      Just unusedBindingNames -> do
          traverse_ (removeUnusedImport unusedBindingNames) oldImports
          _ormolu <$> get

removeImport :: LImportDecl GhcPs -> ReduceM ()
removeImport (ImportName importName) = do
    oldOrmolu <- _ormolu <$> get
    let newOrmolu = 
          changeImports oldOrmolu 
                        (filter (\(ImportName iterName) -> importName /= iterName))
    testAndUpdateState newOrmolu
removeImport _ = return ()

removeUnusedImport :: [BindingName] -> LImportDecl GhcPs -> ReduceM ()
removeUnusedImport unusedBindingNames imp@(ImportName importName)
  | moduleNameString importName `elem` unusedBindingNames = removeImport imp
  | otherwise = return ()
removeUnusedImport _ (L _ (XImportDecl _)) = return ()
removeUnusedImport _ (L _ ImportDecl {}) = return ()

pattern ImportName :: forall l pass. ModuleName -> GenLocated l (ImportDecl pass)
pattern ImportName iterName <- L _ (ImportDecl _ _ (L _ iterName) _ _ _ _ _ _ _)
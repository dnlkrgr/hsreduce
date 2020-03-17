module Passes.RemoveUnused.Imports where

import Control.Monad.State.Strict
import qualified Data.Text as T
import HsSyn
import Module
import Ormolu.Parser.Result as OPR (ParseResult, prParsedSource)
import Ormolu.Printer (printModule)
import SrcLoc
import Types
import Util

-- | run ghc with -Wunused-binds -ddump-json and delete decls that are mentioned there
reduce :: FilePath -> FilePath -> OPR.ParseResult -> IO OPR.ParseResult
reduce test sourceFile oldOrmolu = do
  putStrLn "\n***Removing Imports***"
  debugPrint $ "Size of old ormolu: " ++ (show . T.length $ printModule oldOrmolu)
  let oldImports = hsmodImports . unLoc . prParsedSource $ oldOrmolu
      startState = ReduceState test sourceFile oldOrmolu
  runGhc sourceFile oldOrmolu Imports
    >>= \case
      Nothing -> 
        _ormolu <$> 
          execStateT (traverse removeImport oldImports) 
                     startState
      Just unusedBindingNames ->
        _ormolu <$> 
          execStateT (traverse (removeUnusedImport unusedBindingNames) oldImports) 
                     startState

removeImport :: LImportDecl GhcPs -> StateT ReduceState IO ()
removeImport (ImportName importName) = do
    oldOrmolu <- _ormolu <$> get
    let newOrmolu = 
          changeImports oldOrmolu 
                        (filter (\(ImportName iterName) -> importName /= iterName))
    testAndUpdateState newOrmolu
removeImport _ = return ()

removeUnusedImport :: [BindingName] -> LImportDecl GhcPs -> StateT ReduceState IO ()
removeUnusedImport unusedBindingNames imp@(ImportName importName)
  | moduleNameString importName `elem` unusedBindingNames = removeImport imp
  | otherwise = return ()
removeUnusedImport _ (L _ (XImportDecl _)) = return ()
removeUnusedImport _ (L _ ImportDecl {}) = return ()

pattern ImportName :: forall l pass. ModuleName -> GenLocated l (ImportDecl pass)
pattern ImportName iterName <- L _ (ImportDecl _ _ (L _ iterName) _ _ _ _ _ _ _)
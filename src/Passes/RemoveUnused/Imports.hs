{-# LANGUAGE LambdaCase #-}

module Passes.RemoveUnused.Imports where

import Control.Monad.State.Strict
import Data.Foldable
import HsSyn
import Module
import Ormolu.Parser.Pragma as OPP (Pragma (PragmaLanguage))
import Ormolu.Parser.Result as OPR (ParseResult, prExtensions, prParsedSource)
import Ormolu.Printer (printModule)
import SrcLoc
import Types
import Util
import Outputable (ppr, showSDocUnsafe)
import qualified Data.Text as T

-- | run ghc with -Wunused-binds -ddump-json and delete decls that are mentioned there
reduce :: FilePath -> FilePath -> OPR.ParseResult -> IO OPR.ParseResult
reduce test sourceFile oldOrmolu = do
  putStrLn "\n***Removing Imports***"
  debugPrint $ "Size of old ormolu: " ++ (show . T.length $ printModule oldOrmolu)
  runGhc test sourceFile oldOrmolu Imports
    >>= \case
      Nothing -> return oldOrmolu
      Just unusedBindingNames -> do
        let oldImports = hsmodImports . unLoc . prParsedSource $ oldOrmolu
        _ormolu <$> execStateT (traverse (removeUnusedImport unusedBindingNames) oldImports) (ReduceState test sourceFile oldOrmolu)

removeUnusedImport :: [BindingName] -> LImportDecl GhcPs -> StateT ReduceState IO ()
removeUnusedImport unusedBindingNames (L loc (ImportDecl _ _ (L _ importName) _ _ _ _ _ _ _))
  | moduleNameString importName `elem` unusedBindingNames = do
    oldOrmolu <- _ormolu <$> get
    let newOrmolu = changeImports oldOrmolu (filter (\(L iterLoc (ImportDecl _ _ (L _ iterName) _ _ _ _ _ _ _)) -> importName /= iterName))
    testAndUpdateState newOrmolu
  | otherwise = return ()
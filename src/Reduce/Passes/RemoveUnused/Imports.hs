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
import Control.Monad.Reader

-- | run ghc with -Wunused-binds -ddump-json and delete decls that are mentioned there
reduce :: OPR.ParseResult -> R OPR.ParseResult
reduce oldOrmolu = do
  sourceFile <- asks _sourceFile
  liftIO $ putStrLn "\n***Removing Imports***"
  debugPrint $ "Size of old ormolu: " ++ (show . T.length $ printModule oldOrmolu)
  let oldImports = hsmodImports . unLoc . prParsedSource $ oldOrmolu
  liftIO (getGhcOutput sourceFile Imports)
    >>= \case
      Nothing -> do
          traverse_ removeImport oldImports
          gets _ormolu
      -- Just unusedStrings -> do
      --     traverse_ (removeUnusedImport unusedStrings) oldImports
      --     gets _ormolu

removeImport :: LImportDecl GhcPs -> R ()
removeImport (ImportName importName) =
    changeImports 
                  (filter (\(ImportName iterName) -> importName /= iterName))
    . _ormolu <$> get
  >>= testAndUpdateState
removeImport _ = return ()

removeUnusedImport :: [String] -> LImportDecl GhcPs -> R ()
removeUnusedImport unusedStrings imp@(ImportName importName)
  | moduleNameString importName `elem` unusedStrings = removeImport imp
  | otherwise = return ()
removeUnusedImport _ (L _ (XImportDecl _)) = return ()
removeUnusedImport _ (L _ ImportDecl {}) = return ()

pattern ImportName :: forall l pass. ModuleName -> GenLocated l (ImportDecl pass)
pattern ImportName iterName <- L _ (ImportDecl _ _ (L _ iterName) _ _ _ _ _ _ _)
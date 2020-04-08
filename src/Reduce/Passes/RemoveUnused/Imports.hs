module Reduce.Passes.RemoveUnused.Imports (reduce) where

import Data.Foldable
import Control.Monad.State.Strict
import qualified Data.Text as T
import Util.Types
import Util.Util
import Control.Monad.Reader
import "ghc" GHC

-- | run ghc with -Wunused-binds -ddump-json and delete imports that are mentioned there
reduce :: R ()
reduce = do
  oldOrmolu <- get
  sourceFile <- asks _sourceFile
  liftIO $ putStrLn "\n***Removing Imports***"
  -- debugPrint $ "Size of old ormolu: " ++ (show . T.length $ showGhc oldOrmolu)
  let oldImports = hsmodImports . unLoc . _parsed $ oldOrmolu
  liftIO (getGhcOutput sourceFile Imports)
    >>= \case
      Nothing -> traverse_ removeImport oldImports
      Just l -> do
          let unusedStrings = map fst l
          traverse_ (removeUnusedImport unusedStrings) oldImports

removeImport :: LImportDecl GhcPs -> R ()
removeImport (ImportName importName) =
    changeImports 
                  (filter (\(ImportName iterName) -> importName /= iterName))
    <$> get
  >>= testAndUpdateState
removeImport _ = return ()

removeUnusedImport :: [T.Text] -> LImportDecl GhcPs -> R ()
removeUnusedImport unusedStrings imp@(ImportName importName)
  | (T.pack $ moduleNameString importName) `elem` unusedStrings = removeImport imp
  | otherwise = return ()
removeUnusedImport _ (L _ (XImportDecl _)) = return ()
removeUnusedImport _ (L _ ImportDecl {}) = return ()

pattern ImportName :: ModuleName -> GenLocated l (ImportDecl GhcPs)
pattern ImportName iterName <- L _ (ImportDecl _ _ (L _ iterName) _ _ _ _ _ _ _)

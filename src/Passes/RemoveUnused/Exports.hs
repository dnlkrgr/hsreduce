{-# LANGUAGE LambdaCase #-}

module Passes.RemoveUnused.Exports where

import Control.Monad.State.Strict
import Data.Foldable
import HsSyn
import Module
import Ormolu.Parser.Pragma as OPP (Pragma (PragmaLanguage))
import Ormolu.Parser.Result as OPR (ParseResult, prExtensions, prParsedSource)
import Ormolu.Printer (printModule)
import Outputable (ppr, showSDocUnsafe)
import SrcLoc
import Types
import Util
import Data.Maybe
import qualified Data.Text as T

-- | run ghc with -Wunused-binds -ddump-json and delete decls that are mentioned there
reduce :: FilePath -> FilePath -> OPR.ParseResult -> IO OPR.ParseResult
reduce test sourceFile oldOrmolu = do
  putStrLn "\n***Removing Exports***"
  debugPrint $ "Size of old ormolu: " ++ (show . T.length $ printModule oldOrmolu)
  runGhc test sourceFile oldOrmolu Imports
    >>= \case
      Nothing -> return oldOrmolu
      Just unusedBindingNames -> do
        let maybeExports = hsmodExports . unLoc . prParsedSource $ oldOrmolu
        case maybeExports of
          Nothing -> return oldOrmolu -- TODO: add handling for Nothing, because this means everything is exported!
          Just (L _ oldExports) ->
            _ormolu
              <$> execStateT
                (traverse removeUnusedExport oldExports)
                (ReduceState test sourceFile oldOrmolu)

removeUnusedExport :: LIE GhcPs -> StateT ReduceState IO ()
removeUnusedExport (L loc export) = do
  oldState@(ReduceState _ _ oldOrmolu) <- get
  let newOrmolu = changeExports oldOrmolu (filter (\(L iterLoc _) -> loc /= iterLoc)) 
  testAndUpdateState newOrmolu () ()
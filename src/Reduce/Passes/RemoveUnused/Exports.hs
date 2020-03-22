module Reduce.Passes.RemoveUnused.Exports where

import Data.Foldable
import Control.Monad.State.Strict
import HsSyn
import Ormolu.Parser.Result as OPR (ParseResult, prParsedSource)
import Ormolu.Printer (printModule)
import SrcLoc
import qualified Data.Text as T
import Data.Maybe
import Module
import Reduce.Types
import Reduce.Util

-- | run ghc with -Wunused-binds -ddump-json and delete decls that are mentioned there

reduce :: OPR.ParseResult -> ReduceM OPR.ParseResult
reduce oldOrmolu = do
  liftIO $ putStrLn "\n***Removing Exports***"
  liftIO $ debugPrint $ "Size of old ormolu: " ++ (show . T.length $ printModule oldOrmolu)
  let L l oldModule = prParsedSource oldOrmolu
      maybeModName  = hsmodName oldModule
      maybeExports  = hsmodExports oldModule
      allDecls      = hsmodDecls . unLoc . prParsedSource $ oldOrmolu
  case maybeExports of
    Nothing -> do
      let oldExports = map fromJust . filter isJust . map (getName . unLoc) $ allDecls
          newModName = 
            case maybeModName of 
              Nothing    -> Just . L noSrcSpan . mkModuleName $ "Bug"
              m@(Just _) -> m
          newModule = oldModule { hsmodExports = Just $ L noSrcSpan oldExports, hsmodName = newModName }
          newOrmolu = oldOrmolu { prParsedSource = L l newModule}
      modify $ \s -> s { _ormolu = newOrmolu }
      traverse_ removeUnusedExport oldExports
      _ormolu <$> get
    Just (L _ oldExports) -> do
          traverse_ removeUnusedExport oldExports
          _ormolu <$> get

removeUnusedExport :: LIE GhcPs -> ReduceM ()
removeUnusedExport (L loc export) = do
  liftIO $ putStrLn $ "trying: " ++ oshow export
  oldOrmolu <- _ormolu <$> get
  let newOrmolu = changeExports oldOrmolu (filter (\(L iterLoc _) -> loc /= iterLoc)) 
  testAndUpdateState newOrmolu

getName :: HsDecl GhcPs -> Maybe (LIE GhcPs)
getName (ValD _ (FunBind _ fId _ _ _))                     = Just . L noSrcSpan . IEVar NoExt      . L noSrcSpan . IEName . L noSrcSpan . unLoc $ fId
getName (TyClD _ (DataDecl _ dId _ _ _))                   = Just . L noSrcSpan . IEThingAll NoExt . L noSrcSpan . IEName . L noSrcSpan . unLoc $ dId
getName (TyClD _ (ClassDecl _ _ cId _ _ _ _ _ _ _ _))      = Just . L noSrcSpan . IEThingAll NoExt . L noSrcSpan . IEName . L noSrcSpan . unLoc $ cId
getName (TyClD _ (SynDecl _ sId _ _ _))                    = Just . L noSrcSpan . IEThingAll NoExt . L noSrcSpan . IEName . L noSrcSpan . unLoc $ sId
getName (TyClD _ (FamDecl _ (FamilyDecl _ _ fId _ _ _ _))) = Just . L noSrcSpan . IEThingAll NoExt . L noSrcSpan . IEName . L noSrcSpan . unLoc $ fId
getName _ = Nothing
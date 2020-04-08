module Reduce.Passes.RemoveUnused.Exports (reduce) where

import Data.Foldable
import Control.Monad.State.Strict
import Data.Maybe
import System.FilePath.Posix
import Util.Types
import Util.Util
import Control.Monad.Reader
import qualified Data.Text as T
import "ghc" GHC

reduce :: R ()
reduce = do
  oldOrmolu <- get
  liftIO $ putStrLn "\n***Removing Exports***"
  liftIO $ debugPrint $ "Size of old ormolu: " ++ (show . T.length . T.pack . showGhc . _parsed $ oldOrmolu)
  let L l oldModule = _parsed oldOrmolu
      maybeModName  = hsmodName oldModule
      maybeExports  = hsmodExports oldModule
      allDecls      = hsmodDecls . unLoc . _parsed $ oldOrmolu
  case maybeExports of
    Nothing -> do
      sourceFile <- asks _sourceFile 
      let (_, modName) = takeWhile (/= '.') <$> splitFileName sourceFile
          oldExports   = map fromJust . filter isJust . map (decl2Export . unLoc) $ allDecls
          newModName   = 
            case maybeModName of 
              Nothing -> Just . L noSrcSpan . mkModuleName $ modName
              m -> m
          newModule = oldModule { hsmodExports = Just $ L noSrcSpan oldExports, hsmodName = newModName }
          newOrmolu = oldOrmolu { _parsed = L l newModule }
      put newOrmolu
      liftIO $ putStrLn $ concatMap ((++ " ") . lshow) oldExports
      -- TODO: if no exports were removed, turn it into Nothing again
      traverse_ removeUnusedExport oldExports
    Just (L _ oldExports) -> traverse_ removeUnusedExport oldExports

removeUnusedExport :: LIE GhcPs -> R ()
removeUnusedExport (L _ export) =
  changeExports (filter ((/= oshow export) . oshow . unLoc)) <$> get
  >>= testAndUpdateState

decl2Export :: HsDecl GhcPs -> Maybe (LIE GhcPs)
decl2Export (ValD _ (FunBind _ fId _ _ _))                     = Just . L noSrcSpan . IEVar NoExt      . L noSrcSpan . IEName . L noSrcSpan . unLoc $ fId
decl2Export (TyClD _ (DataDecl _ dId _ _ _))                   = Just . L noSrcSpan . IEThingAll NoExt . L noSrcSpan . IEName . L noSrcSpan . unLoc $ dId
decl2Export (TyClD _ (ClassDecl _ _ cId _ _ _ _ _ _ _ _))      = Just . L noSrcSpan . IEThingAll NoExt . L noSrcSpan . IEName . L noSrcSpan . unLoc $ cId
decl2Export (TyClD _ (SynDecl _ sId _ _ _))                    = Just . L noSrcSpan . IEThingAll NoExt . L noSrcSpan . IEName . L noSrcSpan . unLoc $ sId
decl2Export (TyClD _ (FamDecl _ (FamilyDecl _ _ fId _ _ _ _))) = Just . L noSrcSpan . IEThingAll NoExt . L noSrcSpan . IEName . L noSrcSpan . unLoc $ fId
decl2Export _ = Nothing

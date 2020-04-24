module Reduce.Passes.RemoveUnused.Exports (reduce) where

import Data.Foldable
import Control.Monad.State.Strict
import Data.Maybe
import Path
import Util.Types
import Util.Util
import Control.Monad.Reader
import qualified Data.Text as T
import "ghc" GHC

reduce :: R ()
reduce = do
  oldState <- get
  liftIO $ putStrLn "\n***Removing Exports***"
  liftIO $ debugPrint $ "Size of old state: " ++ (show . T.length . T.pack . showGhc . _parsed $ oldState)

  let L l oldModule = _parsed oldState
      maybeModName  = hsmodName oldModule
      allDecls      = hsmodDecls . unLoc . _parsed $ oldState
      maybeExports  = hsmodExports oldModule

  case maybeExports of
    Nothing -> do
      modName <- takeWhile (/= '.') . fromAbsFile . _sourceFile <$> ask

      let oldExports = map fromJust . filter isJust . map (decl2Export . unLoc) $ allDecls
          newModName =
            case maybeModName of
              Nothing -> Just . L noSrcSpan . mkModuleName $ modName
              m -> m
          newModule = oldModule { hsmodExports = Just $ L noSrcSpan oldExports, hsmodName = newModName }
          newState = oldState { _parsed = L l newModule }

      put newState
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

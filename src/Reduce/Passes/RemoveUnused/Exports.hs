module Reduce.Passes.RemoveUnused.Exports where

import Data.Foldable
import Control.Monad.State.Strict
import Ormolu.Parser.Result as OPR (ParseResult, prParsedSource)
import Ormolu.Printer (printModule)
import qualified Data.Text as T
import Data.Maybe
import "ghc-lib-parser" Module
import "ghc-lib-parser" SrcLoc
import "ghc-lib-parser" HsSyn
import System.FilePath.Posix
import Util.Types
import Util.Util
import Control.Monad.Reader

reduce :: OPR.ParseResult -> R OPR.ParseResult
reduce oldOrmolu = do
  liftIO $ putStrLn "\n***Removing Exports***"
  liftIO $ debugPrint $ "Size of old ormolu: " ++ (show . T.length $ printModule oldOrmolu)
  let L l oldModule = prParsedSource oldOrmolu
      maybeModName  = hsmodName oldModule
      maybeExports  = hsmodExports oldModule
      allDecls      = hsmodDecls . unLoc . prParsedSource $ oldOrmolu
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
          newOrmolu = oldOrmolu { prParsedSource = L l newModule }
      modify $ \s -> s { _ormolu = newOrmolu }
      liftIO $ putStrLn $ concatMap ((++ " ") . lshow) oldExports
      -- TODO: if no exports were removed, turn it into Nothing again
      traverse_ removeUnusedExport oldExports
      gets _ormolu
    Just (L _ oldExports) -> do
          traverse_ removeUnusedExport oldExports
          gets _ormolu

removeUnusedExport :: LIE GhcPs -> R ()
removeUnusedExport (L _ export) =
  changeExports (filter ((/= oshow export) . oshow . unLoc)) . _ormolu <$> get
  >>= testAndUpdateState

decl2Export :: HsDecl GhcPs -> Maybe (LIE GhcPs)
decl2Export (ValD _ (FunBind _ fId _ _ _))                     = Just . L noSrcSpan . IEVar NoExt      . L noSrcSpan . IEName . L noSrcSpan . unLoc $ fId
decl2Export (TyClD _ (DataDecl _ dId _ _ _))                   = Just . L noSrcSpan . IEThingAll NoExt . L noSrcSpan . IEName . L noSrcSpan . unLoc $ dId
decl2Export (TyClD _ (ClassDecl _ _ cId _ _ _ _ _ _ _ _))      = Just . L noSrcSpan . IEThingAll NoExt . L noSrcSpan . IEName . L noSrcSpan . unLoc $ cId
decl2Export (TyClD _ (SynDecl _ sId _ _ _))                    = Just . L noSrcSpan . IEThingAll NoExt . L noSrcSpan . IEName . L noSrcSpan . unLoc $ sId
decl2Export (TyClD _ (FamDecl _ (FamilyDecl _ _ fId _ _ _ _))) = Just . L noSrcSpan . IEThingAll NoExt . L noSrcSpan . IEName . L noSrcSpan . unLoc $ fId
decl2Export _ = Nothing
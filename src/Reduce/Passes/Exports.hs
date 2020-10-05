module Reduce.Passes.Exports (reduce) where

import Control.Concurrent.STM.Lifted
    ( atomically,
      readTVar,
      writeTVar,
    )
import Control.Monad.Reader (MonadReader (ask), asks)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import GHC
    ( GenLocated (L),
      GhcPs,
      HsBindLR (FunBind),
      HsDecl (TyClD, ValD),
      HsModule (hsmodDecls, hsmodExports, hsmodName),
      IE (IEThingAbs, IEThingAll, IEVar),
      IEWrappedName (IEName),
      LIE,
      NoExt (NoExt),
      isSynDecl,
      mkModuleName,
      noLoc,
      noSrcSpan,
      tcdName,
      unLoc,
    )
import Katip (Severity (InfoS), logTM)
import Path (fromRelFile)
import Util.Types
    ( Pass,
      R,
      RConf (_sourceFile, _tState),
      RState (_parsed),
      WaysToChange,
      showState,
    )
import Util.Util
    ( handleSubList,
      mkPass,
      oshow,
      runPass,
      updateStatistics,
    )

reduce :: R IO ()
reduce = do
    conf <- ask
    let tState = _tState conf
    oldState <- atomically $ readTVar tState

    let L l oldModule = _parsed oldState
        maybeModName = hsmodName oldModule
        allDecls = hsmodDecls . unLoc . _parsed $ oldState
        maybeExports = hsmodExports oldModule

    case maybeExports of
        Nothing -> do
            $(logTM) InfoS "making exports explicit"

            modName <- asks (takeWhile (/= '.') . fromRelFile . _sourceFile)

            let oldExports = mapMaybe (decl2Export . unLoc) allDecls
                newModName =
                    case maybeModName of
                        Nothing -> Just . L noSrcSpan . mkModuleName $ modName
                        m -> m
                newModule = oldModule {hsmodExports = Just $ L noSrcSpan oldExports, hsmodName = newModName}
                newState = oldState {_parsed = L l newModule}

            atomically $ writeTVar tState newState
            updateStatistics conf "mkExportsExplicit" 1 (T.length (showState newState) - T.length (showState oldState))
        -- TODO: if no exports were removed, turn it into Nothing again

        Just _ -> return ()

    runPass removeExports

removeExports :: Pass
removeExports = mkPass "rmvExports" f
    where
        f :: WaysToChange [LIE GhcPs]
        f = handleSubList (\e -> filter ((/= e) . oshow)) (map oshow)

-- | turn decl into a fitting export, somehow type synonyms and
decl2Export :: HsDecl GhcPs -> Maybe (LIE GhcPs)
decl2Export (ValD _ (FunBind _ fId _ _ _)) = Just . noLoc . IEVar NoExt . L noSrcSpan . IEName . L noSrcSpan . unLoc $ fId
decl2Export (TyClD _ t)
    | isSynDecl t = Just . noLoc $ IEThingAbs NoExt (noLoc . IEName . noLoc . tcdName $ t)
    | otherwise = Just . noLoc . IEThingAll NoExt . noLoc . IEName . noLoc . tcdName $ t
decl2Export _ = Nothing
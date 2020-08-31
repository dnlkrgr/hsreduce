module Reduce.Passes.Remove.Exports (reduce) where

import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Maybe
import qualified Data.Text as T
import GHC
import Path
import Util.Types
import Util.Util

reduce :: R ()
reduce = do
    conf <- ask
    let tState = _tState conf
    oldState <- liftIO . atomically $ readTVar tState

    let L l oldModule = _parsed oldState
        maybeModName = hsmodName oldModule
        allDecls = hsmodDecls . unLoc . _parsed $ oldState
        maybeExports = hsmodExports oldModule

    case maybeExports of
        Nothing -> do
            liftIO $ putStrLn "\n\n***making exports explicit***"

            modName <- asks (takeWhile (/= '.') . fromRelFile . _sourceFile)

            let oldExports = mapMaybe (decl2Export . unLoc) allDecls
                newModName =
                    case maybeModName of
                        Nothing -> Just . L noSrcSpan . mkModuleName $ modName
                        m -> m
                newModule = oldModule {hsmodExports = Just $ L noSrcSpan oldExports, hsmodName = newModName}
                newState = oldState {_parsed = L l newModule}

            liftIO . atomically $ writeTVar tState newState
            liftIO $ updateStatistics conf "mkExportsExplicit" 1 (T.length (showState newState) - T.length (showState oldState))
        -- TODO: if no exports were removed, turn it into Nothing again

        Just _ -> return ()

    runPass "rmvExports" removeExports

removeExports :: WaysToChange [LIE GhcPs]
removeExports = handleSubList (\e -> filter ((/= e) . oshow)) (map oshow)

-- | turn decl into a fitting export, somehow type synonyms and
decl2Export :: HsDecl GhcPs -> Maybe (LIE GhcPs)
decl2Export (ValD _ (FunBind _ fId _ _ _)) = Just . noLoc . IEVar NoExt . L noSrcSpan . IEName . L noSrcSpan . unLoc $ fId
decl2Export (TyClD _ t)
    | isSynDecl t = Just . noLoc $ IEThingAbs NoExt (noLoc . IEName . noLoc . tcdName $ t)
    | otherwise = Just . noLoc . IEThingAll NoExt . noLoc . IEName . noLoc . tcdName $ t
decl2Export _ = Nothing

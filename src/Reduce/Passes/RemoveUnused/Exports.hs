module Reduce.Passes.RemoveUnused.Exports (reduce) where

import Control.Concurrent.STM
import Control.Monad.State.Strict
import Data.Maybe
import Path
import Util.Types
import Util.Util
import Control.Monad.Reader
import GHC


reduce :: R ()
reduce = do
    printInfo "Removing Exports"
    tState <- asks _tState
    oldState <- liftIO . atomically $ readTVar tState
  

    let 
        L l oldModule = _parsed oldState
        maybeModName  = hsmodName oldModule
        allDecls      = hsmodDecls . unLoc . _parsed $ oldState
        maybeExports  = hsmodExports oldModule
  
    case maybeExports of
        Nothing -> do
            liftIO $ putStrLn "making exports explicit"
            isTestStillFresh "Exports.reduce_1"

            modName <- asks (takeWhile (/= '.') . fromRelFile . _sourceFile)
  
            let oldExports = mapMaybe (decl2Export . unLoc) allDecls
                newModName =
                    case maybeModName of
                        Nothing -> Just . L noSrcSpan . mkModuleName $ modName
                        m -> m
                newModule = oldModule { hsmodExports = Just $ L noSrcSpan oldExports, hsmodName = newModName }
                newState  = oldState { _parsed = L l newModule }

            liftIO . atomically $ writeTVar tState newState
            -- put newState
            -- TODO: if no exports were removed, turn it into Nothing again
  
            isTestStillFresh "Exports.reduce_2"

        Just _ -> return ()
  
    runPass "removeExports" removeExports 


removeExports :: WaysToChange [LIE GhcPs]
removeExports = handleSubList (\e -> filter ((/= e) . oshow)) (map oshow) 


-- | turn decl into a fitting export, somehow type synonyms and 
decl2Export :: HsDecl GhcPs -> Maybe (LIE GhcPs)
decl2Export (ValD _ (FunBind _ fId _ _ _)) = Just . noLoc . IEVar NoExt      . L noSrcSpan . IEName . L noSrcSpan . unLoc $ fId
decl2Export (TyClD _ t)
    | isSynDecl t = Just . noLoc $ IEThingAbs NoExt (noLoc . IEName . noLoc . tcdName $ t) 
    | otherwise   = Just . noLoc . IEThingAll NoExt . noLoc . IEName . noLoc . tcdName $ t
decl2Export _ = Nothing

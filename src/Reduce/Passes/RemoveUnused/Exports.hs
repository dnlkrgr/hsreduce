module Reduce.Passes.RemoveUnused.Exports (reduce) where

import Control.Monad.State.Strict
import Data.Maybe
import Path
import Util.Types
import Util.Util
import Control.Monad.Reader
import qualified Data.Text as T
import GHC

reduce :: R ()
reduce = do
    oldState <- get
    liftIO $ putStrLn "\n***Removing Exports***"
    liftIO $ putStrLn $ "Size of old state: " ++ (show . T.length . showState $ oldState)
  
    let L l oldModule = _parsed oldState
        maybeModName  = hsmodName oldModule
        allDecls      = hsmodDecls . unLoc . _parsed $ oldState
        maybeExports  = hsmodExports oldModule
  
    case maybeExports of
  
      Nothing -> do
          modName <- asks (takeWhile (/= '.') . fromRelFile . _sourceFile)
  
          let oldExports = mapMaybe (decl2Export . unLoc) allDecls
              newModName =
                  case maybeModName of
                      Nothing -> Just . L noSrcSpan . mkModuleName $ modName
                      m -> m
              newModule = oldModule { hsmodExports = Just $ L noSrcSpan oldExports, hsmodName = newModName }
              newState  = oldState { _parsed = L l newModule }
  
          put newState
          -- TODO: if no exports were removed, turn it into Nothing again
  
      Just _ -> return ()
  
    void . runPass removeExports . _parsed =<< get

removeExports :: WaysToChange [LIE GhcPs]
removeExports = h (map getLoc) (\loc -> filter ((/= loc) . getLoc))

-- rmvImports = h (map getLoc . hsmodImports) f
--   where f loc m = m { hsmodImports = filter ((/= loc) . getLoc) (hsmodImports m) }


decl2Export :: HsDecl GhcPs -> Maybe (LIE GhcPs)
decl2Export (ValD _ (FunBind _ fId _ _ _)) = Just . L noSrcSpan . IEVar NoExt      . L noSrcSpan . IEName . L noSrcSpan . unLoc $ fId
decl2Export (TyClD _ t)
    | isSynDecl t = Just . noLoc $ IEThingWith NoExt (noLoc . IEType . noLoc . tcdName $ t) NoIEWildcard [] []
    | otherwise   = Just . noLoc . IEThingAll NoExt . noLoc . IEName . noLoc . tcdName $ t
decl2Export _ = Nothing

module Reduce.Passes.Exports (reduce) where


import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Concurrent.STM.Lifted (
    atomically,
    readTVarIO,
    writeTVar,
 )
import Control.Monad
import Control.Monad.Reader (MonadReader (ask), asks)
import Data.Maybe
import Debug.Trace
import GHC hiding (Parsed, Pass)
import Katip (Severity (InfoS), logTM)
import Path (fromRelFile)
import Util.Types
import Util.Util

reduce :: R IO ()
reduce = do
    conf <- ask
    let tState = _tState conf
    oldState <- readTVarIO tState

    let L l oldModule@HsModule{hsmodName = maybeModName, hsmodDecls = allDecls, hsmodExports = maybeExports} = _parsed oldState

    -- when (isNothing maybeExports) $ do
    --     $(logTM) InfoS "making exports explicit"
    --     isTestStillFresh "make exports explicit"

    --     modName <- asks (takeWhile (/= '.') . fromRelFile . _testCase)

    --     let oldExports = mapMaybe (decl2Export . unLoc) allDecls
    --         newModName =
    --             case maybeModName of
    --                 Nothing -> Just . L noSrcSpan . mkModuleName $ modName
    --                 m -> m
    --         newState =
    --             oldState
    --                 { _parsed =
    --                     L l $
    --                         oldModule
    --                             { hsmodExports = Just $ L noSrcSpan $ traceShow (oshow oldExports) oldExports
    --                             , hsmodName = newModName
    --                             }
    --                 }
    --         sizeDiff = getASTLengthDiff newState oldState
    --         tokenDiff = getTokenDiff newState oldState
    --         nameDiff = getNameDiff newState oldState

    --     _ <- liftIO . traceIO $ "oldExports: " <> concatMap oshow oldExports

    --     atomically $ writeTVar tState newState
    --     updateStatistics conf "mkExportsExplicit" 1 sizeDiff tokenDiff nameDiff
    -- -- TODO: if no exports were removed, turn it into Nothing again

    when (isJust maybeExports) $ runPass removeExports

removeExports :: Pass
removeExports = mkPass "rmvExports" f
  where
    f :: WaysToChange [LIE GhcPs]
    f = handleSubList (\e -> filter ((/= e) . oshow)) (map oshow)

-- | turn decl into a fitting export, somehow type synonyms and
decl2Export :: HsDecl GhcPs -> Maybe (LIE GhcPs)
decl2Export (ValD _ (FunBind _ fId _ _ _)) = Just . noLoc . IEVar NoExtField . L noSrcSpan . IEName . L noSrcSpan . unLoc $ fId
decl2Export (TyClD _ t)
    | isSynDecl t = Just . noLoc $ IEThingAbs NoExtField (noLoc . IEName . noLoc . tcdName $ t)
    | otherwise = Just . noLoc . IEThingAll NoExtField . noLoc . IEName . noLoc . tcdName $ t
decl2Export _ = Nothing
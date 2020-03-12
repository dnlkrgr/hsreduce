module Passes.Stubbing
  ( reduce,
  )
where

import Control.Monad.State.Strict
import qualified Data.Text as T
import HsSyn
import Ormolu.Config (defaultConfig)
import Ormolu.Parser (parseModule)
import Ormolu.Parser.Result as OPR (ParseResult, prParsedSource)
import Ormolu.Printer (printModule)
import SrcLoc
import Types
import Util

-- | run a pass on the old module and return the new one if it's interesting
reduce :: FilePath -> FilePath -> OPR.ParseResult -> IO OPR.ParseResult
reduce test sourceFile oldOrmolu = do
  putStrLn "\n***Stubbing expressions***"
  debugPrint $ "Size of old ormolu: " ++ (show . T.length $ printModule oldOrmolu)
  let oldModule = prParsedSource oldOrmolu
  getUndefined
    >>= \case
      Nothing -> return oldOrmolu
      Just myUndefined ->
        _ormolu
          <$> execStateT
            (everywhereM (mkM $ expr2Undefined myUndefined) oldModule)
            (ReduceState test sourceFile oldOrmolu)

-- | change an expression to `undefined`
expr2Undefined :: HsExpr GhcPs -> LHsExpr GhcPs -> StateT ReduceState IO (LHsExpr GhcPs)
expr2Undefined myUndefined oldExpr@(L l2 _) = do
  oldOrmolu <- _ormolu <$> get
  let oldModule = prParsedSource oldOrmolu
      newExpr = L l2 myUndefined
      newModule = everywhereT (mkT (overwriteAtLoc l2 newExpr)) oldModule
      newOrmolu = oldOrmolu {prParsedSource = newModule}
  testAndUpdateStateFlex newOrmolu oldExpr newExpr

-- | getting undefined as an expression
getUndefined :: IO (Maybe (HsExpr GhcPs))
getUndefined =
  snd <$> parseModule defaultConfig "" "x = undefined"
    >>= \case
      Left _ -> return Nothing
      Right oldOrmolu ->
        case unLoc . head . hsmodDecls . unLoc $ prParsedSource oldOrmolu of
          FunBindGRHS grhs -> do
            let GRHS _ _ (L _ body) = unLoc . head $ grhssGRHSs grhs
            return $ Just body
          _ -> return Nothing

pattern FunBindGRHS :: forall p. GRHSs p (LHsExpr p) -> HsDecl p
pattern FunBindGRHS grhs <- (ValD _ (FunBind _ _ (MG _ (L _ [L _ (Match _ _ _ grhs)]) _) _ _))

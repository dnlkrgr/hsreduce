{-# LANGUAGE LambdaCase #-}

module Passes.Stubbing
  ( reduce,
  )
where

import Control.Monad.State.Strict
import HsSyn
import SrcLoc
import Ormolu.Config (defaultConfig)
import Ormolu.Parser (parseModule)
import Ormolu.Parser.Result as OPR (ParseResult, prParsedSource)
import Ormolu.Printer (printModule)
import Types
import Util
import Data.Data

-- TODO: add handling for let and where
-- | run a pass on the old module and return the new one if it's interesting
reduce :: FilePath -> FilePath -> OPR.ParseResult -> IO OPR.ParseResult
reduce test sourceFile oldOrmolu = do
  putStrLn "\n***Performing Stubbing***"
  let oldModule = prParsedSource oldOrmolu
  getUndefined >>=
    \case
      Nothing -> return oldOrmolu
      Just myUndefined -> do
        IterState _ _ newOrmolu <- execStateT (everywhereM (mkM $ match2Undefined myUndefined) oldModule) (IterState test sourceFile oldOrmolu)
        -- let newOrmolu = oldOrmolu { prParsedSource = newModule }
        debugPrint $ "oldOrmolu == newOrmolu? " ++  show (printModule oldOrmolu == printModule newOrmolu)
        return newOrmolu

-- | change a match's right hand side to `undefined`
match2Undefined :: GRHSs GhcPs (LHsExpr GhcPs) -> LMatch GhcPs (LHsExpr GhcPs) -> StateT IterState IO (LMatch GhcPs (LHsExpr GhcPs))
match2Undefined myUndefined oldMatch@(L l2 (Match _ ctxt pats _)) = do
  oldState@(IterState test sourceFile oldOrmolu) <- get
  let oldModule = prParsedSource oldOrmolu
      newMatch  = L l2 (Match NoExt ctxt pats myUndefined)
      newModule = everywhereT (mkT (overwriteMatchAtLoc (l2, newMatch))) oldModule
      newOrmolu = oldOrmolu { prParsedSource = newModule }
  writeOrmolu2FileAndTest newOrmolu >>=
    \case
      Uninteresting -> return oldMatch
      Interesting -> put (oldState {_ormolu = newOrmolu}) >> return newMatch

-- | overwrite match at a certain location
overwriteMatchAtLoc :: (SrcSpan, LMatch GhcPs (LHsExpr GhcPs)) -> LMatch GhcPs (LHsExpr GhcPs) -> LMatch GhcPs (LHsExpr GhcPs)
overwriteMatchAtLoc (loc, newMatch) oldMatch@(L oldLoc _)
  | loc == oldLoc = newMatch
  | otherwise = oldMatch

-- TODO: build up the right side of undefined manually
-- | getting undefined as a guarded right hand side;
-- it's a function binding with only one match
getUndefined :: IO (Maybe (GRHSs GhcPs (LHsExpr GhcPs)))
getUndefined =
  snd <$> parseModule defaultConfig "" "x = undefined"
    >>= \case
      Left _ -> return Nothing
      Right oldOrmolu ->
        case unLoc . head . hsmodDecls . unLoc $ prParsedSource oldOrmolu of
          (ValD _ (FunBind _ _ (MG _ (L _ [L _ (Match _ _ _ grhs)]) _) _ _)) -> return $ Just grhs
          _ -> return Nothing
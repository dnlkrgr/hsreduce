module Reduce.Passes.Functions where

import Lens.Micro.Platform
import Control.Concurrent.STM
import GHC
import Control.Monad.State
import Control.Monad.Reader
import BasicTypes
import Data.Generics.Uniplate.Data

import Util.Util
import Util.Types

reduce :: R ()
reduce = do
    printInfo "inlineFunctions"

    ast <- fmap _parsed . liftIO . readTVarIO =<< asks _tState

    forM_ 
        [ (funId, lmatches) | (FunBind _ (L _ funId) (MG _ lmatches _) _ _ :: HsBindLR GhcPs GhcPs) <- universeBi ast] 
        inlineFunction


inlineFunction :: (RdrName,  Located [LMatch GhcPs (LHsExpr GhcPs)]) -> R ()
inlineFunction (funName, lmatches) = do
  conf <- ask
  oldState <- liftIO . atomically $ readTVar (_tState conf)

  let 
      oldAST = _parsed oldState
      newAST = transformBi (inlineFunctionHelper funName lmatches) oldAST
      newState = oldState 
          & parsed  .~ newAST 
          & isAlive .~ False -- (oldState ^. isAlive || oshow oldAST /= oshow newAST)
      

  if length (oshow newAST) <= length (oshow oldAST)
      then 
          liftIO (tryNewValue conf newState) >>= \case
              True  -> liftIO . atomically $ do
                  writeTVar (_tState conf) newState
 
                  updateStatistics_ conf "inlineFunctions" True 0

              False -> liftIO $ updateStatistics conf "inlineFunctions" False 0
      else
          return ()

inlineFunctionHelper :: RdrName -> Located [LMatch GhcPs (LHsExpr GhcPs)] -> HsExpr GhcPs -> HsExpr GhcPs
inlineFunctionHelper funName (L l2 lmatches) old@(HsApp _ (L l1 (HsVar _ (L _ n))) expr)
    | funName == n  = new
    | otherwise     = old

  where
    nPats = length . m_pats . unLoc . head $ lmatches
    nMatches = length lmatches

            -- this is obviously not the best we can do
            -- but I don't know how to handle n matches with m patterns yet
    app = \con ctxt f -> HsApp NoExt (L l1 (HsPar NoExt (noLoc (con NoExt $ MG NoExt (L l2 $ map (changeMatchContext ctxt) (f lmatches)) FromSource)))) expr
    new = case (nMatches, nPats) of
            (1, 0) -> old -- eta reduced function, how to handle multiple guards?
            (1, _) -> app HsLam LambdaExpr (take 1)
            (_, 1) -> app HsLamCase CaseAlt id
            _      -> old
inlineFunctionHelper _ _ old = old

changeMatchContext :: HsMatchContext RdrName -> LMatch GhcPs (LHsExpr GhcPs) -> LMatch GhcPs (LHsExpr GhcPs)
changeMatchContext ctxt (L l (Match _ _ p g)) = L l $ Match NoExt ctxt p g
changeMatchContext _ m = m
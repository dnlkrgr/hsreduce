module Reduce.Passes.RemoveUnused.Parameters (reduce) where

import Control.Concurrent.STM
import Lens.Micro.Platform
import Data.Generics.Uniplate.Data
import Control.Monad.State
import Control.Monad.Reader
import GHC

import Util.Types
import Util.Util

-- this should prob. live in its own module
-- *** BEGINNING OF NEW STUFF
-- arst :: HsBindLR GhcPs GhcPs
reduce :: R ()
reduce = do
    printInfo "rmvUnusedParams"

    conf <- ask
    ast <- _parsed <$> (liftIO . atomically $ readTVar (_tState conf) )

    forM_ [ (funId, funMG) | (FunBind _ (L _ funId) funMG _ _ :: HsBindLR GhcPs GhcPs) <- universeBi ast] rmvUnusedParams_

rmvUnusedParams_ :: (RdrName,  MatchGroup GhcPs (LHsExpr GhcPs)) -> R ()
rmvUnusedParams_ (funId, funMG@(MG _ (L matchesLoc _) _)) = do
    case matchgroup2WildPatPositions funMG of
        Nothing        -> return ()
        Just (n, is)   -> do
            conf     <- ask
            oldState <- liftIO . atomically . readTVar $ _tState conf
            let oldAST = oldState ^. parsed

            let 
                proposedChanges :: [ParsedSource -> ParsedSource]
                proposedChanges = 
                       [ transformBi $ overwriteAtLoc l (rmvWildPatExpr n (reverse is)) 
                       | L l e <- universeBi oldAST, exprContainsFunId funId e ]
                    <> [ transformBi $ overwriteAtLoc l (rmvWildPatTypes is) 
                       | (L _ (SigD _ s@(TypeSig _ _ (HsWC _ (HsIB _ (L l (HsFunTy _ _ (L _ _))))))) :: LHsDecl GhcPs) <- universeBi oldAST
                       , sigContainsFunId funId s ]
                    <> [ transformBi (overwriteAtLoc matchesLoc rmvWildPatMatches)]

                newAST = foldr ($) oldAST proposedChanges

            let 
                sizeDiff    = length (lshow oldAST) - length (lshow newAST)
                newState    = oldState & parsed .~ newAST & isAlive .~ (oldState ^. isAlive || oshow oldAST /= oshow newAST)
 
            liftIO (tryNewValue conf newState) >>= \case
                True  -> liftIO . atomically $ do
                    writeTVar (_tState conf) newState
 
                    updateStatistics_ conf "rmvUnusedParams" True sizeDiff

                False -> liftIO $ updateStatistics conf "rmvUnusedParams" False 0
 
rmvUnusedParams_ _ = return ()

-- simplifyTySigs
rmvWildPatTypes :: [Int] -> HsType GhcPs -> HsType GhcPs
rmvWildPatTypes [] t    = t
rmvWildPatTypes (1:is) (HsFunTy _ _ (L _ t))    = rmvWildPatTypes (map (\n -> n - 1) is) t
rmvWildPatTypes is (HsFunTy x a lt)       = HsFunTy x a (rmvWildPatTypes (map (\n -> n - 1) is) <$> lt)
rmvWildPatTypes _ t     = t

-- simplifyFunctionCalls
-- here the pat indexes need to be reversed
-- we also need the total number of pats
rmvWildPatExpr :: Int -> [Int] -> HsExpr GhcPs -> HsExpr GhcPs
rmvWildPatExpr _ [] e                           = e
rmvWildPatExpr n (i:is) (HsApp x la@(L _ a) b)
    | n == i    = rmvWildPatExpr (n-1) is a
    | otherwise = HsApp x (rmvWildPatExpr (n-1) is <$> la) b
rmvWildPatExpr _ _ e                           = e

rmvWildPatMatches :: [LMatch GhcPs (LHsExpr GhcPs)] -> [LMatch GhcPs (LHsExpr GhcPs)]
rmvWildPatMatches mg = [ L l (Match NoExt ctxt (filter (p . unLoc) pats) grhss) | L l (Match _ ctxt pats grhss) <- mg ]
  where
    p (WildPat _) = False
    p _ = True

matchgroup2WildPatPositions :: MatchGroup  GhcPs (LHsExpr GhcPs) -> Maybe (Int, [Int])
matchgroup2WildPatPositions mg 
    | not (null pats) && all (== head pats) pats    = Just $ head pats
    | otherwise                                     = Nothing
  where
    pats = map (match2WildPatPositions . unLoc) . unLoc $ mg_alts mg

match2WildPatPositions :: Match GhcPs (LHsExpr GhcPs) -> (Int, [Int])
match2WildPatPositions m = (length pats, map fst . filter (p . unLoc . snd) $ zip [1..] pats)
  where 
    pats = m_pats m
    p (WildPat _)   = True
    p _             = False

sigContainsFunId :: RdrName -> Sig GhcPs -> Bool
sigContainsFunId n (TypeSig _ ids _)    = n `elem` (map unLoc ids)
sigContainsFunId _ _                    = False

exprContainsFunId :: RdrName -> HsExpr GhcPs -> Bool
exprContainsFunId n (HsApp _ (L _ (HsVar _ (L _ a))) _) = n == a
exprContainsFunId n (HsApp _ (L _ e) _)                 = exprContainsFunId n e
exprContainsFunId _ _                                   = False




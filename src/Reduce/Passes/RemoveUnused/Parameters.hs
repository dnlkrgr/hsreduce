module Passes.RemoveUnused.Parameters (reduce) where

import Control.Concurrent.STM
import Lens.Micro.Platform
import Data.Generics.Uniplate.Data
import Control.Monad.State
import Control.Monad.Reader
import GHC

import Types
import Util

passId :: String
passId = "rmvUnusedParams"

reduce :: R ()
reduce = do
    printInfo passId

    ast <- fmap _parsed . liftIO . readTVarIO =<< asks _tState

    forM_ [ (funId, funMG) | (FunBind _ (L _ funId) funMG _ _ :: HsBindLR GhcPs GhcPs) <- universeBi ast] rmvUnusedParams_


rmvUnusedParams_ :: (RdrName,  MatchGroup GhcPs (LHsExpr GhcPs)) -> R ()
rmvUnusedParams_ (funId, funMG@(MG _ (L matchesLoc _) _)) =
    case matchgroup2WildPatPositions funMG of
        Nothing        -> return ()
        Just (n, is)   -> do
            conf     <- ask

            liftIO $ atomically $ modifyTVar (_tState conf) $ \s -> s & numRmvdArgs .~ 0

            forM_ is $ \i -> liftIO $ tryNewState passId (\oldState ->
                let 
                    oldAST          = oldState ^. parsed
                    newI            = i - fromIntegral (oldState ^. numRmvdArgs)

                    proposedChanges :: [ParsedSource -> ParsedSource]
                    proposedChanges = 
                           [ transformBi $ overwriteAtLoc l (rmvArgsFromExpr funId n i) 
                           | L l e <- universeBi oldAST, exprContainsId funId e ]
                        <> [ transformBi $ overwriteAtLoc l (rmvWildPatTypes newI) 
                           | (L _ (SigD _ s@(TypeSig _ _ (HsWC _ (HsIB _ (L l (HsFunTy _ _ (L _ _))))))) :: LHsDecl GhcPs) <- universeBi oldAST
                           , sigContainsFunId funId s ]
                        <> [ transformBi (overwriteAtLoc matchesLoc (rmvWildPatMatch i))]

                    newAST      = foldr ($) oldAST proposedChanges
                    newState    = oldState 
                        & parsed .~ newAST 
                        & numRmvdArgs +~ 1
                in newState) conf
rmvUnusedParams_ _ = return ()

-- simplifyTySigs
rmvWildPatTypes :: Int -> HsType GhcPs -> HsType GhcPs
rmvWildPatTypes 1 (HsFunTy _ _ (L _ t)) = t
rmvWildPatTypes i (HsFunTy x a lt)      = HsFunTy x a (rmvWildPatTypes (i-1) <$> lt)
rmvWildPatTypes _ t                     = t


rmvWildPatMatch  :: Int -> [LMatch GhcPs (LHsExpr GhcPs)] -> [LMatch GhcPs (LHsExpr GhcPs)]
rmvWildPatMatch i mg = [ L l (Match NoExt ctxt (f pats) grhss) | L l (Match _ ctxt pats grhss) <- mg ]
  where
    f = map snd . filter ((== i) . fst) . zip [1..]

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
sigContainsFunId n (TypeSig _ ids _)    = n `elem` map unLoc ids
sigContainsFunId _ _                    = False
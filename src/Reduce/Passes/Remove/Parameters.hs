module Reduce.Passes.Remove.Parameters (reduce) where

import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Generics.Uniplate.Data
import Debug.Trace
import GHC
import Lens.Micro.Platform
import Util.Types
import Util.Util

passId :: String
passId = "rmvUnusedParams"

reduce :: R ()
reduce = do
    printInfo passId

    ast <- fmap _parsed . liftIO . readTVarIO =<< asks _tState

    forM_ [(funId, funMG) | (FunBind _ (L _ funId) funMG _ _ :: HsBindLR GhcPs GhcPs) <- universeBi ast] rmvUnusedParams_

rmvUnusedParams_ :: (RdrName, MatchGroup GhcPs (LHsExpr GhcPs)) -> R ()
rmvUnusedParams_ (funId, funMG@(MG _ (L matchesLoc _) _)) =
    case matchgroup2WildPatPositions funMG of
        Nothing -> return ()
        Just (n, is) -> do
            conf <- ask

            liftIO $ atomically $ modifyTVar (_tState conf) $ \s -> s & numRmvdArgs .~ 0

            forM_ is $ \i ->
                liftIO $
                    tryNewState
                        passId
                        ( \oldState ->
                              let oldAST = oldState ^. parsed
                                  -- change offset based on the number of parameters already removed
                                  newI = i - fromIntegral (oldState ^. numRmvdArgs) 

                                  newAST =
                                      foldr ($) oldAST $
                                          [ transformBi $ overwriteAtLoc l (rmvArgsFromExpr funId n i)
                                            | L l e <- universeBi oldAST,
                                              exprContainsId funId e
                                          ]
                                              <> [ transformBi $ overwriteAtLoc l (handleTypes newI)
                                                   | (L _ (SigD _ s@(TypeSig _ _ (HsWC _ (HsIB _ (L l (HsFunTy _ _ (L _ _))))))) :: LHsDecl GhcPs) <- universeBi oldAST,
                                                     sigContainsFunId funId s
                                                 ]
                                              <> [transformBi (overwriteAtLoc matchesLoc (handleMatches i))]
                                  newState =
                                      oldState
                                          & parsed .~ newAST
                                          & numRmvdArgs +~ 1
                               in newState
                        )
                        conf
rmvUnusedParams_ _ = return ()

-- simplifyTySigs
handleTypes :: Int -> HsType GhcPs -> HsType GhcPs
handleTypes 1 (HsFunTy _ _ (L _ t)) = t
handleTypes i (HsFunTy x a lt) = HsFunTy x a (handleTypes (i -1) <$> lt)
handleTypes _ t = t

handleMatches :: Int -> [LMatch GhcPs (LHsExpr GhcPs)] -> [LMatch GhcPs (LHsExpr GhcPs)]
handleMatches i mg = [L l (Match NoExt ctxt (f pats) grhss) | L l (Match _ ctxt pats grhss) <- mg]
    where
        f =
            (\pats -> traceShow ("after: " <> oshow pats) pats)
                . map snd
                . filter ((== i) . fst)
                . zip [1 ..]
                . (\pats -> traceShow ("before: " <> oshow pats) pats)

matchgroup2WildPatPositions :: MatchGroup GhcPs (LHsExpr GhcPs) -> Maybe (Int, [Int])
matchgroup2WildPatPositions mg
    | not (null pats) && all (== head pats) pats = Just $ head pats
    | otherwise = Nothing
    where
        pats = map (match2WildPatPositions . unLoc) . unLoc $ mg_alts mg

match2WildPatPositions :: Match GhcPs (LHsExpr GhcPs) -> (Int, [Int])
match2WildPatPositions m = (length pats, map fst . filter (p . unLoc . snd) $ zip [1 ..] pats)
    where
        pats = m_pats m
        p (WildPat _) = True
        p _ = False

sigContainsFunId :: RdrName -> Sig GhcPs -> Bool
sigContainsFunId n (TypeSig _ ids _) = n `elem` map unLoc ids
sigContainsFunId _ _ = False
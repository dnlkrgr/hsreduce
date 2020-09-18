module Reduce.Passes.Remove.Parameters (reduce) where

import Data.Generics.Uniplate.Data
import Debug.Trace
import GHC hiding (Pass)
import Util.Types
import Util.Util

reduce :: Pass
reduce = AST "rmvUnusedParams" $ \ast ->
    concatMap
        ( \(funId, funMG@(MG _ (L matchesLoc _) _)) ->
              case matchgroup2WildPatPositions funMG of
                  Nothing -> []
                  Just (n, is) ->
                      map
                          ( \i oldAST ->
                                let arst = getPatsLength funId oldAST
                                 in if length arst == 1
                                        then
                                            let nRmvdParams = (length is - head arst)
                                                newI = i - (traceShow ("nRmvdParams: " <> show nRmvdParams) nRmvdParams)
                                             in foldr ($) oldAST $
                                                    [ transformBi $ overwriteAtLoc l (rmvArgsFromExpr funId n i)
                                                      | L l e <- universeBi oldAST,
                                                        exprContainsId funId e
                                                    ]
                                                        <> [ transformBi $ overwriteAtLoc l (handleTypes newI)
                                                             | ( L _ (SigD _ s@(TypeSig _ _ (HsWC _ (HsIB _ (L l (HsFunTy _ _ (L _ _))))))) ::
                                                                     LHsDecl GhcPs
                                                                   ) <-
                                                                   universeBi oldAST,
                                                               sigContainsFunId funId s
                                                           ]
                                                        <> [transformBi (overwriteAtLoc matchesLoc (handleMatches i))]
                                        else oldAST
                          )
                          is
        )
        [(funId, funMG) | (FunBind _ (L _ funId) funMG _ _ :: HsBindLR GhcPs GhcPs) <- universeBi ast]

getPatsLength :: RdrName -> ParsedSource -> [Int]
getPatsLength name ast =
    [ length . m_pats . unLoc . head . unLoc $ mg_alts funMG
      | FunBind _ (L _ funId) funMG _ _ :: HsBindLR GhcPs GhcPs <- universeBi ast,
        name == funId,
        length (mg_alts funMG) == 1
    ]

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
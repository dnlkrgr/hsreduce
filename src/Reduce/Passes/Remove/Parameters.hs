module Reduce.Passes.Remove.Parameters (reduce) where

import Data.Generics.Uniplate.Data
import Debug.Trace
import GHC hiding (Pass)
import Util.Types
import Util.Util

reduce :: Pass
reduce = AST "rmvUnusedParams" $ \ast ->
    concatMap
        ( \(funId, funMG@(MG _ (L _ _) _)) ->
              case matchgroup2WildPatPositions funMG of
                  Nothing -> []
                  Just (n, is) ->
                      map
                          ( \i oldAST ->
                                let patsLengths = getPatsLength funId oldAST
                                 in if length patsLengths == 1
                                        then
                                            let nRmvdParams = (n - head patsLengths)
                                                newI = i - (traceShow ("nRmvdParams: " <> show nRmvdParams) nRmvdParams)
                                             in
                                                -- traceShow ("funId: " <> oshow funId) $
                                                -- traceShow ("length is: " <> (show $ length is)) $
                                                -- traceShow ("head patsLengths: " <> (show $ head patsLengths)) $
                                                -- traceShow ("nRmvdParams: " <> show nRmvdParams) $
                                                -- traceShow ("i: " <> show i) $
                                                -- traceShow ("newI: " <> show newI) $

                                                transformBi (rmvArgsFromExpr funId n i)
                                                   . transformBi (handleSigs funId newI)
                                                   . transformBi (handleFunBinds funId i)
                                                   $ oldAST
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
handleSigs :: RdrName -> Int -> Sig GhcPs -> Sig GhcPs
handleSigs funId i ts@(TypeSig _ [sigId] (HsWC _ (HsIB _ (L l t))))
    | funId == unLoc sigId = TypeSig NoExt [sigId] . HsWC NoExt . HsIB NoExt . L l $ handleTypes i t
    | otherwise = ts
handleSigs _ _ d = d

handleTypes :: Int -> HsType GhcPs -> HsType GhcPs
handleTypes 1 (HsFunTy _ _ (L _ t)) = t
handleTypes i (HsFunTy x a lt) = HsFunTy x a (handleTypes (i -1) <$> lt)
handleTypes _ t = t

handleFunBinds :: RdrName -> Int -> HsBind GhcPs -> HsBind GhcPs
handleFunBinds funId i (FunBind _ bindId (MG _ (L l m) o) a b)
    | funId == unLoc bindId = FunBind NoExt bindId (MG NoExt (L l (handleMatches i m)) o) a b
handleFunBinds _ _ b = b

handleMatches :: Int -> [LMatch GhcPs (LHsExpr GhcPs)] -> [LMatch GhcPs (LHsExpr GhcPs)]
handleMatches i mg = [L l (Match NoExt ctxt (f pats) grhss) | L l (Match _ ctxt pats grhss) <- mg]
    where
        f =
            map snd
            . filter ((== i) . fst)
            . zip [1 ..]

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
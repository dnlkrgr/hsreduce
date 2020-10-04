module Reduce.Passes.Remove.Parameters (reduce) where

import Data.Generics.Uniplate.Data
import GHC hiding (Pass)
import Util.Types
import Util.Util

reduce :: Pass
reduce = AST "rmvUnusedParams" $ \ast ->
    concatMap
        ( \(funId, funMG) ->
              case matchgroup2WildPatPositions funMG of
                  Nothing -> []
                  Just (oldPatsLength, indices) ->
                      map
                          ( \i oldAST ->
                                let patsLengths = getPatsLength funId oldAST
                                 in if length patsLengths >= 1
                                        then
                                            let newPatsLength = head patsLengths
                                                nRmvdParams = (oldPatsLength - newPatsLength)
                                                newI = i - nRmvdParams -- (traceShow ("nRmvdParams: " <> show nRmvdParams) nRmvdParams)
                                             in -- traceShow ("funId: " <> oshow funId)
                                                --     $ traceShow ("length indices: " <> (show $ length indices))
                                                --     $ traceShow ("oldPatsLength: " <> (show $ oldPatsLength))
                                                --     $ traceShow ("newPatsLengths: " <> (show $ newPatsLength))
                                                --     $ traceShow ("nRmvdParams: " <> show nRmvdParams)
                                                --     $ traceShow ("i: " <> show i)
                                                --     $ traceShow ("newI: " <> show newI)

                                                transformBi (rmvArgsFromExpr funId newPatsLength newI)
                                                    . transformBi (handleSigs funId newI)
                                                    . transformBi (handleFunBinds funId newI)
                                                    $ oldAST
                                        else oldAST
                          )
                          indices
        )
        [ (funId, funMG)
          | (FunBind _ (L _ funId) funMG _ _ :: HsBindLR GhcPs GhcPs) <- universeBi ast
            -- infix matches can't be handled yet, GHC panics when trying to print what hsreduce produces
            , all (not . isInfixMatch) . map unLoc . unLoc $ mg_alts funMG
        ]

getPatsLength :: RdrName -> ParsedSource -> [Int]
getPatsLength name ast =
    [ length . m_pats . unLoc $ alternatives
      | FunBind _ (L _ funId) funMG _ _ :: HsBindLR GhcPs GhcPs <- universeBi ast,
        name == funId,
        alternatives <- unLoc $ mg_alts funMG
    ]

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
                . filter ((/= i) . fst)
                . zip [1 ..]

-- get indices of wildcard patterns
matchgroup2WildPatPositions :: MatchGroup GhcPs (LHsExpr GhcPs) -> Maybe (Int, [Int])
matchgroup2WildPatPositions mg
    | not (null pats) && all (== head pats) pats = Just $ head pats
    | otherwise = Nothing
    where
        pats = map (match2WildPatPositions . unLoc) . unLoc $ mg_alts mg

-- get indices of wildcard patterns
match2WildPatPositions :: Match GhcPs (LHsExpr GhcPs) -> (Int, [Int])
match2WildPatPositions m = (length pats, map fst . filter (p . unLoc . snd) $ zip [1 ..] pats)
    where
        pats = m_pats m
        p (WildPat _) = True
        p _ = False
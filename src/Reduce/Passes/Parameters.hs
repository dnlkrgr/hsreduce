module Reduce.Passes.Parameters (rmvUnusedParams, reduce) where

import Data.Generics.Uniplate.Data (transformBi, universeBi)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import GHC hiding (Pass)
import Util.Types (Pass (AST))
import Util.Util 

reduce ::
    String ->
    (ParsedSource -> [(RdrName, [a])]) ->
    (RdrName -> ParsedSource -> [Int]) ->
    (RdrName -> [a] -> NE.NonEmpty Int -> Int -> Int -> ParsedSource -> ParsedSource) ->
    Pass
reduce passId getParams getArgsLength myTransform = AST passId $ \ast ->
    concatMap
        ( \(name, args) ->
              map
                  ( \i ->
                        let mTemp = NE.nonEmpty (getArgsLength name ast)
                         in case mTemp of
                                Just temp ->
                                    if NE.head temp >= 1
                                        then
                                            let nRmvdArgs = length args - NE.head temp
                                                newI = i - nRmvdArgs
                                             in myTransform name args temp i newI
                                        else id
                                _ -> id
                  )
                  [1 .. length args]
        )
        (getParams ast)

rmvUnusedParams :: Pass
rmvUnusedParams =
    reduce
        "rmvUnusedParams"
        ( \ast ->
              [ (funId, pats)
                | FunBind _ (L _ funId) funMG _ _ :: HsBindLR GhcPs GhcPs <- universeBi ast,
                  -- infix matches can't be handled yet, GHC panics when trying to print what hsreduce produces
                  all (not . isInfixMatch) . map unLoc . unLoc $ mg_alts funMG,
                  pats <- maybeToList $ NE.head <$> (NE.nonEmpty $ matchgroup2ListOfPats funMG)
              ]
        )
        getPatsLength
        ( \funId _ temp _ newI ->
              transformBi (rmvArgsFromExpr funId (NE.head temp) newI)
                  . transformBi (handleSigs funId newI)
                  . transformBi (handleFunBinds funId newI)
        )

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
handleSigs funId i ts@(ClassOpSig _ b [sigId] (HsIB _ (L l t)))
    | funId == unLoc sigId = ClassOpSig NoExt b [sigId] . HsIB NoExt . L l $ handleTypes i t
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
matchgroup2ListOfPats :: MatchGroup GhcPs (LHsExpr GhcPs) -> [[LPat GhcPs]]
matchgroup2ListOfPats mg = [m_pats . unLoc $ match | match <- unLoc $ mg_alts mg]
module Reduce.Passes.Functions (inline, etaReduceMatches, rmvMatches, rmvRHSs, rmvGuards, betaReduceExprs, substituteVarWithExpr) where

import Data.Generics.Uniplate.Data (transformBi, universeBi)
import Debug.Trace
import GHC hiding (Pass)
import Util.Types
import Util.Util

substituteVarWithExpr :: LHsExpr GhcPs -> RdrName -> HsExpr GhcPs -> HsExpr GhcPs
substituteVarWithExpr newExpr name e@(HsVar _ (unLoc -> varName))
    | varName == name = unLoc newExpr
    | otherwise = e
substituteVarWithExpr _ _ e = e

betaReduceExprs :: Pass
betaReduceExprs = mkPass "betaReduceExprs" f
    where
        f :: WaysToChange (HsExpr GhcPs)
        f
            ( HsApp
                  _
                  ( unLoc ->
                        HsPar
                            _
                            ( unLoc ->
                                  HsLam
                                      _
                                      ( MG
                                            _
                                            ( unLoc ->
                                                  [ unLoc ->
                                                        Match
                                                            _
                                                            _
                                                            [(unLoc -> VarPat _ (unLoc -> name))]
                                                            (GRHSs _ [unLoc -> GRHS _ [] body] _)
                                                      ]
                                                )
                                            _
                                          )
                                )
                      )
                  newExpr
                ) =
                let newBody = transformBi (substituteVarWithExpr newExpr name) body
                 in [const (HsPar NoExt newBody)]
        f
            ( HsApp
                  _
                  ( unLoc ->
                        HsPar
                            _
                            ( unLoc ->
                                  HsLam
                                      _
                                      ( MG
                                            _
                                            ( unLoc ->
                                                  [ unLoc ->
                                                        Match
                                                            _
                                                            ctxt
                                                            ((unLoc -> VarPat _ (unLoc -> name)) : otherPats)
                                                            rhs
                                                      ]
                                                )
                                            mgOrigin
                                          )
                                )
                      )
                  newExpr
                ) =
                let newRHS = transformBi (substituteVarWithExpr newExpr name) rhs
                 in [const (HsPar NoExt (noLoc $ HsLam NoExt (MG NoExt (noLoc [noLoc $ Match NoExt ctxt otherPats newRHS]) mgOrigin)))]
        f _ = []

-- only one match and rhs is just a function name
inline :: Pass
inline = AST "Functions:inline" $ \ast ->
    map
        ( \(funId, body) ->
              transformBi (substituteVarWithExpr body funId)
        )
        -- f = g
        [ (funId, body)
          | (FunBind _ (L _ funId) (MG _ (unLoc -> [unLoc -> Match _ _ [] (GRHSs _ [unLoc -> GRHS _ [] body] _)]) _) _ _) :: HsBindLR GhcPs GhcPs <- universeBi ast,
              let usages = [() | HsApp _ (unLoc -> HsVar _ (unLoc -> funName)) _ :: HsExpr GhcPs <- universeBi ast, funName == funId],
              length usages == 1
        ]
        <> map
            ( \(funId, mg) ->
                  transformBi (substituteVarWithExpr (noLoc $ HsPar NoExt $ noLoc $ HsLam NoExt mg) funId)
            )
            ( [ (funId, transformBi changeCtxt2Lambda mg)
                | (FunBind _ (L _ funId) mg@(MG _ (unLoc -> [unLoc -> Match _ _ _ (GRHSs _ [unLoc -> GRHS _ [] _] _)]) _) _ _) :: HsBindLR GhcPs GhcPs <- universeBi ast,
                  let usages = [() | HsApp _ (unLoc -> HsVar _ (unLoc -> funName)) _ :: HsExpr GhcPs <- universeBi ast, funName == funId],
                  length usages == 1
              ]
            )
    where
        changeCtxt2Lambda :: p ~ GhcPs => HsMatchContext (NameOrRdrName (IdP p)) -> HsMatchContext (NameOrRdrName (IdP p))
        changeCtxt2Lambda _ = LambdaExpr

-- function with:
-- - one match
-- - without guards
-- - with one rhs
-- - where the body is applying some function to an argument
etaReduceMatches :: Pass
etaReduceMatches = mkPass "etaReduceMatches" f
    where
        f :: p ~ GhcPs => WaysToChange [LMatch p (LHsExpr p)]
        f [L l1 m@(Match {m_pats = pats@(_ : _), m_grhss = g@(GRHSs {grhssGRHSs = [L l2 (GRHS _ guards (L l3 (HsApp _ lExpr rExpr)))]})})]
            | let sPat = oshow (last pats),
              sPat == "_" || sPat == oshow rExpr =
                [const [L l1 (m {m_pats = init pats, m_grhss = g {grhssGRHSs = [L l2 (GRHS NoExt guards (L l3 (unLoc lExpr)))]}})]]
            | otherwise = []
        f _ = []

-- ***************************************************************************

-- MATCHES

-- ***************************************************************************

rmvRHSs :: Pass
rmvRHSs = mkPass "rmvRHSs" f
    where
        f :: WaysToChange (Match GhcPs (LHsExpr GhcPs))
        f (Match _ _ _ (GRHSs _ [] _)) = []
        f mm = handleSubList g p mm
            where
                p = \case
                    -- reverse because the lower have to be tried first
                    MatchP iterGRHSs _ -> map getLoc . reverse $ iterGRHSs
                    _ -> []
                g grhsLoc = \case
                    m@(Match _ _ _ (GRHSs _ grhss lb)) ->
                        let newGRHSs = filter ((/= grhsLoc) . getLoc) grhss
                         in case newGRHSs of
                                [] -> m
                                _ -> m {m_grhss = GRHSs NoExt newGRHSs lb}
                    m -> m

rmvMatches :: Pass
rmvMatches = mkPass "rmvMatches" f
    where
        f :: WaysToChange [LMatch GhcPs (LHsExpr GhcPs)]
        f = handleSubList (\loc -> filter ((/= loc) . getLoc)) (map getLoc)

-- <> [ filter (\(L _ (Match _ _ _ grhss@GRHSs{})) -> showSDocUnsafe (pprGRHSs LambdaExpr grhss) /= "-> undefined")
-- ,  filter (\(L _ (Match _ _ _ (GRHSs _ grhs _))) -> not (all ( ("undefined" `isSubsequenceOf`) . showSDocUnsafe . pprGRHS LambdaExpr . unLoc) grhs))]

rmvGuards :: Pass
rmvGuards = mkPass "rmvGuards" f
    where
        f :: WaysToChange (GRHS GhcPs (LHsExpr GhcPs))
        f (GRHS _ [] _) = []
        f g@(GRHS _ _ body) = [const (GRHS NoExt [] body)] <> handleSubList h p g
            where
                p (GRHS _ stmts _) = map getLoc stmts
                p _ = []
                h loc (GRHS _ s b) = GRHS NoExt (filter ((/= loc) . getLoc) s) b
                h _ _ = g
        f _ = []

-- ***************************************************************************

-- PATTERN SYNONYMS

-- ***************************************************************************

pattern MatchP :: [LGRHS GhcPs (LHsExpr GhcPs)] -> LHsLocalBinds GhcPs -> Match GhcPs (LHsExpr GhcPs)
pattern MatchP grhss binds <- Match _ _ _ (GRHSs _ grhss binds)
module Reduce.Passes.Simplify.Expr where

import GHC
import OccName (mkOccName, varName)
import Util.Types
import Util.Util

expr2Undefined :: WaysToChange (HsExpr GhcPs)
expr2Undefined e
    | oshow e /= "undefined" = [const (HsVar NoExt . noLoc . Unqual . mkOccName varName $ "undefined")]
    | otherwise = []

filterExprSubList :: WaysToChange (HsExpr GhcPs)
filterExprSubList e@RecordUpd {} = handleSubList fExpr pExpr e
filterExprSubList e@RecordCon {} = handleSubList fExpr pExpr e
filterExprSubList e@ExplicitTuple {} = handleSubList fExpr pExpr e
filterExprSubList e@HsCase {} = handleSubList fExpr pExpr e
filterExprSubList e@HsMultiIf {} = handleSubList fExpr pExpr e
-- filterExprSubList e@HsDo{}                           = handleSubList fExpr pExpr e       <-- this creates unprintable expressions
filterExprSubList e@ExplicitList {} = handleSubList fExpr pExpr e
filterExprSubList _ = []

pExpr :: HsExpr p -> [SrcSpan]
pExpr = \case
    (RecordUpd _ _ fields) -> map getLoc fields
    (RecordCon _ _ fields) -> map getLoc . rec_flds $ fields
    (ExplicitTuple _ args _) -> map getLoc args
    (HsCase _ _ mg) -> map getLoc . unLoc . mg_alts $ mg
    (HsMultiIf _ es) -> map getLoc es
    (HsDo _ _ (L _ stmts)) -> map getLoc stmts
    (ExplicitList _ _ es) -> map getLoc es
    (HsArrForm _ _ _ cmds) -> map getLoc cmds
    _ -> []

fExpr :: SrcSpan -> HsExpr GhcPs -> HsExpr GhcPs
fExpr loc = \case
    (RecordUpd _ e fields) -> RecordUpd NoExt e $ filter ((/= loc) . getLoc) fields
    (RecordCon _ n fields) -> RecordCon NoExt n $ fields {rec_flds = filter ((/= loc) . getLoc) (rec_flds fields)}
    (ExplicitTuple _ args b) -> ExplicitTuple NoExt (filter ((/= loc) . getLoc) args) b
    (HsCase _ e mg) -> HsCase NoExt e $ mg {mg_alts = fmap (filter ((/= loc) . getLoc)) (mg_alts mg)}
    (HsMultiIf _ es) -> HsMultiIf NoExt $ filter ((/= loc) . getLoc) es
    (HsDo _ ctxt (L l stmts)) -> HsDo NoExt ctxt $ L l $ filter ((/= loc) . getLoc) stmts
    (ExplicitList _ se es) -> ExplicitList NoExt se $ filter ((/= loc) . getLoc) es
    (HsArrForm x e f cmds) -> HsArrForm x e f (filter ((/= loc) . getLoc) cmds)
    e -> e

simplifyExpr :: WaysToChange (HsExpr GhcPs)
simplifyExpr (SingleCase body) = map const [body]
simplifyExpr (HsIf _ _ _ (L _ ls) (L _ rs)) = map const [ls, rs]
simplifyExpr (HsApp _ (L _ l) (L _ r)) = map const [l, r]
simplifyExpr (HsAppType _ (L _ e) _) = [const e]
simplifyExpr (OpApp _ _ (L _ l) (L _ r)) = map const [l, r]
simplifyExpr (HsLet _ _ (L _ e)) = [const e]
simplifyExpr (ExprWithTySig _ (L _ e) _) = [const e]
simplifyExpr (HsStatic _ (L _ e)) = [const e]
simplifyExpr (HsArrApp _ (L _ l) (L _ r) _ _) = map const [l, r]
simplifyExpr (HsTick _ _ (L _ e)) = [const e]
simplifyExpr (HsBinTick _ _ _ (L _ e)) = [const e]
simplifyExpr (EAsPat _ _ (L _ e)) = [const e]
simplifyExpr (EViewPat _ _ (L _ e)) = [const e]
simplifyExpr (ELazyPat _ (L _ e)) = [const e]
simplifyExpr (HsWrap _ _ e) = [const e]
simplifyExpr _ = []


pattern SingleCase :: HsExpr GhcPs -> HsExpr GhcPs
pattern SingleCase body <-
    HsCase
        _
        _
        ( MG
              _
              ( L
                    _
                    [ L
                          _
                          ( Match
                                _
                                _
                                _
                                ( GRHSs
                                      _
                                      [ L
                                            _
                                            ( GRHS
                                                  _
                                                  []
                                                  (L _ body)
                                                )
                                          ]
                                      (L _ (EmptyLocalBinds _))
                                    )
                              )
                        ]
                  )
              _
            )
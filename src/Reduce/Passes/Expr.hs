module Reduce.Passes.Expr where

import GHC
    ( GRHS (GRHS),
      GRHSs (GRHSs),
      GenLocated (L),
      GhcPs,
      HsExpr
          ( EAsPat,
            ELazyPat,
            ExplicitList,
            ExplicitTuple,
            ExprWithTySig,
            HsApp,
            HsAppType,
            HsArrApp,
            HsArrForm,
            HsBinTick,
            HsCase,
            HsDo,
            HsIf,
            HsLet,
            HsMultiIf,
            HsStatic,
            HsTick,
            HsVar,
            HsWrap,
            OpApp,
            RecordCon,
            RecordUpd
          ),
      HsLocalBindsLR (EmptyLocalBinds),
      HsRecFields (rec_flds),
      Match (Match),
      MatchGroup (MG, mg_alts),
      NoExt (NoExt),
      RdrName (Unqual),
      SrcSpan,
      getLoc,
      noLoc,
      unLoc,
    )
import OccName (mkOccName, varName)
import Util.Types (Pass, WaysToChange)
import Util.Util (handleSubList, mkPass, oshow)

expr2Undefined :: Pass
expr2Undefined = mkPass "expr2Undefined" f
    where
        f :: WaysToChange (HsExpr GhcPs)
        f e
            | oshow e /= "undefined" = [const (HsVar NoExt . noLoc . Unqual . mkOccName varName $ "undefined")]
            | otherwise = []

filterExprSubList :: Pass
filterExprSubList = mkPass "filterExprSubList" f
    where
        f :: WaysToChange (HsExpr GhcPs)
        f e@RecordUpd {} = handleSubList fExpr pExpr e
        f e@RecordCon {} = handleSubList fExpr pExpr e
        f e@ExplicitTuple {} = handleSubList fExpr pExpr e
        f e@HsCase {} = handleSubList fExpr pExpr e
        f e@HsMultiIf {} = handleSubList fExpr pExpr e
        -- f e@HsDo{}                           = handleSubList fExpr pExpr e       <-- this creates unprintable expressions
        f e@ExplicitList {} = handleSubList fExpr pExpr e
        f _ = []
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
            (HsArrForm x e g cmds) -> HsArrForm x e g (filter ((/= loc) . getLoc) cmds)
            e -> e

simplifyExpr :: Pass
simplifyExpr = mkPass "simplifyExpr" f
    where
        f :: WaysToChange (HsExpr GhcPs)
        f (SingleCase body) = [const body]
        f (HsIf _ _ _ (L _ ls) (L _ rs)) = map const [ls, rs]
        f (HsApp _ (L _ l) (L _ r)) = map const [l, r]
        f (HsAppType _ (L _ e) _) = [const e]
        f (OpApp _ (L _ o) (L _ l) (L _ r)) = map const [o, l, r]
        f (HsLet _ _ (L _ e)) = [const e]
        f (ExprWithTySig _ (L _ e) _) = [const e]
        f (HsStatic _ (L _ e)) = [const e]
        f (HsArrApp _ (L _ l) (L _ r) _ _) = map const [l, r]
        f (HsTick _ _ (L _ e)) = [const e]
        f (HsBinTick _ _ _ (L _ e)) = [const e]
        f (EAsPat _ _ (L _ e)) = [const e]
        f (ELazyPat _ (L _ e)) = [const e]
        f (HsWrap _ _ e) = [const e]
        f _ = []

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
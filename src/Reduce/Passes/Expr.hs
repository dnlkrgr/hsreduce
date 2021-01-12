module Reduce.Passes.Expr where

import OccName
import Data.String
import GHC hiding (Pass)
import Util.Types (Pass, WaysToChange)
import Util.Util 

expr2Undefined :: Pass
expr2Undefined = mkPass "expr2Undefined" f
    where
        f :: WaysToChange (HsExpr GhcPs)
        f e
            | oshow e /= "undefined" = [const (HsVar NoExtField . noLoc . Unqual . mkOccName varName $ "undefined")]
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
        f e@HsDo{} = handleSubList fExpr pExpr e
        f e@ExplicitList {} = handleSubList fExpr pExpr e
        f _ = []
        pExpr :: HsExpr p -> [SrcSpan]
        pExpr = \case
            (RecordUpd _ _ fields) -> map getLoc fields
            (RecordCon _ _ fields) -> map getLoc . rec_flds $ fields
            (ExplicitTuple _ args _) -> map getLoc args
            (HsCase _ _ mg) -> map getLoc . unLoc . mg_alts $ mg
            (HsMultiIf _ es) -> map getLoc es
            (HsDo _ _ (unLoc -> stmts)) -> map getLoc stmts
            (ExplicitList _ _ es) -> map getLoc es
            -- (HsArrForm _ _ _ cmds) -> map getLoc cmds
            _ -> []
        fExpr :: SrcSpan -> HsExpr GhcPs -> HsExpr GhcPs
        fExpr loc = \case
            (RecordUpd _ e fields) -> RecordUpd NoExtField e $ filter ((/= loc) . getLoc) fields
            (RecordCon _ n fields) -> RecordCon NoExtField n $ fields {rec_flds = filter ((/= loc) . getLoc) (rec_flds fields)}
            (ExplicitTuple _ args b) -> ExplicitTuple NoExtField (filter ((/= loc) . getLoc) args) b
            (HsCase _ e mg) -> HsCase NoExtField e $ mg {mg_alts = fmap (filter ((/= loc) . getLoc)) (mg_alts mg)}
            (HsMultiIf _ es) -> HsMultiIf NoExtField $ filter ((/= loc) . getLoc) es
            (HsDo _ ctxt (L l stmts)) -> HsDo NoExtField ctxt $ L l $ filter ((/= loc) . getLoc) stmts
            (ExplicitList _ se es) -> ExplicitList NoExtField se $ filter ((/= loc) . getLoc) es
            -- (HsArrForm x e g cmds) -> HsArrForm x e g (filter ((/= loc) . getLoc) cmds)
            e -> e

simplifyExpr :: Pass
simplifyExpr = mkPass "simplifyExpr" f
    where
        f :: WaysToChange (HsExpr GhcPs)
        -- (\_ -> e) x => e
        f (HsApp _ (unLoc -> HsPar _ (unLoc -> HsLam _ (MG _ (unLoc -> [unLoc -> Match _ LambdaExpr [unLoc -> WildPat _] (GRHSs _ [unLoc -> GRHS _ [] (unLoc -> e)] _)]) _))) _) = [const e]
        -- \h -> e => \h -> h
        f (HsLam _ (MG _ (unLoc -> [unLoc -> Match _ LambdaExpr pats@[unLoc -> VarPat _ patName] (GRHSs _ [unLoc -> GRHS _ [] _] arst)]) orig)) 
            = [const $ HsLam NoExtField (MG NoExtField (noLoc [noLoc $ Match NoExtField LambdaExpr pats (GRHSs NoExtField [noLoc . GRHS NoExtField [] . noLoc $ HsVar NoExtField patName] arst)]) orig)]
        f (HsLam _ _) = [const (HsVar NoExtField . noLoc . Unqual $ mkVarOcc "id")]
        f (ArithSeq _ ms _) = [const $ ExplicitList NoExtField ms []]
        f e@HsCase {} = handleCaseMulti e
        f e@(HsMultiIf _ _) = handleCaseMulti e
        f (HsDo _ ListComp _)  = [const $ ExplicitList NoExtField Nothing []]
        f (HsIf _ _ _ (L _ ls) (L _ rs)) = map const [ls, rs]
        f (HsAppType _ (L _ e) _) = [const e]
        f (HsLet _ _ (L _ e)) = [const e]
        f (ExprWithTySig _ (L _ e) _) = [const e]
        f (HsStatic _ (L _ e)) = [const e]
        -- f (HsArrApp _ (L _ l) (L _ r) _ _) = map const [l, r]
        f (HsTick _ _ (L _ e)) = [const e]
        f (HsBinTick _ _ _ (L _ e)) = [const e]
        -- f (AsPat _ _ (L _ e)) = [const e]
        -- f (LazyPat _ (L _ e)) = [const e]
        f (HsWrap _ _ e) = [const e]
        f (HsPar _ (L _ e)) = [const e]
        f (HsApp _ (L _ l) (L _ r)) = map const [l, r]
        f (OpApp _ (L _ o) (L _ l) (L _ r)) = map const [o, l, r]
        f (HsLit x l) = map (const . HsLit x) $ simplifyLit l
        f (ExplicitList _ ms es) = const (ExplicitList NoExtField ms []) : map (const . unLoc) es
        f _ = []

handleCaseMulti :: WaysToChange (HsExpr GhcPs)
handleCaseMulti expr@(HsCase _ _ MG{}) = handleSubList fCaseMulti pCaseMutli expr
handleCaseMulti expr@(HsMultiIf _ _) = handleSubList fCaseMulti pCaseMutli expr
handleCaseMulti _ = []

fCaseMulti :: SrcSpan -> HsExpr GhcPs -> HsExpr GhcPs
fCaseMulti loc e@(HsCase _ _ (MG _ (L _ lmatches) _)) = maybe e unLoc . grhs2Body . unLoc . head . grhssGRHSs . m_grhss . unLoc . head $ filter ((==loc) . getLoc) lmatches
fCaseMulti loc e@(HsMultiIf _ lgrhss) = maybe e unLoc . grhs2Body . unLoc . head $ filter ((==loc) . getLoc) lgrhss
fCaseMulti _ e = e

pCaseMutli :: HsExpr p -> [SrcSpan]
pCaseMutli (HsCase _ _ (MG _ (L _ lmatches) _)) = map getLoc lmatches
pCaseMutli (HsMultiIf _ lgrhss) = map getLoc lgrhss
pCaseMutli _ = []


grhs2Body :: GRHS p body -> Maybe body
grhs2Body (GRHS _ _ body) = Just body
grhs2Body _ = Nothing


simplifyLit :: HsLit GhcPs -> [HsLit GhcPs]
simplifyLit (HsString x (show -> s)) 
            | let l = length s =
                let s1 = take (div l 2) s
                    s2 = drop (div l 2) s
                in [HsString x (fromString ""), HsString x (fromString "")]
                -- in [HsString x (fromString s1), HsString x (fromString s2)]
            | otherwise = []
simplifyLit (HsStringPrim x (show -> s))
            | let l = length s =
                let s1 = take (div l 2) s
                    s2 = drop (div l 2) s
                in [HsString x (fromString ""), HsString x (fromString "")]
            | otherwise = []
simplifyLit _ = []
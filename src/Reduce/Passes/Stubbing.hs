module Reduce.Passes.Stubbing where

import Bag (bagToList, listToBag)
import GHC hiding (Pass)
import Util.Types
import Util.Util

printStubbingInfo :: R ()
printStubbingInfo = printInfo "Stubbing Expressions"

-- ******************************

-- DATA TYPE /W 1 CONS -> NEWTYPE

-- ******************************

-- change datadecl into newtype
-- change occurrences / pattern matches to not have the data constructor
-- function sigs don't need to be changed

-- ******************************

-- INLINE NEWTYPE

-- ******************************

-- change occurrences / pattern matches to not have the data constructor
-- Patterns
-- Function Sigs

-- 1. go over all data decls
-- 2. check if they are newtypes
-- 3. inline patterns that match the constructor name
-- 4. inline type sigs that match the type name

-- CONTEXTS
contexts :: Pass
contexts = mkPass "contexts" f
    where
        f :: WaysToChange (HsContext GhcPs)
        f = handleSubList (\loc -> filter ((/= loc) . getLoc)) (map getLoc)

-- ***************************************************************************
-- DERIVING CLAUSES
-- ***************************************************************************
simplifyDeriving :: WaysToChange [LHsDerivingClause GhcPs]
simplifyDeriving = handleSubList f p
    where
        p = map getLoc
        f loc = filter ((/= loc) . getLoc)

simplifyDerivingClause :: WaysToChange (HsDerivingClause GhcPs)
simplifyDerivingClause = handleSubList f p
    where
        p (HsDerivingClause _ _ t) = map (getLoc . unIB) $ unLoc t
        p _ = []
        f loc (HsDerivingClause x s t) = HsDerivingClause x s (filter ((/= loc) . getLoc . unIB) <$> t)
        f _ d = d

unIB :: HsImplicitBndrs pass thing -> thing
unIB (HsIB _ b) = b
unIB _ = error "Stubbing:unIB - trying to work with NoExt"


-- ***************************************************************************

-- BINDS

-- ***************************************************************************

data LocalBindSpan = Bind SrcSpan | Sig SrcSpan
    deriving (Eq)

localBinds :: WaysToChange (HsLocalBinds GhcPs)
localBinds EmptyLocalBinds {} = []
localBinds v@HsValBinds {} = handleSubList f p v <> [const (EmptyLocalBinds NoExt)]
    where
        p = \case
            (HsValBinds _ (ValBinds _ binds sigs)) ->
                let bindList = bagToList binds
                 in map (Sig . getLoc) sigs <> map (Bind . getLoc) bindList
            _ -> []
        f (Sig loc) = \case
            (HsValBinds _ (ValBinds _ binds sigs)) -> HsValBinds NoExt . ValBinds NoExt binds . filter ((/= loc) . getLoc) $ sigs
            hvb -> hvb
        f (Bind loc) = \case
            (HsValBinds _ (ValBinds _ binds sigs)) -> HsValBinds NoExt . ValBinds NoExt (listToBag . filter ((/= loc) . getLoc) $ bagToList binds) $ sigs
            hvb -> hvb
localBinds v@HsIPBinds {} = handleSubList f p v <> [const (EmptyLocalBinds NoExt)]
    where
        p = \case
            (HsIPBinds _ (IPBinds _ binds)) -> map getLoc binds
            _ -> []
        f loc = \case
            (HsIPBinds _ (IPBinds _ binds)) -> HsIPBinds NoExt . IPBinds NoExt . filter ((/= loc) . getLoc) $ binds
            hvb -> hvb
localBinds _ = [const (EmptyLocalBinds NoExt)]

-- ***************************************************************************

-- MATCHES

-- ***************************************************************************

simplifyMatch :: WaysToChange (Match GhcPs (LHsExpr GhcPs))
simplifyMatch (Match _ _ _ (GRHSs _ [] _)) = []
simplifyMatch mm = handleSubList f p mm
    where
        p = \case
            -- reverse because the lower have to be tried first
            MatchP iterGRHSs _ -> map getLoc . reverse $ iterGRHSs
            _ -> []
        f grhsLoc = \case
            m@(Match _ _ _ (GRHSs _ grhss lb)) ->
                let newGRHSs = filter ((/= grhsLoc) . getLoc) grhss
                 in case newGRHSs of
                        [] -> m
                        _ -> m {m_grhss = GRHSs NoExt newGRHSs lb}
            m -> m

simplifyMatches :: WaysToChange [LMatch GhcPs (LHsExpr GhcPs)]
simplifyMatches = handleSubList (\loc -> filter ((/= loc) . getLoc)) (map getLoc)

-- <> [ filter (\(L _ (Match _ _ _ grhss@GRHSs{})) -> showSDocUnsafe (pprGRHSs LambdaExpr grhss) /= "-> undefined")
-- ,  filter (\(L _ (Match _ _ _ (GRHSs _ grhs _))) -> not (all ( ("undefined" `isSubsequenceOf`) . showSDocUnsafe . pprGRHS LambdaExpr . unLoc) grhs))]

simplifyLGRHS :: WaysToChange (GRHS GhcPs (LHsExpr GhcPs))
simplifyLGRHS (GRHS _ [] _) = []
simplifyLGRHS g@(GRHS _ _ body) = [const (GRHS NoExt [] body)] <> handleSubList f p g
    where
        p (GRHS _ stmts _) = map getLoc stmts
        p _ = []
        f loc (GRHS _ s b) = GRHS NoExt (filter ((/= loc) . getLoc) s) b
        f _ _ = g
simplifyLGRHS _ = []


-- ***************************************************************************

-- MISC

-- ***************************************************************************

familyResultSig :: WaysToChange (FamilyResultSig GhcPs)
familyResultSig (NoSig _) = []
familyResultSig (XFamilyResultSig _) = []
familyResultSig _ = [const (NoSig NoExt)]

tyVarBndr :: WaysToChange (HsTyVarBndr GhcPs)
tyVarBndr (KindedTyVar _ lId _) = [const (UserTyVar NoExt lId)]
tyVarBndr _ = []

unqualNames :: WaysToChange RdrName
unqualNames (Qual _ on) = [const (Unqual on)]
unqualNames _ = []

-- ***************************************************************************

-- PATTERN SYNONYMS

-- ***************************************************************************

pattern MatchP :: [LGRHS GhcPs (LHsExpr GhcPs)] -> LHsLocalBinds GhcPs -> Match GhcPs (LHsExpr GhcPs)
pattern MatchP grhss binds <- Match _ _ _ (GRHSs _ grhss binds)
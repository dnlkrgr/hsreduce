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

simplifyDeriving :: Pass
simplifyDeriving = mkPass "simplifyDeriving" f
    where
        f :: WaysToChange [LHsDerivingClause GhcPs]
        f = handleSubList g p
            where
                p = map getLoc
                g loc = filter ((/= loc) . getLoc)

simplifyDerivingClause :: Pass
simplifyDerivingClause = mkPass "simplifyDerivingClause" f
    where
        f :: WaysToChange (HsDerivingClause GhcPs)
        f = handleSubList g p
            where
                p (HsDerivingClause _ _ t) = map (getLoc . unIB) $ unLoc t
                p _ = []
                g loc (HsDerivingClause x s t) = HsDerivingClause x s (filter ((/= loc) . getLoc . unIB) <$> t)
                g _ d = d

unIB :: HsImplicitBndrs pass thing -> thing
unIB (HsIB _ b) = b
unIB _ = error "Stubbing:unIB - trying to work with NoExt"

-- ***************************************************************************

-- BINDS

-- ***************************************************************************

data LocalBindSpan = Bind SrcSpan | Sig SrcSpan
    deriving (Eq)

localBinds :: Pass
localBinds = mkPass "localBinds" f
    where
        f :: WaysToChange (HsLocalBinds GhcPs)
        f EmptyLocalBinds {} = []
        f v@HsValBinds {} = handleSubList g p v <> [const (EmptyLocalBinds NoExt)]
            where
                p = \case
                    (HsValBinds _ (ValBinds _ binds sigs)) ->
                        let bindList = bagToList binds
                         in map (Sig . getLoc) sigs <> map (Bind . getLoc) bindList
                    _ -> []
                g (Sig loc) = \case
                    (HsValBinds _ (ValBinds _ binds sigs)) -> HsValBinds NoExt . ValBinds NoExt binds . filter ((/= loc) . getLoc) $ sigs
                    hvb -> hvb
                g (Bind loc) = \case
                    (HsValBinds _ (ValBinds _ binds sigs)) -> HsValBinds NoExt . ValBinds NoExt (listToBag . filter ((/= loc) . getLoc) $ bagToList binds) $ sigs
                    hvb -> hvb
        f v@HsIPBinds {} = handleSubList g p v <> [const (EmptyLocalBinds NoExt)]
            where
                p = \case
                    (HsIPBinds _ (IPBinds _ binds)) -> map getLoc binds
                    _ -> []
                g loc = \case
                    (HsIPBinds _ (IPBinds _ binds)) -> HsIPBinds NoExt . IPBinds NoExt . filter ((/= loc) . getLoc) $ binds
                    hvb -> hvb
        f _ = [const (EmptyLocalBinds NoExt)]

-- ***************************************************************************

-- MATCHES

-- ***************************************************************************

simplifyMatch :: Pass
simplifyMatch = mkPass "simplifyMatch" f
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

simplifyMatches :: Pass
simplifyMatches = mkPass "simplifyMatches" f
    where
        f :: WaysToChange [LMatch GhcPs (LHsExpr GhcPs)]
        f = handleSubList (\loc -> filter ((/= loc) . getLoc)) (map getLoc)

-- <> [ filter (\(L _ (Match _ _ _ grhss@GRHSs{})) -> showSDocUnsafe (pprGRHSs LambdaExpr grhss) /= "-> undefined")
-- ,  filter (\(L _ (Match _ _ _ (GRHSs _ grhs _))) -> not (all ( ("undefined" `isSubsequenceOf`) . showSDocUnsafe . pprGRHS LambdaExpr . unLoc) grhs))]

simplifyLGRHS :: Pass
simplifyLGRHS = mkPass "simplifyLGRHS" f
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

-- MISC

-- ***************************************************************************

familyResultSig :: Pass
familyResultSig = mkPass "familyResultSig" f
    where
        f :: WaysToChange (FamilyResultSig GhcPs)
        f (NoSig _) = []
        f (XFamilyResultSig _) = []
        f _ = [const (NoSig NoExt)]

tyVarBndr :: Pass
tyVarBndr = mkPass "tyVarBndr" f
    where
        f :: WaysToChange (HsTyVarBndr GhcPs)
        f (KindedTyVar _ lId _) = [const (UserTyVar NoExt lId)]
        f _ = []

unqualNames :: Pass
unqualNames = mkPass "unqualNames" f
    where
        f :: WaysToChange RdrName
        f (Qual _ on) = [const (Unqual on)]
        f _ = []

-- ***************************************************************************

-- PATTERN SYNONYMS

-- ***************************************************************************

pattern MatchP :: [LGRHS GhcPs (LHsExpr GhcPs)] -> LHsLocalBinds GhcPs -> Match GhcPs (LHsExpr GhcPs)
pattern MatchP grhss binds <- Match _ _ _ (GRHSs _ grhss binds)
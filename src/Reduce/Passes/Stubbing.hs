module Reduce.Passes.Stubbing
    ( contexts,
      rmvDerivingClause,
      simplifyDerivingClause,
      localBinds,
      tyVarBndr,
    )
where

import Bag (bagToList, listToBag)
import GHC
    ( GhcPs,
      HsContext,
      HsDerivingClause (HsDerivingClause),
      HsIPBinds (IPBinds),
      HsImplicitBndrs (HsIB),
      HsLocalBinds,
      HsLocalBindsLR (EmptyLocalBinds, HsIPBinds, HsValBinds),
      HsTyVarBndr (KindedTyVar, UserTyVar),
      HsValBindsLR (ValBinds),
      LHsDerivingClause,
      NoExtField (NoExtField),
      SrcSpan,
      getLoc,
      unLoc,
    )
import Util.Types (Pass, WaysToChange)
import Util.Util (handleSubList, mkPass)

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

rmvDerivingClause :: Pass
rmvDerivingClause = mkPass "rmvDerivingClause" f
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
unIB _ = error "Stubbing:unIB - trying to work with NoExtField"

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
        f v@HsValBinds {} = handleSubList g p v <> [const (EmptyLocalBinds NoExtField)]
            where
                p = \case
                    (HsValBinds _ (ValBinds _ binds sigs)) ->
                        let bindList = bagToList binds
                         in map (Sig . getLoc) sigs <> map (Bind . getLoc) bindList
                    _ -> []
                g (Sig loc) = \case
                    (HsValBinds _ (ValBinds _ binds sigs)) -> HsValBinds NoExtField . ValBinds NoExtField binds . filter ((/= loc) . getLoc) $ sigs
                    hvb -> hvb
                g (Bind loc) = \case
                    (HsValBinds _ (ValBinds _ binds sigs)) -> HsValBinds NoExtField . ValBinds NoExtField (listToBag . filter ((/= loc) . getLoc) $ bagToList binds) $ sigs
                    hvb -> hvb
        f v@HsIPBinds {} = handleSubList g p v <> [const (EmptyLocalBinds NoExtField)]
            where
                p = \case
                    (HsIPBinds _ (IPBinds _ binds)) -> map getLoc binds
                    _ -> []
                g loc = \case
                    (HsIPBinds _ (IPBinds _ binds)) -> HsIPBinds NoExtField . IPBinds NoExtField . filter ((/= loc) . getLoc) $ binds
                    hvb -> hvb
        f _ = [const (EmptyLocalBinds NoExtField)]

-- ***************************************************************************
-- MISC

-- ***************************************************************************

tyVarBndr :: Pass
tyVarBndr = mkPass "tyVarBndr" f
    where
        f :: WaysToChange (HsTyVarBndr GhcPs)
        f (KindedTyVar _ lId _) = [const (UserTyVar NoExtField lId)]
        f _ = []

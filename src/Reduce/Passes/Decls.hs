module Reduce.Passes.Decls (splitSigs, rmvSigs, rmvDecls, rmvConstructors, simplifyConDecl) where

import GHC hiding (Pass, getName)
import Util.Types
import Util.Util

-- ***************************************************************************
-- SIGNATURES

-- ***************************************************************************
-- split several names sharing one signature into several signatures

splitSigs :: Pass
splitSigs = mkPass "splitSigs" f
    where
        f :: WaysToChange (HsModule GhcPs)
        f m = [const (m {hsmodDecls = concatMap g $ hsmodDecls m})]
        g ld@(L l d)
            | SigD _ (TypeSig _ names sigType) <- d, length names > 1 = map (\name -> L l $ SigD NoExtField $ TypeSig NoExtField [name] sigType) names
            | SigD _ (PatSynSig _ names sigType) <- d, length names > 1 = map (\name -> L l $ SigD NoExtField $ PatSynSig NoExtField [name] sigType) names
            | SigD _ (ClassOpSig _ b names sigType) <- d, length names > 1 = map (\name -> L l $ SigD NoExtField $ ClassOpSig NoExtField b [name] sigType) names
            | otherwise = [ld]

rmvSigs :: Pass
rmvSigs = mkPass "rmvSigs" f
    where
        f :: WaysToChange (HsModule GhcPs)
        f = handleSubList rmvOneDecl (getDeclLocs (filter isSig))

-- ***************************************************************************
-- UNUSED DECLS IN MODULE

-- ***************************************************************************

rmvDecls :: Pass
rmvDecls = mkPass "rmvDecls" f
    where
        f :: WaysToChange (HsModule GhcPs)
        f = handleSubList rmvOneDecl (getDeclLocs id)


-- ***************************************************************************
-- HsDecls

-- ***************************************************************************
-- remove unused constructors

rmvConstructors :: Pass
rmvConstructors = mkPass "rmvConstructors" f
    where
        f :: WaysToChange (HsDecl GhcPs)
        f t@(TyClD {}) =
            handleSubList delCons decl2ConsStrings t
            where
                decl2ConsStrings = \case
                    (TyClD _ (DataDecl _ _ _ _ oldDataDefn)) -> map getLoc $ dd_cons oldDataDefn
                    _ -> []
                delCons loc = \case
                    (TyClD _ oDD@(DataDecl _ _ _ _ oldDataDefn)) -> TyClD NoExtField oDD {tcdDataDefn = oldDataDefn {dd_cons = filter ((/= loc) . getLoc) (dd_cons oldDataDefn)}}
                    d -> d
        f _ = []

-- ***************************************************************************
-- RECORD CON
-- it is called in `simplifyConDecl`

-- ***************************************************************************

recCon2Prefix :: HsConDetails (LBangType GhcPs) (Located [LConDeclField GhcPs]) -> HsConDetails (LBangType GhcPs) (Located [LConDeclField GhcPs])
recCon2Prefix (RecCon rec) = PrefixCon . map (cd_fld_type . unLoc) $ unLoc rec
recCon2Prefix d = d

-- two things happen here:
-- 1. remove qvars from GADT
-- 2. remove fields from record con
simplifyConDecl :: Pass
simplifyConDecl = mkPass "simplifyConDecl" f
    where
        f :: WaysToChange (ConDecl GhcPs)
        f gadtDecl@ConDeclGADT {} =
            handleSubList
                (\loc g -> g {con_qvars = HsQTvs NoExtField (filter ((/= loc) . getLoc) (hsq_explicit $ con_qvars g))})
                (map getLoc . hsq_explicit . con_qvars)
                gadtDecl
        f d
            | isRecCon d = handleSubList g p d <> [const (d {con_args = recCon2Prefix $ con_args d})]
            | otherwise = []
            where
                isRecCon =
                    ( \case
                          RecCon _ -> True
                          _ -> False
                    )
                        . con_args
                p =
                    ( \case
                          RecCon (L _ flds) -> map getLoc flds
                          _ -> []
                    )
                        . con_args
                g loc = \case
                    XConDecl x -> XConDecl x
                    c ->
                        c
                            { con_args = case con_args c of
                                  RecCon (L l flds) -> RecCon . L l $ filter ((/= loc) . getLoc) flds
                                  a -> a
                            }

-- ***************************************************************************
-- HELPER FUNCTIONS & OTHER STUFF

-- ***************************************************************************

isSig :: LHsDecl GhcPs -> Bool
isSig (L _ (SigD _ _)) = True
isSig _ = False

getDeclLocs :: ([LHsDecl GhcPs] -> [Located e]) -> HsModule GhcPs -> [SrcSpan]
getDeclLocs f = map getLoc . f . hsmodDecls

rmvOneDecl :: SrcSpan -> HsModule GhcPs -> HsModule GhcPs
rmvOneDecl loc m = m {hsmodDecls = filter ((/= loc) . getLoc) $ hsmodDecls m}
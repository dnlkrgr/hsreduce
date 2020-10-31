module Reduce.Passes.Decls (splitSigs, rmvSigs, rmvDecls, rmvConstructors, simplifyConDecl) where

import qualified Data.Text as T
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
            | SigD _ (TypeSig _ names sigType) <- d = map (\name -> L l $ SigD NoExt $ TypeSig NoExt [name] sigType) names
            | SigD _ (PatSynSig _ names sigType) <- d = map (\name -> L l $ SigD NoExt $ PatSynSig NoExt [name] sigType) names
            | SigD _ (ClassOpSig _ b names sigType) <- d = map (\name -> L l $ SigD NoExt $ ClassOpSig NoExt b [name] sigType) names
            | otherwise = [ld]

rmvSigs :: Maybe [T.Text] -> Pass
rmvSigs mb = mkPass "rmvSigs" (f mb)
    where
        f :: Maybe [T.Text] -> WaysToChange (HsModule GhcPs)
        f Nothing = handleSubList rmvOneDecl (getDeclLocs (filter isSig))
        f (Just unusedBinds) = handleSubList rmvOneDecl (getDeclLocs (filter (isSig <&&> (maybe False ((`elem` unusedBinds) . T.pack . oshow) . getName))))

-- ***************************************************************************
-- UNUSED DECLS IN MODULE

-- ***************************************************************************

rmvDecls :: Maybe [T.Text] -> Pass
rmvDecls mb = mkPass "rmvDecls" (f mb)
    where
        f :: Maybe [T.Text] -> WaysToChange (HsModule GhcPs)
        f Nothing = defaultBehavior
        f (Just []) = defaultBehavior
        f (Just unusedBinds) =
            handleSubList rmvOneDecl (getDeclLocs (filter (maybe False ((`elem` unusedBinds) . T.pack . oshow) . getName)))

-- ***************************************************************************
-- HsDecls

-- ***************************************************************************
-- remove unused constructors

rmvConstructors :: Maybe [T.Text] -> Pass
rmvConstructors mb = mkPass "rmvConstructors" (f mb)
    where
        f :: Maybe [T.Text] -> WaysToChange (HsDecl GhcPs)
        f _ t@(TyClD {}) =
            handleSubList delCons decl2ConsStrings t
            where
                decl2ConsStrings = \case
                    (TyClD _ (DataDecl _ _ _ _ oldDataDefn)) -> map getLoc $ dd_cons oldDataDefn
                    _ -> []
                delCons loc = \case
                    (TyClD _ oDD@(DataDecl _ _ _ _ oldDataDefn)) -> TyClD NoExt oDD {tcdDataDefn = oldDataDefn {dd_cons = filter ((/= loc) . getLoc) (dd_cons oldDataDefn)}}
                    d -> d
        f _ _ = []

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
        f gadtDecl@(ConDeclGADT {}) =
            handleSubList
                (\loc g -> g {con_qvars = HsQTvs NoExt (filter ((/= loc) . getLoc) (hsq_explicit $ con_qvars g))})
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
                    XConDecl _ -> XConDecl NoExt
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

defaultBehavior :: WaysToChange (HsModule GhcPs)
defaultBehavior = handleSubList rmvOneDecl (map getLoc . hsmodDecls)

getDeclLocs :: ([LHsDecl GhcPs] -> [Located e]) -> HsModule GhcPs -> [SrcSpan]
getDeclLocs f = map getLoc . f . hsmodDecls

rmvOneDecl :: SrcSpan -> HsModule GhcPs -> HsModule GhcPs
rmvOneDecl loc m = m {hsmodDecls = filter ((/= loc) . getLoc) $ hsmodDecls m}

-- TODO: what other decls make sense here?
getName :: LHsDecl GhcPs -> Maybe (IdP GhcPs)
getName (L _ (TyClD _ d)) = Just . tcdName $ d
getName (L _ (SimplFunP funId)) = Just . unLoc $ funId
getName (L _ (SimplSigP funId)) = Just . unLoc $ funId
getName _ = Nothing

-- ***************************************************************************
-- PATTERNS

-- ***************************************************************************

pattern SimplSigP, SimplFunP :: Located (IdP GhcPs) -> HsDecl GhcPs
pattern SimplSigP lFunId <- SigD _ (TypeSig _ [lFunId] _)
pattern SimplFunP lFunId <- ValD _ (FunBind _ lFunId _ _ _)

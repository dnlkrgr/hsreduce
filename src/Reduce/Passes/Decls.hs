module Reduce.Passes.Decls (rmvSigs, rmvDecls, simplifyDecl, recCon2Prefix, simplifyConDecl) where

import BasicTypes (Origin)
import CoreSyn (Tickish)
import qualified Data.Text as T
import GHC
    ( ConDecl
          ( ConDeclGADT,
            XConDecl,
            con_args,
            con_forall,
            con_mb_cxt,
            con_qvars
          ),
      ConDeclField (cd_fld_type),
      GenLocated (L),
      GhcPs,
      HsBindLR (FunBind),
      HsConDetails (PrefixCon, RecCon),
      HsDataDefn (dd_cons),
      HsDecl (SigD, TyClD, ValD),
      HsMatchContext (LambdaExpr),
      HsModule (hsmodDecls),
      Id,
      IdP,
      LBangType,
      LConDeclField,
      LHsDecl,
      LHsExpr,
      LHsQTyVars (HsQTvs, hsq_explicit),
      LHsSigWcType,
      LMatch,
      Located,
      Match (Match),
      MatchGroup (MG),
      NoExt (NoExt),
      Sig (TypeSig),
      SrcSpan,
      TyClDecl (DataDecl, tcdDataDefn),
      getLoc,
      pprGRHSs,
      tcdName,
      unLoc,
    )
import Outputable (showSDocUnsafe)
import TcEvidence (HsWrapper)
import Util.Types (Pass, WaysToChange)
import Util.Util ((<&&>), handleSubList, mkPass, oshow)

-- ***************************************************************************
-- SIGNATURES

-- ***************************************************************************

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
        f (Just unusedBinds) = handleSubList rmvOneDecl (getDeclLocs (filter (maybe False ((`elem` unusedBinds) . T.pack . oshow) . getName)))

-- ***************************************************************************
-- HsDecls

-- ***************************************************************************

simplifyDecl :: Maybe [T.Text] -> Pass
simplifyDecl mb = mkPass "simplifyDecl" (f mb)
    where
        f :: Maybe [T.Text] -> WaysToChange (HsDecl GhcPs)
        f (Just bns) (TypeSigDeclP ids swt) =
            let newFunIds = filter ((`notElem` bns) . T.pack . oshow . unLoc) ids
             in [const (TypeSigDeclX newFunIds swt)]
        f _ t@(TypeSigDeclP {}) =
            handleSubList transformTypeSig typeSig2Ids t
            where
                typeSig2Ids = \case
                    TypeSigDeclP ids _ -> ids
                    _ -> []
                transformTypeSig e = \case
                    TypeSigDeclP ids swt -> TypeSigDeclX (filter (/= e) ids) swt
                    d -> d
        f _ (FunDeclP fid loc mtchs mo fw ft) =
            let nMtchs = filter (\(L _ (Match _ _ _ grhss)) -> showSDocUnsafe (pprGRHSs LambdaExpr grhss) /= "-> undefined") mtchs
             in [const (FunDeclP fid loc nMtchs mo fw ft)]
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

-- ***************************************************************************

recCon2Prefix :: Pass
recCon2Prefix = mkPass "recCon2Prefix" f
    where
        f :: WaysToChange (HsConDetails (LBangType GhcPs) (Located [LConDeclField GhcPs]))
        f (RecCon rec) = [const (PrefixCon . map (cd_fld_type . unLoc) $ unLoc rec)]
        f _ = []

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
rmvOneDecl loc m =
    m {hsmodDecls = filter ((/= loc) . getLoc) $ hsmodDecls m}

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

pattern FunDeclP :: Located (IdP GhcPs) -> SrcSpan -> [LMatch GhcPs (LHsExpr GhcPs)] -> Origin -> HsWrapper -> [Tickish Id] -> HsDecl GhcPs
pattern FunDeclP lFunId matchesLoc funMatches mgOrigin funWrapper funTick =
    ValD NoExt (FunBind NoExt lFunId (MG NoExt (L matchesLoc funMatches) mgOrigin) funWrapper funTick)

pattern TypeSigDeclP :: [Located (IdP GhcPs)] -> LHsSigWcType GhcPs -> HsDecl GhcPs
pattern TypeSigDeclP funIds sigWctype <- (SigD _ (TypeSig _ funIds sigWctype))

pattern TypeSigDeclX :: [Located (IdP GhcPs)] -> LHsSigWcType GhcPs -> HsDecl GhcPs
pattern TypeSigDeclX funIds sigWctype = SigD NoExt (TypeSig NoExt funIds sigWctype)

simplifyConDecl :: Pass
simplifyConDecl = mkPass "simplifyConDecl" f
    where
        f :: WaysToChange (ConDecl GhcPs)
        f gadtDecl@(ConDeclGADT _ _ (L forallLoc _) _ _ _ _ _) =
            map const [gadtDecl {con_forall = L forallLoc False}, gadtDecl {con_mb_cxt = Nothing}] <> temp gadtDecl
            where
                temp = handleSubList (\loc g -> g {con_qvars = HsQTvs NoExt (filter ((/= loc) . getLoc) (hsq_explicit $ con_qvars g))}) (map getLoc . hsq_explicit . con_qvars)
        f d
            | isRecCon d = handleSubList g p d -- <> [const (d { con_args = recCon2Prefix $ con_args d})]
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
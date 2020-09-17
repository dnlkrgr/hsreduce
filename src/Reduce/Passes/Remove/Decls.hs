module Reduce.Passes.Remove.Decls where

import BasicTypes
import Control.Monad.State.Strict
import CoreSyn
import qualified Data.Text as T
import GHC hiding (Pass, getName)
import Outputable hiding ((<>))
import Path
import TcEvidence
import Util.Types
import Util.Util


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
        f _ (FunDeclP fid loc mtchs mo fw ft) =
            let nMtchs = filter (\(L _ (Match _ _ _ grhss)) -> showSDocUnsafe (pprGRHSs LambdaExpr grhss) /= "-> undefined") mtchs
             in [const (FunDeclP fid loc nMtchs mo fw ft)]
        f _ t@(TypeSigDeclP {}) =
            handleSubList transformTypeSig typeSig2Ids t
            where
                typeSig2Ids = \case
                    TypeSigDeclP ids _ -> ids
                    _ -> []
                transformTypeSig e = \case
                    TypeSigDeclP ids swt -> TypeSigDeclX (filter (/= e) ids) swt
                    d -> d
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

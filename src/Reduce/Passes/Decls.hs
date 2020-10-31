module Reduce.Passes.Decls (rmvSigs, rmvDecls, rmvConstructors, recCon2Prefix, simplifyConDecl) where

import qualified Data.Text as T
import GHC hiding (Pass, getName)
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
        f (Just unusedBinds) = 
            handleSubList rmvOneDecl (getDeclLocs (filter (maybe False ((`elem` unusedBinds) . T.pack . oshow) . getName)))

-- ***************************************************************************
-- HsDecls

-- ***************************************************************************

-- remove unused constructors
rmvConstructors :: Maybe [T.Text] -> Pass
rmvConstructors mb = mkPass "simplifyDecl" (f mb)
    where
        f :: Maybe [T.Text] -> WaysToChange (HsDecl GhcPs)
        -- I THINK THESE CASES ARE COVERED BY `rmvSig`
        -- f (Just bns) (TypeSigDeclP ids swt) =
        --     let newFunIds = filter ((`notElem` bns) . T.pack . oshow . unLoc) ids
        --      in [const (TypeSigDeclX newFunIds swt)]
        -- f _ t@(TypeSigDeclP {}) =
        --     handleSubList transformTypeSig typeSig2Ids t
        --     where
        --         typeSig2Ids = \case
        --             TypeSigDeclP ids _ -> ids
        --             _ -> []
        --         transformTypeSig e = \case
        --             TypeSigDeclP ids swt -> TypeSigDeclX (filter (/= e) ids) swt
        --             d -> d
        -- this case is covered by `rmvMatches`
        -- f _ (FunDeclP fid loc mtchs mo fw ft) =
        --     let nMtchs = filter (\(L _ (Match _ _ _ grhss)) -> showSDocUnsafe (pprGRHSs LambdaExpr grhss) /= "-> undefined") mtchs
        --      in [const (FunDeclP fid loc nMtchs mo fw ft)]
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

-- remove qvars from GADT and remove fields from record con
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

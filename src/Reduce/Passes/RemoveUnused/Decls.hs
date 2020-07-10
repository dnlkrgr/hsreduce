module Reduce.Passes.RemoveUnused.Decls where

import Debug.Trace
import Path 
import Data.Either
import Control.Monad.State.Strict
import qualified Data.Text as T
import Util.Types
import Util.Util
import Control.Monad.Reader
import GHC hiding (getName)
import Outputable
import BasicTypes
import TcEvidence
import CoreSyn

fast :: R ()
fast = do
    tchan      <- asks _tempDirs
    sourceFile <- asks _sourceFile
    printInfo "Removing unused declarations"

    mUnusedBinds <- fmap (map (fromRight "" . fst)) <$> liftIO (withTempDir tchan $ \temp -> getGhcOutput Ghc Binds (temp </> sourceFile))
    (traceShow ("rmvSigs"           :: String) $ runPass (rmvSigs mUnusedBinds))
    (traceShow ("rmvDecls"          :: String) $ runPass (rmvDecls mUnusedBinds))

slow :: R ()
slow = do
    tchan      <- asks _tempDirs
    sourceFile <- asks _sourceFile
    printInfo "Removing unused declarations"

    mUnusedBinds <- fmap (map (fromRight "" . fst)) <$> liftIO (withTempDir tchan $ \temp -> getGhcOutput Ghc Binds (temp </> sourceFile))
    (traceShow ("simplifyDecl"      :: String) $ runPass (simplifyDecl mUnusedBinds))


-- ***************************************************************************
-- SIGNATURES
-- ***************************************************************************
rmvSigs :: Maybe [T.Text] -> WaysToChange (HsModule GhcPs)
rmvSigs Nothing            = handleSubList rmvOneDecl (getDeclLocs (filter isSig))
rmvSigs (Just unusedBinds) = handleSubList rmvOneDecl (getDeclLocs (filter (isSig <&&> (maybe False ((`elem` unusedBinds) . T.pack . oshow) . getName))))

isSig :: LHsDecl GhcPs -> Bool
isSig (L _ (SigD _ _))  = True
isSig _                 = False


-- ***************************************************************************
-- UNUSED DECLS IN MODULE
-- ***************************************************************************
rmvDecls :: Maybe [T.Text] -> WaysToChange (HsModule GhcPs)
rmvDecls Nothing            = defaultBehavior
rmvDecls (Just [])          = defaultBehavior
rmvDecls (Just unusedBinds) = handleSubList rmvOneDecl (getDeclLocs (filter (maybe False ((`elem` unusedBinds) . T.pack . oshow) . getName)))

defaultBehavior :: WaysToChange (HsModule GhcPs)
defaultBehavior = handleSubList rmvOneDecl (map getLoc . hsmodDecls)

getDeclLocs :: ([LHsDecl GhcPs] -> [Located e]) -> HsModule GhcPs -> [SrcSpan]
getDeclLocs f = map getLoc . f .  hsmodDecls

rmvOneDecl :: SrcSpan -> HsModule GhcPs -> HsModule GhcPs
rmvOneDecl loc m = m { hsmodDecls = filter ((/= loc) . getLoc ) $ hsmodDecls m }

-- TODO: what other decls make sense here?
getName :: LHsDecl GhcPs -> Maybe (IdP GhcPs)
getName (L _ (TyClD _ d))       = Just . tcdName $ d
getName (L _ (SimplFunP funId)) = Just . unLoc $ funId
getName (L _ (SimplSigP funId)) = Just . unLoc $ funId
getName _ = Nothing


-- ***************************************************************************
-- HsDecls
-- ***************************************************************************
simplifyDecl :: Maybe [T.Text] -> WaysToChange (HsDecl GhcPs)
simplifyDecl (Just bns) (TypeSigDeclP ids swt) =
    let newFunIds = filter ((`notElem` bns) . T.pack . oshow . unLoc) ids
    in [const (TypeSigDeclX newFunIds swt)]
simplifyDecl _ (FunDeclP fid loc mtchs mo fw ft) =
    let nMtchs = filter (\(L _ (Match _ _ _ grhss)) -> showSDocUnsafe (pprGRHSs LambdaExpr grhss) /= "-> undefined") mtchs
    in [const (FunDeclP fid loc nMtchs mo fw ft)]
simplifyDecl _ t@(TypeSigDeclP{}) = 
    handleSubList transformTypeSig typeSig2Ids t
  where
      typeSig2Ids = \case
          TypeSigDeclP ids _      -> ids
          _                       -> []
      transformTypeSig e = \case
          TypeSigDeclP ids swt    -> TypeSigDeclX (filter (/= e) ids) swt
          d                       -> d
simplifyDecl _ t@(TyClD {}) = 
    handleSubList delCons decl2ConsStrings t
  where
      decl2ConsStrings = \case
          (TyClD _ (DataDecl _ _ _ _ oldDataDefn))  -> map getLoc $ dd_cons oldDataDefn
          _                                         -> []
      delCons loc = \case
          (TyClD _ oDD@(DataDecl _ _ _ _ oldDataDefn)) -> TyClD NoExt oDD { tcdDataDefn = oldDataDefn { dd_cons = filter ((/= loc) . getLoc) (dd_cons oldDataDefn)}}
          d                                            -> d
simplifyDecl _ _ = []


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

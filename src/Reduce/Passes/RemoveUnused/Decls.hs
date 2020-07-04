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

-- | run ghc with -Wunused-binds -ddump-json and delete decls that are mentioned there
reduce :: R ()
reduce = do
    tchan      <- asks _tempDirs
    oldState   <- get
    sourceFile <- asks _sourceFile

    liftIO $ putStrLn "\n***Removing unused declarations***"
    liftIO $ putStrLn $ "Size of old state: " ++ (show . T.length . showState $ oldState)

    mUnusedBinds <- fmap (map (fromRight "" . fst)) <$> liftIO (withTempDir tchan $ \temp -> getGhcOutput Ghc Binds (temp </> sourceFile))

    void . allActions mUnusedBinds . _parsed =<< get


allActions :: Maybe [T.Text] -> ParsedSource -> R ParsedSource
allActions mUnusedBinds = 
        (traceShow ("rmvSigs"           :: String) $ runPass (rmvSigs mUnusedBinds))
    >=> (traceShow ("simplifyHsDecl"    :: String) $ runPass (simplifyHsDecl mUnusedBinds))
    >=> (traceShow ("rmvDecls"          :: String) $ runPass (rmvDecls mUnusedBinds))


-- ***************************************************************************
-- SIGNATURES
-- ***************************************************************************
rmvSigs :: Maybe [T.Text] -> WaysToChange (HsModule GhcPs)
rmvSigs Nothing             = h rmvOneDecl (getDeclLocs (filter isSig))
rmvSigs (Just unusedBinds)  = h rmvOneDecl (getDeclLocs (filter (isSig <&&> (maybe False ((`elem` unusedBinds) . T.pack . oshow) . getName))))

isSig :: LHsDecl GhcPs -> Bool
isSig (L _ (SigD _ _))  = True
isSig _                 = False


-- ***************************************************************************
-- UNUSED DECLS IN MODULE
-- ***************************************************************************
rmvDecls :: Maybe [T.Text] -> WaysToChange (HsModule GhcPs)
rmvDecls Nothing            = defaultBehavior
rmvDecls (Just [])          = defaultBehavior
rmvDecls (Just unusedBinds) = h rmvOneDecl (getDeclLocs (filter (maybe False ((`elem` unusedBinds) . T.pack . oshow) . getName)))

defaultBehavior :: WaysToChange (HsModule GhcPs)
defaultBehavior = h rmvOneDecl (map getLoc . hsmodDecls) 

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

simplifyHsDecl :: Maybe [T.Text] -> WaysToChange (HsDecl GhcPs)
simplifyHsDecl (Just bns) (TypeSigDeclP ids swt) =
    let newFunIds = filter ((`notElem` bns) . T.pack . oshow . unLoc) ids
    in [const (TypeSigDeclX newFunIds swt)]
simplifyHsDecl _ (FunDeclP fid loc mtchs mo fw ft) =
    let nMtchs = filter (\(L _ (Match _ _ _ grhss)) -> showSDocUnsafe (pprGRHSs LambdaExpr grhss) /= "-> undefined") mtchs
    in [const (FunDeclP fid loc nMtchs mo fw ft)]
simplifyHsDecl _ t@(TypeSigDeclP{}) = 
    h transformTypeSig typeSig2Ids t
    where
        typeSig2Ids = \case
            TypeSigDeclP ids _ -> ids
            _ -> []
        transformTypeSig e = \case
            TypeSigDeclP ids swt ->
              let newIds = filter (/= e) ids -- L l . flip TypeSigDeclX swt
              in TypeSigDeclX newIds swt
            d -> d
simplifyHsDecl _ t@(TyClD {}) = 
    h delCons decl2ConsStrings t
    where
        decl2ConsStrings = \case
            (TyClD _ (DataDecl _ _ _ _ oldDataDefn)) -> map getLoc $ dd_cons oldDataDefn
            _ -> []
        delCons loc = \case
            (TyClD _ oDD@(DataDecl _ _ _ _ oldDataDefn)) ->
                let newCons = dd_cons oldDataDefn
                in TyClD NoExt oDD { tcdDataDefn = oldDataDefn { dd_cons = filter ((/= loc) . getLoc) newCons}}
            d -> d
simplifyHsDecl _ _ = []

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

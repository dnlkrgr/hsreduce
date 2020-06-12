module Reduce.Passes.RemoveUnused.Decls (reduce) where

import Data.Either
import Debug.Trace
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
import Data.Generics.Uniplate.Data

-- | run ghc with -Wunused-binds -ddump-json and delete decls that are mentioned there
reduce :: R ()
reduce = do
    oldState <- get
    sourceFile <- asks _sourceFile
    liftIO $ putStrLn "\n***Removing unused declarations***"
    liftIO $ putStrLn $ "Size of old state: " ++ (show . T.length . showState $ oldState)
    mUnusedBinds <- fmap (map (fromRight "" . fst)) <$> liftIO (getGhcOutput Ghc Binds sourceFile)
    _ <- (traceShow ("rmvSigs" :: String) descendBiM (rmvSigs mUnusedBinds)
            >=> traceShow ("filterUnusedSigLists" :: String) descendBiM (fastTry (filterUnusedSigLists mUnusedBinds))
            >=> traceShow ("filterSigLists" :: String) descendBiM (fastTryR filterSigLists)
            >=> traceShow ("rmvDecls" :: String) descendBiM (rmvDecls mUnusedBinds)
            >=> traceShow ("rmvCons" :: String) descendBiM (fastTryR rmvCons)
            >=> traceShow ("undefFunBind" :: String) descendBiM (fastTry undefFunBind))
             (_parsed oldState)
    return ()



-- ***************************************************************************
-- SIGNATURES
-- ***************************************************************************

-- | remove whole signature declarations
rmvSigs :: Maybe [T.Text] -> ParsedSource -> R ParsedSource
rmvSigs Nothing = reduceListOfSubelements f' rmvOneDecl
  where f' = getDeclLocs (filter (isSig . unLoc))
rmvSigs (Just unusedBinds) = reduceListOfSubelements f' rmvOneDecl
  where f' = getDeclLocs (filter ((isSig
                                  <&&> (maybe False ((`elem` unusedBinds) . T.pack . oshow)
                                        . getName))
                                  . unLoc))

-- | filter out unused IDs in a signature
filterUnusedSigLists :: Maybe [T.Text] -> HsDecl GhcPs -> Maybe (HsDecl GhcPs)
filterUnusedSigLists (Just bns) (TypeSigDeclP ids swt) =
  let newFunIds = filter ((`notElem` bns) . T.pack . oshow . unLoc) ids
  in Just $ TypeSigDeclX newFunIds swt
filterUnusedSigLists _ _ = Nothing

-- | brute force filter out IDs in a signature
filterSigLists :: LHsDecl GhcPs -> Maybe (R (LHsDecl GhcPs))
filterSigLists ldcl@(L _ TypeSigDeclP{}) =
  Just $ reduceListOfSubelements typeSig2Ids transformTypeSig ldcl
  where
    typeSig2Ids = \case
      TypeSigDeclP ids _ -> ids
      _ -> []
    transformTypeSig e = \case
      TypeSigDeclP ids swt ->
        let newIds = filter (/= e) ids -- L l . flip TypeSigDeclX swt
        in TypeSigDeclX newIds swt
      d -> d
filterSigLists _ = Nothing

isSig :: HsDecl GhcPs -> Bool
isSig (SigD _ _) = True
isSig _ = False



-- ***************************************************************************
-- DECLS
-- ***************************************************************************

-- | remove unused decls
rmvDecls :: Maybe [T.Text] -> ParsedSource -> R ParsedSource
rmvDecls Nothing   = defaultBehavior
rmvDecls (Just []) = defaultBehavior
rmvDecls (Just unusedBinds) = reduceListOfSubelements f' rmvOneDecl
  where f' = getDeclLocs (filter (maybe False ((`elem` unusedBinds) . T.pack . oshow) . getName . unLoc))

defaultBehavior :: ParsedSource -> R ParsedSource
defaultBehavior = reduceListOfSubelements (map getLoc . hsmodDecls) rmvOneDecl

getDeclLocs :: ([LHsDecl GhcPs] -> [Located e]) -> HsModule GhcPs -> [SrcSpan]
getDeclLocs h = map getLoc . h .  hsmodDecls

rmvOneDecl :: SrcSpan -> HsModule GhcPs -> HsModule GhcPs
rmvOneDecl loc m = m { hsmodDecls = filter ((/= loc) . getLoc ) $ hsmodDecls m }

-- TODO: what other decls make sense here?
getName :: HsDecl GhcPs -> Maybe (IdP GhcPs)
getName (TyClD _ d)       = Just . tcdName $ d
getName (SimplFunP funId) = Just . unLoc $ funId
getName (SimplSigP funId) = Just . unLoc $ funId
getName _ = Nothing



-- ***************************************************************************
-- CONSTRUCTORS
-- ***************************************************************************

-- | brute force remove constructors
rmvCons :: LHsDecl GhcPs -> Maybe (R (LHsDecl GhcPs))
rmvCons t@(L _ (TyClD _ DataDecl{})) =
  Just $ reduceListOfSubelements decl2ConsStrings delCons t
  where
    decl2ConsStrings = \case
      (TyClD _ (DataDecl _ _ _ _ oldDataDefn)) -> map getLoc $ dd_cons oldDataDefn
      _ -> []
    delCons loc = \case
      (TyClD _ oDD@(DataDecl _ _ _ _ oldDataDefn)) ->
        let newCons = dd_cons oldDataDefn
        in TyClD NoExt oDD { tcdDataDefn = oldDataDefn { dd_cons = filter ((/= loc) . getLoc) newCons}}
      d -> d
rmvCons _ = Nothing



-- ***************************************************************************
-- FUN BINDS
-- ***************************************************************************

-- | remove fun binds with undefined rhs
undefFunBind :: HsDecl GhcPs -> Maybe (HsDecl GhcPs)
undefFunBind (FunDeclP fid loc mtchs mo fw ft) =
  let nMtchs =
        filter (\(L _ (Match _ _ _ grhss)) ->
          showSDocUnsafe (pprGRHSs LambdaExpr grhss) /= "-> undefined") mtchs
  in Just (FunDeclP fid loc nMtchs mo fw ft)
undefFunBind _ = Nothing



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

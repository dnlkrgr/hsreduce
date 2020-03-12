module Passes.RemoveUnused.Decls where

import Control.Monad.State.Strict
import qualified Data.Text as T
import Debug.Trace
import HsSyn
import SrcLoc
import TcEvidence
import CoreSyn
import Var
import Ormolu.Parser.Result as OPR (ParseResult, prParsedSource)
import Ormolu.Printer (printModule)
import Outputable (ppr, showSDocUnsafe)
import BasicTypes
import Types
import Util

-- | run ghc with -Wunused-binds -ddump-json and delete decls that are mentioned there
reduce :: FilePath -> FilePath -> OPR.ParseResult -> IO OPR.ParseResult
reduce test sourceFile oldOrmolu = do
  putStrLn "\n***Removing unused declarations***"
  debugPrint $ "Size of old ormolu: " ++ (show . T.length $ printModule oldOrmolu)
  runGhc sourceFile oldOrmolu Binds
    >>= \case
      Nothing -> return oldOrmolu
      Just unusedBindingNames -> do
        debugPrint $ "unused binding names: " ++ show unusedBindingNames
        let allDecls = hsmodDecls . unLoc . prParsedSource $ oldOrmolu
            usedDecls = filter (maybe False (`notElem` unusedBindingNames) . nameAccessor) allDecls
        _ormolu
          <$> execStateT
            (traverse (removeUnusedDecl unusedBindingNames) usedDecls)
            (ReduceState test sourceFile oldOrmolu)

nameAccessor :: LHsDecl GhcPs -> Maybe String
nameAccessor (SimplFunDecl funId) = Just . showSDocUnsafe . ppr $ funId
nameAccessor (SimplTypeSigDecl [funId]) = Just . showSDocUnsafe . ppr $ funId
nameAccessor (SimplDataDecl tyId) = Just . showSDocUnsafe . ppr $ tyId
nameAccessor (SimplPatSyn synId) = Just . showSDocUnsafe . ppr $ synId
nameAccessor _ = Nothing


-- | remove:
-- - unused function bindings with their type signatures
-- - constructors from unused data types
-- - data types without constructors
removeUnusedDecl :: [BindingName] -> LHsDecl GhcPs -> StateT ReduceState IO ()
-- TODO: iwie ist das unschön, dass hier mehr gemacht wird, als nur die value decl zu löschen
removeUnusedDecl unusedBindingNames (FunDecl declLoc funId matchesLoc funMatches mgOrigin funWrapper funTick) = do
  oldOrmolu <- _ormolu <$> get
  if showSDocUnsafe (ppr $ unLoc funId) `elem` unusedBindingNames then do
    debugPrint $ "looking at function binding: " ++ (showSDocUnsafe . ppr . unLoc $ funId)
    let newOrmolu =
          changeDecls
            oldOrmolu
            ( map
                ( \iterDecl -> case iterDecl of
                    -- if a signature is defined for several IDs, the function ID should be removed from the list of identifiers
                    -- TODO: check if this is working properly
                    TypeSigDecl iterLoc funIds sigWctype ->
                      let newFunIds = filter ((`notElem` unusedBindingNames) . showSDocUnsafe . ppr . unLoc) funIds
                      in  TypeSigDeclX iterLoc newFunIds sigWctype
                    _ -> iterDecl
                )
                . filter
                  ( \(L iterLoc iterDecl) ->
                      case iterDecl of
                        SigD _ (TypeSig _ [tyFunId] _) -> ((`notElem` unusedBindingNames) . showSDocUnsafe . ppr . unLoc) tyFunId
                        _ -> iterLoc /= declLoc
                  )
            )
    testAndUpdateState newOrmolu
  else do
    let newMatches = 
          filter (\(L _ (Match _ _ _ grhss)) -> showSDocUnsafe (pprGRHSs LambdaExpr grhss) /= "-> undefined") funMatches 
        newDecl = FunDecl declLoc funId matchesLoc newMatches mgOrigin funWrapper funTick
        newOrmolu = changeDecls oldOrmolu (map (\(L iterLoc iterDecl) -> if iterLoc == declLoc then newDecl else L iterLoc iterDecl))
    testAndUpdateState newOrmolu
removeUnusedDecl unusedBindingNames (L declLoc (TyClD _ oldDecl@(DataDecl _ _ _ _ oldDataDefn))) = do
  oldOrmolu <- _ormolu <$> get
  let constructors     = dd_cons oldDataDefn
      construcorsInUse = filter (constructorIsUsed unusedBindingNames) constructors
  newConstructors <- traverse simplifyGADTs construcorsInUse
  let newDecl = TyClD NoExt oldDecl{ tcdDataDefn = oldDataDefn{ dd_cons = newConstructors}}
      newOrmolu =
        changeDecls
          oldOrmolu
          ( if null constructors
              then filter (\(L iterLoc _) -> iterLoc /= declLoc)
              else map (\(L iterLoc iterDecl) -> if iterLoc == declLoc then L declLoc newDecl else L iterLoc iterDecl)
          )
  testAndUpdateState newOrmolu
-- TODO: more fine granular handling of instance decls
removeUnusedDecl _ (L declLoc _) = do
  ReduceState _ _ oldOrmolu <- get
  let newOrmolu = changeDecls oldOrmolu (filter (\(L iterLoc _) -> iterLoc /= declLoc))
  testAndUpdateState newOrmolu

simplifyGADTs :: LConDecl GhcPs -> StateT ReduceState IO (LConDecl GhcPs)
simplifyGADTs decl@(L declLoc gadtDecl@(ConDeclGADT _ _ (L forallLoc _) _ _ _ (L tyLoc conTy) _)) = do
  liftIO $ putStrLn $ "conTy: " ++ showSDocUnsafe (ppr conTy)
  -- delete forall
  let newDecl = L declLoc gadtDecl{ con_forall = L forallLoc False}
  L l2 newDecl2 <- tryNewDecl decl newDecl
  -- delete context
  let newDecl3 = L declLoc newDecl2{ con_mb_cxt = Nothing}
  tryNewDecl (L l2 newDecl2) newDecl3
simplifyGADTs d = return d

tryNewDecl :: LConDecl GhcPs -> LConDecl GhcPs -> StateT ReduceState IO (LConDecl GhcPs)
tryNewDecl oldDecl@(L declLoc _) newDecl = do
  oldOrmolu <- _ormolu <$> get
  let oldModule = prParsedSource oldOrmolu
      newModule = everywhereT (mkT (overwriteAtLoc declLoc newDecl)) oldModule
  testAndUpdateStateFlex (oldOrmolu{ prParsedSource = newModule}) oldDecl newDecl

-- TODO: check what to do in 3rd case
constructorIsUsed :: [String] -> LConDecl GhcPs -> Bool
constructorIsUsed unusedBindingNames (H98Decl rdrName) = (showSDocUnsafe . ppr $ rdrName) `notElem` unusedBindingNames
constructorIsUsed unusedBindingNames (GADTDecl names) =
  let result = all (\(L _ rdrName) -> (showSDocUnsafe . ppr $ rdrName) `notElem` unusedBindingNames) names
   in traceShow (if not result then "Is constructor " ++ unwords (map (showSDocUnsafe . ppr) names) ++ " used? " ++ show result else []) result
constructorIsUsed _ _ = True

pattern SimplFunDecl :: forall l p. IdP p -> GenLocated l (HsDecl p)
pattern SimplFunDecl funId <- L _ (ValD _ (FunBind _ (L _ funId) _ _ _))

pattern SimplTypeSigDecl :: forall l p. [Located (IdP p)] -> GenLocated l (HsDecl p)
pattern SimplTypeSigDecl funIds <- L _ (SigD _ (TypeSig _ funIds _))

pattern SimplDataDecl :: forall l p. Located (IdP p) -> GenLocated l (HsDecl p)
pattern SimplDataDecl tyId <- L _ (TyClD _ (DataDecl _ tyId _ _ _))

pattern SimplPatSyn :: forall l p. IdP p -> GenLocated l (HsDecl p)
pattern SimplPatSyn synId <- L _ (TyClD _ (SynDecl _ (L _ synId) _ _ _))

pattern FunDecl :: forall p l. (XFunBind p p ~ NoExt, XValD p ~ NoExt, XMG p (LHsExpr p) ~ NoExt) => l -> Located (IdP p) -> SrcLoc.SrcSpan -> [LMatch p (LHsExpr p)] -> Origin -> TcEvidence.HsWrapper -> [CoreSyn.Tickish Var.Id] -> GenLocated l (HsDecl p)
pattern FunDecl declLoc funId matchesLoc funMatches mgOrigin funWrapper funTick = 
  L declLoc (ValD NoExt (FunBind NoExt funId (MG NoExt (L matchesLoc funMatches) mgOrigin) funWrapper funTick))

pattern TypeSigDecl :: forall l p. l -> [SrcLoc.Located (IdP p)] -> LHsSigWcType p -> GenLocated l (HsDecl p)
pattern TypeSigDecl declLoc funIds sigWctype <- L declLoc (SigD _ (TypeSig _ funIds sigWctype))

pattern TypeSigDeclX :: forall p l. (XSigD p ~ NoExt, XTypeSig p ~ NoExt) => l -> [Located (IdP p)] -> LHsSigWcType p -> GenLocated l (HsDecl p)
pattern TypeSigDeclX declLoc funIds sigWctype = L declLoc (SigD NoExt (TypeSig NoExt funIds sigWctype))

pattern H98Decl :: forall l pass. IdP pass -> GenLocated l (ConDecl pass)
pattern H98Decl rdrName <- L _ (ConDeclH98 _ (L _ rdrName) _ _ _ _ _)

pattern GADTDecl :: forall l pass. [Located (IdP pass)] -> GenLocated l (ConDecl pass)
pattern GADTDecl names <- L _ (ConDeclGADT _ names _ _ _ _ _ _)

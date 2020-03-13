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
        let allDecls  = hsmodDecls . unLoc . prParsedSource $ oldOrmolu
        _ormolu
          <$> execStateT
            (traverse (removeUnusedDecl unusedBindingNames) allDecls)
            (ReduceState test sourceFile oldOrmolu)

removeUnusedDecl :: [BindingName] -> LHsDecl GhcPs -> StateT ReduceState IO ()
-- TODO: iwie ist das unschön, dass hier mehr gemacht wird, als nur die value decl zu löschen
removeUnusedDecl unusedBindingNames (L declLoc decl@(FunDecl funId matchesLoc funMatches mgOrigin funWrapper funTick)) = do
  debugPrint $ "\nlooking at function binding: " ++ (showSDocUnsafe . ppr . unLoc $ funId)
  oldOrmolu <- _ormolu <$> get
  let newOrmolu =
        changeDecls
          oldOrmolu
          ( filter
                ( \(L iterLoc iterDecl) ->
                    case iterDecl of
                      SigD _ (TypeSig _ [tyFunId] _) -> 
                        ((`notElem` unusedBindingNames) . showSDocUnsafe . ppr . unLoc) tyFunId
                      _ -> iterLoc /= declLoc
                )
          )
  testAndUpdateState newOrmolu
  -- check matches
  transformDeclST unusedBindingNames 
                  (L declLoc decl)
                  (\_ _ -> 
                      let newMatches = 
                            filter (\(L _ (Match _ _ _ grhss)) -> 
                              showSDocUnsafe (pprGRHSs LambdaExpr grhss) /= "-> undefined") funMatches 
                      in return $ FunDecl funId matchesLoc newMatches mgOrigin funWrapper funTick)
removeUnusedDecl unusedBindingNames decl@(TypeSigDecl _ funIds sigWctype) = do
  debugPrint $ "\nlooking at type signature: " ++ (concatMap (showSDocUnsafe . ppr) funIds)
  transformDeclST unusedBindingNames 
                  decl 
                  (\_ _ -> 
                     let newFunIds = filter ((`notElem` unusedBindingNames) . showSDocUnsafe . ppr . unLoc) funIds
                     in  return $ TypeSigDeclX newFunIds sigWctype)
removeUnusedDecl unusedBindingNames decl@(L _ (TyClD _ _)) = do
  debugPrint "\nlooking at type class / data declaration "
  transformDeclST unusedBindingNames decl reduceConstructors
-- TODO: more fine granular handling of instance decls
removeUnusedDecl _ (L declLoc _) = do
  debugPrint "\nreducing some kind of decl"
  ReduceState _ _ oldOrmolu <- get
  let newOrmolu = changeDecls oldOrmolu (filter (\(L iterLoc _) -> iterLoc /= declLoc))
  testAndUpdateState newOrmolu

transformDeclST :: [BindingName] 
                -> LHsDecl GhcPs 
                -> ([BindingName] -> HsDecl GhcPs -> StateT ReduceState IO (HsDecl GhcPs)) 
                -> StateT ReduceState IO ()
transformDeclST unusedBindingNames (L declLoc oldDecl) transformDecl = do
  oldOrmolu <- _ormolu <$> get
  newDecl   <- transformDecl unusedBindingNames oldDecl
  let newOrmolu = changeDecls oldOrmolu 
                              (map (\(L iterLoc iterDecl) -> 
                                        if iterLoc == declLoc then L declLoc newDecl else L iterLoc iterDecl))
  testAndUpdateState newOrmolu

reduceConstructors :: [BindingName] -> HsDecl GhcPs -> StateT ReduceState IO (HsDecl GhcPs)
reduceConstructors unusedBindingNames (TyClD _ oldDecl@(DataDecl _ _ _ _ oldDataDefn)) = do
  let constructors     = dd_cons oldDataDefn
      construcorsInUse = filter (constructorIsUsed unusedBindingNames) (traceShow ("***current constr: ***" ++ concatMap (showSDocUnsafe . ppr) constructors) constructors)
  newConstructors <- traverse simplifyGADTs construcorsInUse
  return $ TyClD NoExt oldDecl{ tcdDataDefn = oldDataDefn{ dd_cons = newConstructors}}
reduceConstructors _ d = return d

simplifyGADTs :: LConDecl GhcPs -> StateT ReduceState IO (LConDecl GhcPs)
simplifyGADTs decl@(L declLoc gadtDecl@(ConDeclGADT _ _ (L forallLoc _) _ _ _ (L _ conTy) _)) = do
  liftIO $ putStrLn $ "conTy: " ++ showSDocUnsafe (ppr conTy)
  -- delete forall
  let newDecl = L declLoc gadtDecl{ con_forall = L forallLoc False}
  L l2 newDecl2 <- tryNewConDecl decl newDecl
  -- delete context
  let newDecl3 = L declLoc newDecl2{ con_mb_cxt = Nothing}
  tryNewConDecl (L l2 newDecl2) newDecl3
simplifyGADTs d = return d

tryNewConDecl :: LConDecl GhcPs -> LConDecl GhcPs -> StateT ReduceState IO (LConDecl GhcPs)
tryNewConDecl oldDecl@(L declLoc _) newDecl = do
  oldOrmolu <- _ormolu <$> get
  let oldModule = prParsedSource oldOrmolu
      newModule = everywhereT (mkT (overwriteAtLoc declLoc newDecl)) oldModule
  testAndUpdateStateFlex (oldOrmolu{ prParsedSource = newModule}) oldDecl newDecl

-- TODO: check what to do in 3rd case
constructorIsUsed :: [String] -> LConDecl GhcPs -> Bool
constructorIsUsed unusedBindingNames (H98Decl rdrName) = (showSDocUnsafe . ppr $ rdrName) `notElem` unusedBindingNames
constructorIsUsed unusedBindingNames (GADTDecl names) =
  let result = all (\(L _ rdrName) -> (showSDocUnsafe . ppr $ rdrName) `notElem` unusedBindingNames) names
   in 
     traceShow (if not result 
                  then "Is constructor " ++ unwords (map (showSDocUnsafe . ppr) names) ++ " used? " ++ show result 
                  else []) 
               result
constructorIsUsed _ _ = True

pattern FunDecl :: forall p.
                         (XFunBind p p ~ NoExt, XValD p ~ NoExt,
                          XMG p (LHsExpr p) ~ NoExt) =>
                         Located (IdP p)
                         -> SrcSpan
                         -> [LMatch p (LHsExpr p)]
                         -> Origin
                         -> HsWrapper
                         -> [Tickish Id]
                         -> HsDecl p
pattern FunDecl funId matchesLoc funMatches mgOrigin funWrapper funTick = 
  ValD NoExt (FunBind NoExt funId (MG NoExt (L matchesLoc funMatches) mgOrigin) funWrapper funTick)

pattern TypeSigDecl :: forall l p. l -> [SrcLoc.Located (IdP p)] -> LHsSigWcType p -> GenLocated l (HsDecl p)
pattern TypeSigDecl declLoc funIds sigWctype <- L declLoc (SigD _ (TypeSig _ funIds sigWctype))

pattern TypeSigDeclX :: forall p.  (XSigD p ~ NoExt, XTypeSig p ~ NoExt) => [Located (IdP p)] -> LHsSigWcType p -> HsDecl p
pattern TypeSigDeclX funIds sigWctype = SigD NoExt (TypeSig NoExt funIds sigWctype)

pattern H98Decl :: forall l pass. IdP pass -> GenLocated l (ConDecl pass)
pattern H98Decl rdrName <- L _ (ConDeclH98 _ (L _ rdrName) _ _ _ _ _)

pattern GADTDecl :: forall l pass. [Located (IdP pass)] -> GenLocated l (ConDecl pass)
pattern GADTDecl names <- L _ (ConDeclGADT _ names _ _ _ _ _ _)

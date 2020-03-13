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
            (traverse (\decl ->
                          filterUnusedDecl unusedBindingNames decl >>=
                            flip unless 
                              (tryRemoveDecl decl >>=
                                flip unless 
                                  (simplifyDecl unusedBindingNames decl))) allDecls)
            (ReduceState test sourceFile oldOrmolu)

filterUnusedDecl :: [BindingName] -> LHsDecl GhcPs -> StateT ReduceState IO Bool
filterUnusedDecl unusedBindingNames lDecl@(L _ decl) =
  if maybe False (`elem` unusedBindingNames) $ getName decl
    then tryRemoveDecl lDecl
    else return False

tryRemoveDecl :: LHsDecl GhcPs -> StateT ReduceState IO Bool
tryRemoveDecl (L declLoc _) = do
    oldOrmolu <- _ormolu <$> get
    let newOrmolu = changeDecls oldOrmolu (filter (\(L iterLoc _) -> iterLoc /= declLoc))
    testAndUpdateStateFlex newOrmolu False True

-- TODO: what other decls make sense here?
getName :: HsDecl GhcPs -> Maybe String
getName (TyClD _ decl@ClassDecl{}) = Just . showSDocUnsafe . ppr . unLoc . tcdLName $ decl
getName (TyClD _ decl@DataDecl{})  = Just . showSDocUnsafe . ppr . unLoc . tcdLName $ decl
getName (TyClD _ decl@SynDecl{})   = Just . showSDocUnsafe . ppr . unLoc . tcdLName $ decl
getName _ = Nothing

-- TODO: die Funktion hat mehr als 1 Verantwortlichkeit, unschön!
simplifyDecl :: [BindingName] -> LHsDecl GhcPs -> StateT ReduceState IO ()
simplifyDecl unusedBindingNames (L declLoc decl@(FunDecl funId matchesLoc funMatches mgOrigin funWrapper funTick)) = do
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
simplifyDecl unusedBindingNames decl@(TypeSigDecl _ funIds sigWctype) = do
  debugPrint $ "\nlooking at type signature: " ++ (concatMap (showSDocUnsafe . ppr) funIds)
  transformDeclST unusedBindingNames 
                  decl 
                  (\_ _ -> 
                     let newFunIds = filter ((`notElem` unusedBindingNames) . showSDocUnsafe . ppr . unLoc) funIds
                     in  return $ TypeSigDeclX newFunIds sigWctype)
simplifyDecl unusedBindingNames decl@(L _ (TyClD _ _)) = do
  debugPrint "\nlooking at type class / data declaration "
  transformDeclST unusedBindingNames decl reduceConstructors
simplifyDecl _ _ = return ()
-- TODO: more fine granular handling of instance decls

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

-- TODO: apply to more TyClD types
reduceConstructors :: [BindingName] -> HsDecl GhcPs -> StateT ReduceState IO (HsDecl GhcPs)
reduceConstructors unusedBindingNames (TyClD _ oldDecl@(DataDecl _ _ _ _ oldDataDefn)) = do
  let constructors     = dd_cons oldDataDefn
      newConstructors = 
        filter (constructorIsUsed unusedBindingNames) 
               (traceShow ("***current constr: ***" ++ concatMap (showSDocUnsafe . ppr) constructors) constructors)
  return $ TyClD NoExt oldDecl{ tcdDataDefn = oldDataDefn{ dd_cons = newConstructors}}
reduceConstructors _ d = return d

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

pattern TypeSigDecl :: SrcSpan -> [Located (IdP GhcPs)] -> LHsSigWcType GhcPs -> LHsDecl GhcPs
pattern TypeSigDecl declLoc funIds sigWctype <- L declLoc (SigD _ (TypeSig _ funIds sigWctype))

pattern TypeSigDeclX :: [Located (IdP GhcPs)] -> LHsSigWcType GhcPs -> HsDecl GhcPs
pattern TypeSigDeclX funIds sigWctype = SigD NoExt (TypeSig NoExt funIds sigWctype)

pattern H98Decl :: IdP GhcPs -> LConDecl GhcPs
pattern H98Decl rdrName <- L _ (ConDeclH98 _ (L _ rdrName) _ _ _ _ _)

pattern GADTDecl :: [Located (IdP GhcPs)] -> LConDecl GhcPs
pattern GADTDecl names <- L _ (ConDeclGADT _ names _ _ _ _ _ _)
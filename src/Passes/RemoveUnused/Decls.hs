module Passes.RemoveUnused.Decls where

import Control.Monad.State.Strict
import qualified Data.Text as T
import HsSyn
import SrcLoc
import TcEvidence
import CoreSyn
import Var
import Ormolu.Parser.Result as OPR (ParseResult, prParsedSource)
import Ormolu.Printer (printModule)
import Outputable (showSDocUnsafe)
import BasicTypes
import Types
import Util

-- | run ghc with -Wunused-binds -ddump-json and delete decls that are mentioned there
reduce :: FilePath -> FilePath -> OPR.ParseResult -> IO OPR.ParseResult
reduce test sourceFile oldOrmolu = do
  putStrLn "\n***Removing unused declarations***"
  debugPrint $ "Size of old ormolu: " ++ (show . T.length $ printModule oldOrmolu)
  let allDecls  = hsmodDecls . unLoc . prParsedSource $ oldOrmolu
      startState = ReduceState test sourceFile oldOrmolu
  runGhc sourceFile oldOrmolu Binds
    >>= \case
      Nothing -> _ormolu <$> execStateT (traverse removeDecl allDecls) startState
      Just unusedBindingNames ->
        _ormolu <$> 
          execStateT
            (traverse (\decl -> removeUnusedDecl unusedBindingNames decl
                                >>= flip unless (removeDecl decl
                                >>= flip unless (simplifyDecl unusedBindingNames decl))) 
                      allDecls)
            startState

-- | for FunBinds: also delete the type signature
removeUnusedDecl :: [BindingName] -> LHsDecl GhcPs -> StateT ReduceState IO Bool
removeUnusedDecl unusedBindingNames (L declLoc (SimplFun funId)) 
  | lshow funId `elem` unusedBindingNames = do
    oldOrmolu <- _ormolu <$> get
    let newOrmolu =
          changeDecls oldOrmolu 
                      (filter (\(L iterLoc iterDecl) -> 
                        case iterDecl of
                          SimplSig tyFunId -> 
                            ((`notElem` unusedBindingNames) . lshow) tyFunId
                          _ -> iterLoc /= declLoc))
    testAndUpdateStateFlex newOrmolu False True
  | otherwise = return False
removeUnusedDecl unusedBindingNames lDecl@(L _ decl) =
  if maybe False ((`elem` unusedBindingNames) . lshow) . getName $ decl
    then removeDecl lDecl
    else return False

removeDecl :: LHsDecl GhcPs -> StateT ReduceState IO Bool
removeDecl (L declLoc _) = do
  oldOrmolu <- _ormolu <$> get
  let newOrmolu = changeDecls oldOrmolu (filter (\(L iterLoc _) -> iterLoc /= declLoc))
  testAndUpdateStateFlex newOrmolu False True

simplifyDecl :: [BindingName] -> LHsDecl GhcPs -> StateT ReduceState IO ()
simplifyDecl _ oldDecl@(L _ (FunDecl lFunId matchesLoc funMatches mgOrigin funWrapper funTick)) = do
  -- check matches
  let newMatches = 
        filter (\(L _ (Match _ _ _ grhss)) -> showSDocUnsafe (pprGRHSs LambdaExpr grhss) /= "-> undefined") 
               funMatches 
  void $ tryNewValue oldDecl (FunDecl lFunId matchesLoc newMatches mgOrigin funWrapper funTick)
simplifyDecl unusedBindingNames decl@(TypeSigDecl _ funIds sigWctype) = do
  let newFunIds = filter ((`notElem` unusedBindingNames) . lshow) funIds
  void $ tryNewValue decl (TypeSigDeclX newFunIds sigWctype)
simplifyDecl unusedBindingNames ldecl@(L _ decl@(TyClD _ _)) =
  void $ tryNewValue ldecl (reduceConstructors unusedBindingNames decl)
simplifyDecl _ _ = return ()

-- TODO: apply to more TyClD types
reduceConstructors :: [BindingName] -> HsDecl GhcPs -> HsDecl GhcPs
reduceConstructors unusedBindingNames (TyClD _ oldDecl@(DataDecl _ _ _ _ oldDataDefn)) =
  let constructors    = dd_cons oldDataDefn
      newConstructors = filter (isConstructorUsed unusedBindingNames) constructors
  in  TyClD NoExt oldDecl{ tcdDataDefn = oldDataDefn{ dd_cons = newConstructors}}
reduceConstructors _ d = d

isConstructorUsed :: [String] -> LConDecl GhcPs -> Bool
isConstructorUsed unusedBindingNames (H98Decl rdrName) = oshow rdrName `notElem` unusedBindingNames
isConstructorUsed unusedBindingNames (GADTDecl names) =
  all (\(L _ rdrName) -> oshow rdrName `notElem` unusedBindingNames) names
isConstructorUsed _ _ = True

-- TODO: what other decls make sense here?
getName :: HsDecl GhcPs -> Maybe (Located (IdP GhcPs))
getName (TyClD _ decl@ClassDecl{}) = Just $ tcdLName decl
getName (TyClD _ decl@DataDecl{})  = Just $ tcdLName decl
getName (TyClD _ decl@SynDecl{})   = Just $ tcdLName decl
getName (SimplFun funId)           = Just funId
getName (SimplSig funId)           = Just funId
getName _ = Nothing

-- getName
pattern SimplSig, SimplFun :: Located (IdP GhcPs) -> HsDecl GhcPs
pattern SimplSig lFunId <- SigD _ (TypeSig _ [lFunId] _)
pattern SimplFun lFunId <- ValD _ (FunBind _ lFunId _ _ _)


-- simplifyDecl
pattern FunDecl :: Located (IdP GhcPs) -> SrcSpan -> [LMatch GhcPs (LHsExpr GhcPs)] -> Origin -> HsWrapper -> [Tickish Id] -> HsDecl GhcPs
pattern FunDecl lFunId matchesLoc funMatches mgOrigin funWrapper funTick = 
  ValD NoExt (FunBind NoExt lFunId (MG NoExt (L matchesLoc funMatches) mgOrigin) funWrapper funTick)

pattern TypeSigDecl :: SrcSpan -> [Located (IdP GhcPs)] -> LHsSigWcType GhcPs -> LHsDecl GhcPs
pattern TypeSigDecl declLoc funIds sigWctype <- L declLoc (SigD _ (TypeSig _ funIds sigWctype))

pattern TypeSigDeclX :: [Located (IdP GhcPs)] -> LHsSigWcType GhcPs -> HsDecl GhcPs
pattern TypeSigDeclX funIds sigWctype = SigD NoExt (TypeSig NoExt funIds sigWctype)


-- isConstructorUsed
pattern H98Decl :: IdP GhcPs -> LConDecl GhcPs
pattern H98Decl rdrName <- L _ (ConDeclH98 _ (L _ rdrName) _ _ _ _ _)

pattern GADTDecl :: [Located (IdP GhcPs)] -> LConDecl GhcPs
pattern GADTDecl names <- L _ (ConDeclGADT _ names _ _ _ _ _ _)
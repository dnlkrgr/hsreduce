module Reduce.Passes.RemoveUnused.Decls where

import Data.Foldable
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
import Reduce.Types
import Reduce.Util

-- | run ghc with -Wunused-binds -ddump-json and delete decls that are mentioned there
reduce :: OPR.ParseResult -> ReduceM OPR.ParseResult
reduce oldOrmolu = do
  liftIO $ putStrLn "\n***Removing unused declarations***"
  debugPrint $ "Size of old ormolu: " ++ (show . T.length $ printModule oldOrmolu)
  let allDecls   = hsmodDecls . unLoc . prParsedSource $ oldOrmolu
  runGhc oldOrmolu Binds
    >>= \case
      Nothing -> do
            traverse_ (\dcl -> rmvDecl dcl 
                               >>= flip unless (rmvBinds Nothing dcl)) allDecls
            _ormolu <$> get
      Just unusedBindingNames -> do
            traverse_ (\dcl -> rmvUnusedDecl unusedBindingNames dcl
                               >>= flip unless (rmvDecl dcl
                               >>= flip unless (rmvBinds (Just unusedBindingNames) dcl))) 
                      allDecls
            _ormolu <$> get

-- | rmv unused decls whole
-- for FunBinds: also delete the type signature
rmvUnusedDecl :: [BindingName] -> LHsDecl GhcPs -> ReduceM Bool
rmvUnusedDecl unusedBindingNames (L declLoc (SimplFun funId)) 
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
rmvUnusedDecl unusedBindingNames lDecl@(L _ decl) =
  if maybe False ((`elem` unusedBindingNames) . lshow) . getName $ decl
    then rmvDecl lDecl
    else return False

-- | rmv decls whole
rmvDecl :: LHsDecl GhcPs -> ReduceM Bool
rmvDecl (L declLoc _) = do
  oldOrmolu <- _ormolu <$> get
  let newOrmolu = changeDecls oldOrmolu (filter (\(L iterLoc _) -> iterLoc /= declLoc))
  testAndUpdateStateFlex newOrmolu False True

-- | rmv unused parts of decls
rmvBinds :: Maybe [BindingName] -> LHsDecl GhcPs -> ReduceM ()
rmvBinds (Just bns) dcl@(TypeSigDecl _ ids swt) = do
  let newFunIds = filter ((`notElem` bns) . lshow) ids
  void $ tryNewValue dcl (TypeSigDeclX newFunIds swt)
rmvBinds Nothing dcl@(TypeSigDecl _ ids swt) =
  foldM_ (\nDcl fId -> 
           let nIds = filter (/= fId) ids 
           in tryNewValue nDcl (TypeSigDeclX nIds swt)) 
         dcl 
         ids
rmvBinds _ oldDecl@(L _ (FunDecl fid loc mtchs mo fw ft)) = do
  let nMtchs = 
        filter (\(L _ (Match _ _ _ grhss)) -> 
          showSDocUnsafe (pprGRHSs LambdaExpr grhss) /= "-> undefined") mtchs 
  void $ tryNewValue oldDecl (FunDecl fid loc nMtchs mo fw ft)
rmvBinds mBns ldcl@(L _ (TyClD _ _)) =
  rmvCons mBns ldcl
rmvBinds _ _ = return ()

-- | rmv unused constructors
-- TODO: apply to more TyClD types
rmvCons :: Maybe [BindingName] -> LHsDecl GhcPs -> ReduceM ()
rmvCons (Just unusedBindingNames) ldcl@(L _ (TyClD _ oldDecl@(DataDecl _ _ _ _ oldDataDefn))) = do
  let constructors    = dd_cons oldDataDefn
      newConstructors = filter (isConstructorUsed unusedBindingNames) constructors
  void $ tryNewValue ldcl $ TyClD NoExt oldDecl{ tcdDataDefn = oldDataDefn{ dd_cons = newConstructors}}
rmvCons Nothing ldcl@(L _ (TyClD _ oldDecl@(DataDecl _ _ _ _ oldDataDefn))) = do
  let constructors    = dd_cons oldDataDefn
  foldM_ (\dcl cons -> 
           let newCons = filter ((== cons2String cons) . cons2String) constructors
           in tryNewValue dcl $ TyClD NoExt oldDecl{ tcdDataDefn = oldDataDefn{ dd_cons = newCons}})
         ldcl 
         constructors
rmvCons _ _ = return ()

-- Left:  H98
-- Right: GADT
cons2String :: LConDecl GhcPs -> Either String [String]
cons2String (H98Decl rdrName) = Left $ oshow rdrName
cons2String (GADTDecl names) = Right $ map (oshow . unLoc) names
cons2String _ = Left ""

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
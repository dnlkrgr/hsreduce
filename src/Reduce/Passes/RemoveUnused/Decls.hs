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
rmvUnusedDecl unusedBindingNames (L declLoc (SimplFunP funId)) 
  | lshow funId `elem` unusedBindingNames =
          changeDecls 
            (filter (\(L iterLoc iterDecl) -> 
              case iterDecl of
                SimplSigP tyFunId -> 
                  ((`notElem` unusedBindingNames) . lshow) tyFunId
                _ -> iterLoc /= declLoc))
          . _ormolu 
          <$> get
    >>= testAndUpdateStateFlex False True
  | otherwise = return False
rmvUnusedDecl unusedBindingNames lDecl@(L _ decl) =
  if maybe False ((`elem` unusedBindingNames) . oshow) . getName $ decl
    then rmvDecl lDecl
    else return False

-- | rmv decls whole
rmvDecl :: LHsDecl GhcPs -> ReduceM Bool
rmvDecl (L declLoc _) =
  changeDecls (filter (\(L iterLoc _) -> iterLoc /= declLoc)) . _ormolu <$> get
  >>= testAndUpdateStateFlex False True 

-- | rmv unused parts of decls
rmvBinds :: Maybe [BindingName] -> LHsDecl GhcPs -> ReduceM ()
rmvBinds (Just bns) dcl@(TypeSigDeclP _ ids swt) = do
  liftIO $ putStrLn $ "--- Just ub, TypeSigDeclP" ++ oshow dcl
  let newFunIds = filter ((`notElem` bns) . lshow) ids
  void $ tryNewValue dcl (TypeSigDeclX newFunIds swt)
rmvBinds Nothing dcl@(TypeSigDeclP _ ids swt) = do
  liftIO $ putStrLn $ "--- Nothing TypeSigDeclP" ++ oshow dcl
  void $ tryRemoveEach (/=) (flip TypeSigDeclX swt) dcl ids
rmvBinds _ oldDecl@(L _ (FunDeclP fid loc mtchs mo fw ft)) = do
  liftIO $ putStrLn $ "--- fundeclp" ++ oshow fid
  let nMtchs = 
        filter (\(L _ (Match _ _ _ grhss)) -> 
          showSDocUnsafe (pprGRHSs LambdaExpr grhss) /= "-> undefined") mtchs 
  void $ tryNewValue oldDecl (FunDeclP fid loc nMtchs mo fw ft)
rmvBinds mBns ldcl@(L _ (TyClD _ _)) = do
  liftIO $ putStrLn $ "--- tycld" ++ lshow ldcl
  rmvCons mBns ldcl
rmvBinds _ _ = return ()

-- | rmv unused constructors
-- TODO: apply to more TyClD types
rmvCons :: Maybe [BindingName] -> LHsDecl GhcPs -> ReduceM ()
--rmvCons (Just unusedBindingNames) ldcl@(L _ (TyClD _ oldDecl@(DataDecl _ _ _ _ oldDataDefn))) = do
--  let constructors    = dd_cons oldDataDefn
--      newConstructors = filter (isConstructorUsed unusedBindingNames) constructors
--  liftIO $ putStrLn $ "!!! newConstructors: " ++ concatMap ((++ " ") . lshow) newConstructors
--  void $ tryNewValue ldcl $ TyClD NoExt oldDecl{ tcdDataDefn = oldDataDefn{ dd_cons = newConstructors}}
rmvCons _ ldcl@(L _ (TyClD _ oldDecl@(DataDecl _ _ _ _ oldDataDefn))) = do
  let constructors    = dd_cons oldDataDefn
  void $ tryRemoveEach (\c -> (/= cons2String c) . cons2String) 
                       (\l -> TyClD NoExt oldDecl {tcdDataDefn = oldDataDefn {dd_cons = l}})
                       ldcl
                       constructors
rmvCons _ _ = return ()

-- Left:  H98
-- Right: GADT
cons2String :: LConDecl GhcPs -> Either String [String]
cons2String (H98DeclP rdrName) = Left $ oshow rdrName
cons2String (GADTDeclP names) = Right $ map (oshow . unLoc) names
cons2String _ = Left ""

isConstructorUsed :: [String] -> LConDecl GhcPs -> Bool
isConstructorUsed unusedBindingNames (H98DeclP rdrName) = oshow rdrName `notElem` unusedBindingNames
isConstructorUsed unusedBindingNames (GADTDeclP names) =
  all (\(L _ rdrName) -> oshow rdrName `notElem` unusedBindingNames) names
isConstructorUsed _ _ = True

-- TODO: what other decls make sense here?
getName :: HsDecl GhcPs -> Maybe (IdP GhcPs)
getName (TyClD _ d)      = Just . tcdName $ d
getName (SimplFunP funId) = Just . unLoc $ funId
getName (SimplSigP funId) = Just . unLoc $ funId
getName _ = Nothing

-- getName
pattern SimplSigP, SimplFunP :: Located (IdP GhcPs) -> HsDecl GhcPs
pattern SimplSigP lFunId <- SigD _ (TypeSig _ [lFunId] _)
pattern SimplFunP lFunId <- ValD _ (FunBind _ lFunId _ _ _)


-- simplifyDecl
pattern FunDeclP :: Located (IdP GhcPs) -> SrcSpan -> [LMatch GhcPs (LHsExpr GhcPs)] -> Origin -> HsWrapper -> [Tickish Id] -> HsDecl GhcPs
pattern FunDeclP lFunId matchesLoc funMatches mgOrigin funWrapper funTick = 
  ValD NoExt (FunBind NoExt lFunId (MG NoExt (L matchesLoc funMatches) mgOrigin) funWrapper funTick)

pattern TypeSigDeclP :: SrcSpan -> [Located (IdP GhcPs)] -> LHsSigWcType GhcPs -> LHsDecl GhcPs
pattern TypeSigDeclP declLoc funIds sigWctype <- L declLoc (SigD _ (TypeSig _ funIds sigWctype))

pattern TypeSigDeclX :: [Located (IdP GhcPs)] -> LHsSigWcType GhcPs -> HsDecl GhcPs
pattern TypeSigDeclX funIds sigWctype = SigD NoExt (TypeSig NoExt funIds sigWctype)


-- isConstructorUsed
pattern H98DeclP :: IdP GhcPs -> LConDecl GhcPs
pattern H98DeclP rdrName <- L _ (ConDeclH98 _ (L _ rdrName) _ _ _ _ _)

pattern GADTDeclP :: [Located (IdP GhcPs)] -> LConDecl GhcPs
pattern GADTDeclP names <- L _ (ConDeclGADT _ names _ _ _ _ _ _)
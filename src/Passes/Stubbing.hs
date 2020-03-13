module Passes.Stubbing
  ( reduce,
  )
where

import Control.Monad.State.Strict
import qualified Data.Text as T
import HsSyn
import Ormolu.Config (defaultConfig)
import Ormolu.Parser (parseModule)
import Ormolu.Parser.Result as OPR (ParseResult, prParsedSource)
import Ormolu.Printer (printModule)
import SrcLoc
import Types
import Util

-- | run a pass on the old module and return the new one if it's interesting
reduce :: FilePath -> FilePath -> OPR.ParseResult -> IO OPR.ParseResult
reduce test sourceFile oldOrmolu = do
  putStrLn "\n***Stubbing expressions***"
  debugPrint $ "Size of old ormolu: " ++ (show . T.length $ printModule oldOrmolu)
  let oldModule = prParsedSource oldOrmolu
  getUndefined
    >>= \case
      Nothing -> return oldOrmolu
      Just myUndefined ->
        _ormolu
          <$> execStateT
            (everywhereM (mkM (expr2Undefined myUndefined) >=> mkM simplifyType >=> mkM simplifyGADTs >=> mkM deleteWhereClause) oldModule)
            (ReduceState test sourceFile oldOrmolu)

-- | change an expression to `undefined`
expr2Undefined :: HsExpr GhcPs -> LHsExpr GhcPs -> StateT ReduceState IO (LHsExpr GhcPs)
expr2Undefined myUndefined oldExpr@(L loc _) = do
  oldOrmolu <- _ormolu <$> get
  let oldModule = prParsedSource oldOrmolu
      newExpr = L loc myUndefined
      newModule = 
        everywhereT (mkT (overwriteAtLoc loc newExpr)) oldModule
      newOrmolu = oldOrmolu {prParsedSource = newModule}
  testAndUpdateStateFlex newOrmolu oldExpr newExpr

simplifyType :: LHsType GhcPs -> StateT ReduceState IO (LHsType GhcPs)
simplifyType t@(L _ TupleType) = return t
simplifyType t@(L _ (HsFunTy NoExt (L _ TupleType) (L _ TupleType))) = return t
simplifyType oldType@(L loc (HsForAllTy _ _ (L _ body))) = do
  let newType = L loc body
  tryNewValue oldType newType
simplifyType oldType@(L loc (HsQualTy _ _ (L _ body))) = do
  let newType = L loc body
  tryNewValue oldType newType
simplifyType oldType@(L loc _) = tryNewValue oldType (LTupleType loc)

pattern LTupleType :: SrcSpan -> LHsType GhcPs
pattern LTupleType loc = L loc (HsTupleTy NoExt HsBoxedTuple [])
pattern TupleType :: HsType GhcPs
pattern TupleType <- HsTupleTy NoExt HsBoxedTuple []

simplifyGADTs :: LConDecl GhcPs -> StateT ReduceState IO (LConDecl GhcPs)
simplifyGADTs decl@(L declLoc gadtDecl@(ConDeclGADT _ _ (L forallLoc _) _ _ _ _ _)) = do
  -- delete forall
  let newDecl = L declLoc gadtDecl{ con_forall = L forallLoc False}
  L _ newDecl2 <- tryNewValue decl newDecl
  -- delete context
  let newDecl3 = L declLoc newDecl2{ con_mb_cxt = Nothing}
  tryNewValue (L declLoc newDecl2) newDecl3
simplifyGADTs d = return d

deleteWhereClause :: LHsLocalBinds GhcPs -> StateT ReduceState IO (LHsLocalBinds GhcPs)
deleteWhereClause e@(L _ (EmptyLocalBinds _)) = return e
deleteWhereClause oldClause@(L loc _) = tryNewValue oldClause (L loc (EmptyLocalBinds NoExt))

-- | getting undefined as an expression
getUndefined :: IO (Maybe (HsExpr GhcPs))
getUndefined =
  snd <$> parseModule defaultConfig "" "x = undefined"
    >>= \case
      Left _ -> return Nothing
      Right oldOrmolu ->
        case unLoc . head . hsmodDecls . unLoc $ prParsedSource oldOrmolu of
          FunBindGRHS grhs -> do
            let GRHS _ _ (L _ body) = unLoc . head $ grhssGRHSs grhs
            return $ Just body
          _ -> return Nothing

pattern FunBindGRHS :: forall p. GRHSs p (LHsExpr p) -> HsDecl p
pattern FunBindGRHS grhs <- (ValD _ (FunBind _ _ (MG _ (L _ [L _ (Match _ _ _ grhs)]) _) _ _))

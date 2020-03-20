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
import Outputable


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
            (everywhereM (    mkM (expr2Undefined myUndefined) 
                          >=> mkM simplifyLGRHS
                          >=> mkM simplifyExpr
                          >=> mkM deleteUndefinedMatches
                          >=> mkM simplifyType 
                          >=> mkM simplifyGADTs 
                          >=> mkM deleteWhereClause)
                         oldModule)
            (ReduceState test sourceFile oldOrmolu)

-- | change an expression to `undefined`
expr2Undefined :: HsExpr GhcPs -> LHsExpr GhcPs -> StateT ReduceState IO (LHsExpr GhcPs)
expr2Undefined myUndefined lexp@(L _ expr) 
  | oshow expr == "undefined" = return lexp
  | otherwise = tryNewValue lexp myUndefined

simplifyType :: LHsType GhcPs -> StateT ReduceState IO (LHsType GhcPs)
simplifyType t@(L _ TupleType) = return t
simplifyType t@(L _ (HsFunTy NoExt (L _ TupleType) (L _ TupleType))) = return t
simplifyType oldType@(ForallType body) = tryNewValue oldType body
simplifyType oldType@(QualType body)   = tryNewValue oldType body
simplifyType oldType = tryNewValue oldType UnitType

simplifyGADTs :: LConDecl GhcPs -> StateT ReduceState IO (LConDecl GhcPs)
simplifyGADTs decl@(L declLoc gadtDecl@(ConDeclGADT _ _ (L forallLoc _) _ _ _ _ _)) = do
  let newDecl = gadtDecl{ con_forall = L forallLoc False} -- delete forall
  L _ newDecl2 <- tryNewValue decl newDecl
  let newDecl3 = newDecl2{ con_mb_cxt = Nothing} -- delete context
  tryNewValue (L declLoc newDecl2) newDecl3
simplifyGADTs d = return d

deleteWhereClause :: LHsLocalBinds GhcPs -> StateT ReduceState IO (LHsLocalBinds GhcPs)
deleteWhereClause e@(L _ (EmptyLocalBinds _)) = return e
deleteWhereClause oldClause = tryNewValue oldClause (EmptyLocalBinds NoExt)

deleteUndefinedMatches :: Located [LMatch GhcPs (LHsExpr GhcPs)]
                       -> StateT ReduceState IO (Located [LMatch GhcPs (LHsExpr GhcPs)])
deleteUndefinedMatches (L l []) = return $ L l []
deleteUndefinedMatches (L l mtchs) = do
  let nMtchs = 
        filter (\(L _ (Match _ _ _ grhss)) -> 
          showSDocUnsafe (pprGRHSs LambdaExpr grhss) /= "-> undefined") mtchs 
  tryNewValue (L l mtchs) nMtchs

simplifyExpr :: LHsExpr GhcPs -> StateT ReduceState IO (LHsExpr GhcPs)
simplifyExpr lexpr@(SingleCase _ body) = tryNewValue lexpr body
simplifyExpr lexpr@(L _ (HsIf _ _ _ (L _ ls) (L _ rs)))
  | oshow ls == "undefined" = tryNewValue lexpr rs
  | oshow rs == "undefined" = tryNewValue lexpr ls
simplifyExpr e = return e

simplifyLGRHS :: LGRHS GhcPs (LHsExpr GhcPs) -> StateT ReduceState IO (LGRHS GhcPs (LHsExpr GhcPs))
simplifyLGRHS lgrhs@(L _ (GRHS _ _ body)) = tryNewValue lgrhs (GRHS NoExt [] body)
simplifyLGRHS lgrhs = return lgrhs


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

pattern FunBindGRHS :: GRHSs GhcPs (LHsExpr GhcPs) -> HsDecl GhcPs
pattern FunBindGRHS grhs <- (ValD _ (FunBind _ _ (MG _ (L _ [L _ (Match _ _ _ grhs)]) _) _ _))

pattern ForallType, QualType :: HsType GhcPs -> LHsType GhcPs
pattern ForallType body <-  L _ (HsForAllTy _ _ (L _ body))
pattern QualType   body <-  L _ (HsQualTy _ _ (L _ body))

pattern UnitType, TupleType :: HsType GhcPs
pattern UnitType = HsTupleTy NoExt HsBoxedTuple []
pattern TupleType <- HsTupleTy NoExt HsBoxedTuple []

pattern SingleCase :: SrcSpan -> HsExpr GhcPs -> LHsExpr GhcPs
pattern SingleCase l body <- 
  L l 
    (HsCase _ 
            _ 
            (MG _ 
                (L _ 
                   [L _ 
                      (Match _ 
                             _ 
                             _ 
                             (GRHSs _ 
                                    [L _ 
                                       (GRHS _ 
                                             [] 
                                             (L _ body))] 
                                    (L _ (EmptyLocalBinds _))))]) 
                _))
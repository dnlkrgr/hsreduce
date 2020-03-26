module Reduce.Passes.Stubbing (reduce) where

import Data.List
import Control.Monad.State.Strict
import qualified Data.Text as T
import HsSyn
import Ormolu.Config (defaultConfig)
import Ormolu.Parser (parseModule)
import Ormolu.Parser.Result as OPR (ParseResult, prParsedSource)
import Ormolu.Printer (printModule)
import SrcLoc
import Outputable

import Reduce.Types
import Reduce.Util


-- | run a pass on the old module and return the new one if it's interesting
reduce :: OPR.ParseResult -> ReduceM OPR.ParseResult
reduce oldOrmolu = do
  liftIO $ putStrLn "\n***Stubbing expressions***"
  liftIO $ debugPrint $ "Size of old ormolu: " ++ (show . T.length $ printModule oldOrmolu)
  let oldModule = prParsedSource oldOrmolu
  getUndefined
    >>= \case
      Nothing -> return oldOrmolu
      Just myUndefined -> do
            newModule <-  everywhereM 
                            (mkM (expr2Undefined myUndefined) 
                                 >=> mkM deleteUndefinedMatches
                                 >=> mkM simplifyMatch
                                 >=> mkM simplifyLGRHS
                                 >=> mkM simplifyLocalBinds
                                 >=> mkM simplifyExpr
                                 >=> mkM simplifyType 
                                 >=> mkM simplifyGADTs 
                                 >=> mkM deleteWhereClause)
                                 oldModule
            return oldOrmolu { prParsedSource = newModule }

-- | change an expression to `undefined`
expr2Undefined :: HsExpr GhcPs -> LHsExpr GhcPs -> ReduceM (LHsExpr GhcPs)
expr2Undefined myUndefined lexp@(L _ expr) 
  | oshow expr == "undefined" = return lexp
  | otherwise = tryNewValue lexp myUndefined

simplifyType :: LHsType GhcPs -> ReduceM (LHsType GhcPs)
simplifyType t@(L _ UnitTypeP) = return t
simplifyType t@(L _ (HsFunTy NoExt (L _ UnitTypeP) (L _ UnitTypeP))) = return t
simplifyType oldType@(ForallTypeP body) = tryNewValue oldType body
simplifyType oldType@(QualTypeP body)   = tryNewValue oldType body
simplifyType oldType = tryNewValue oldType UnitTypeP

simplifyGADTs :: LConDecl GhcPs -> ReduceM (LConDecl GhcPs)
simplifyGADTs decl@(L declLoc gadtDecl@(ConDeclGADT _ _ (L forallLoc _) _ _ _ _ _)) = do
  let newDecl = gadtDecl{ con_forall = L forallLoc False} -- delete forall
  L _ newDecl2 <- tryNewValue decl newDecl
  let newDecl3 = newDecl2{ con_mb_cxt = Nothing} -- delete context
  tryNewValue (L declLoc newDecl2) newDecl3
simplifyGADTs d = return d

simplifyLocalBinds :: Located (HsLocalBindsLR GhcPs GhcPs) -> ReduceM (Located (HsLocalBindsLR GhcPs GhcPs))
simplifyLocalBinds hvb@(L _ (HsValBinds _ (ValBinds _ binds sigs))) =
  tryRemoveEach (\l -> (/= getLoc l) . getLoc)
                (\ns -> HsValBinds NoExt (ValBinds NoExt binds ns))
                hvb 
                sigs
simplifyLocalBinds lb = return lb

deleteWhereClause :: LHsLocalBinds GhcPs -> ReduceM (LHsLocalBinds GhcPs)
deleteWhereClause e@(L _ (EmptyLocalBinds _)) = return e
deleteWhereClause oldClause = tryNewValue oldClause (EmptyLocalBinds NoExt)

-- TODO: this doesn't fit, this deletes the WHOLE match if only one grhs is undefined
deleteUndefinedMatches :: Located [LMatch GhcPs (LHsExpr GhcPs)]
                       -> ReduceM (Located [LMatch GhcPs (LHsExpr GhcPs)])
deleteUndefinedMatches (L l []) = return $ L l []
deleteUndefinedMatches (L l mtchs) = do
  let nMtchs = 
        filter (\(L _ (Match _ _ _ grhss@(GRHSs _ _ _))) -> 
                 showSDocUnsafe (pprGRHSs LambdaExpr grhss) /= "-> undefined")
               mtchs 
  L _ tmpMtchs <- tryNewValue (L l mtchs) nMtchs
  let nMtchs2 = 
        filter (\(L _ (Match _ _ _ (GRHSs _ grhs _))) -> 
               not 
                 (all 
                   (("undefined" `isSubsequenceOf`) 
                   . showSDocUnsafe 
                   . pprGRHS LambdaExpr 
                   . unLoc) 
                   grhs))
               tmpMtchs 
  tryNewValue (L l tmpMtchs) nMtchs2

-- BUG: some GRHS are deleted even though the test case is not interesting thereafter!
simplifyMatch :: LMatch GhcPs (LHsExpr GhcPs) -> ReduceM (LMatch GhcPs (LHsExpr GhcPs))
simplifyMatch lm@(L _ (Match _ _ _ (GRHSs _ [] _))) = return lm
simplifyMatch lm@(L _ (Match _ _ _ (GRHSs _ grhss lb))) =
  foldM (\(L _ m@(MatchP iterGRHSs _)) (L l _) -> do
          let newGRHSs = filter ((/= l) . getLoc) iterGRHSs
          case newGRHSs of
            [] -> return $ L l m
            _  -> tryNewValue (L l m) (m { m_grhss = GRHSs NoExt newGRHSs lb })
        ) 
        lm
        (reverse grhss) -- reverse because the lower have to be tried first
simplifyMatch m = return m

pattern MatchP ::  [LGRHS GhcPs (LHsExpr GhcPs)] -> LHsLocalBinds GhcPs -> Match GhcPs (LHsExpr GhcPs)
pattern MatchP grhss binds <- Match _ _ _ (GRHSs _ grhss binds)

simplifyLGRHS :: LGRHS GhcPs (LHsExpr GhcPs) -> ReduceM (LGRHS GhcPs (LHsExpr GhcPs))
simplifyLGRHS lgrhs@(L _ (GRHS _ [] _))   = return lgrhs
simplifyLGRHS lgrhs@(L _ (GRHS _ _ body)) = tryNewValue lgrhs (GRHS NoExt [] body)
simplifyLGRHS lgrhs = return lgrhs

simplifyExpr :: LHsExpr GhcPs -> ReduceM (LHsExpr GhcPs)
simplifyExpr lexpr@(SingleCase _ body) = tryNewValue lexpr body
simplifyExpr lexpr@(L _ (HsIf _ _ _ (L _ ls) (L _ rs)))
  | oshow ls == "undefined" = tryNewValue lexpr rs
  | oshow rs == "undefined" = tryNewValue lexpr ls
simplifyExpr e = return e

-- | getting undefined as an expression
getUndefined :: ReduceM (Maybe (HsExpr GhcPs))
getUndefined =
  snd <$> parseModule defaultConfig "" "x = undefined"
    >>= \case
      Left _ -> return Nothing
      Right oldOrmolu ->
        case unLoc . head . hsmodDecls . unLoc $ prParsedSource oldOrmolu of
          FunBindGRHSP grhs -> do
            let GRHS _ _ (L _ body) = unLoc . head $ grhssGRHSs grhs
            return $ Just body
          _ -> return Nothing

pattern FunBindGRHSP :: GRHSs GhcPs (LHsExpr GhcPs) -> HsDecl GhcPs
pattern FunBindGRHSP grhs <- (ValD _ (FunBind _ _ (MG _ (L _ [L _ (Match _ _ _ grhs)]) _) _ _))

pattern ForallTypeP, QualTypeP :: HsType GhcPs -> LHsType GhcPs
pattern ForallTypeP body <-  L _ (HsForAllTy _ _ (L _ body))
pattern QualTypeP   body <-  L _ (HsQualTy _ _ (L _ body))

pattern UnitTypeP :: HsType GhcPs
pattern UnitTypeP = HsTupleTy NoExt HsBoxedTuple []

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
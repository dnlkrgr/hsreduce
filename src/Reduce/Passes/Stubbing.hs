module Reduce.Passes.Stubbing (reduce) where

import Debug.Trace
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
reduce :: OPR.ParseResult -> R OPR.ParseResult
reduce oldOrmolu = do
  liftIO $ putStrLn "\n***Stubbing expressions***"
  liftIO $ debugPrint $ "Size of old ormolu: " ++ (show . T.length $ printModule oldOrmolu)
  let oldModule = prParsedSource oldOrmolu
  getUndefined
    >>= \case
      Nothing -> return oldOrmolu
      Just myUndefined -> do
        newModule <-  everywhereM
                             ( mkM (traceShow "expr2Undefined" (try (expr2Undefined myUndefined))) )
                          -- >=> mkM (traceShow "matchDelRhsUndef" (try matchDelRhsUndef))
                          -- >=> mkM (traceShow "matchDelIfUndefAnywhere" (try matchDelIfUndefAnywhere))
                          -- >=> mkM (traceShow "simplifyMatch" simplifyMatch)
                          -- >=> mkM (traceShow "simplifyLGRHS" (try simplifyLGRHS))
                          -- >=> mkM (traceShow "filterLocalBindSigs" filterLocalBindSigs)
                          -- >=> mkM (traceShow "simplifyExpr" (try simplifyExpr))
                          -- >=> mkM (traceShow "simplifyType" (try simplifyType))
                          -- >=> mkM (traceShow "gadtDelForall" (try gadtDelForall))
                          -- >=> mkM (traceShow "gadtDelCtxt" (try gadtDelCtxt))
                          -- >=> mkM (traceShow "deleteWhereClause" (try deleteWhereClause)))
                        oldModule
        return oldOrmolu { prParsedSource = newModule }

type LLocalBind = Located (HsLocalBindsLR GhcPs GhcPs)


filterLocalBindSigs :: LLocalBind -> R LLocalBind
filterLocalBindSigs = 
  reduceListOfSubelements lvalBinds2SrcSpans deleteLocalBindSig
  where 
    lvalBinds2SrcSpans =
      \case
        L _ (HsValBinds _ (ValBinds _ _ sigs)) -> map getLoc sigs
        _ -> []
    deleteLocalBindSig sigLoc =
      \case 
        L l (HsValBinds _ (ValBinds _ binds sigs)) ->
            L l 
          . HsValBinds NoExt 
          . ValBinds NoExt binds 
          . filter ((/= sigLoc) . getLoc) 
          $ sigs
        lhvb -> lhvb



-- TODO: better name
simplifyMatch :: LMatch GhcPs (LHsExpr GhcPs) -> R (LMatch GhcPs (LHsExpr GhcPs))
simplifyMatch = 
  reduceListOfSubelements match2SrcSpans deleteGRHS
  where
    match2SrcSpans =
      \case
        L _ (MatchP iterGRHSs _) -> map getLoc . reverse $ iterGRHSs -- reverse because the lower have to be tried first
        _ -> []
    deleteGRHS grhsLoc =
      \case
        L l m@(Match _ _ _ (GRHSs _ grhss lb)) ->
          let newGRHSs = filter ((/= grhsLoc) . getLoc) grhss
          in case newGRHSs of
              [] -> L l m
              _  -> L l $ m { m_grhss = GRHSs NoExt newGRHSs lb }
        lm -> lm


-- | change an expression to `undefined`
expr2Undefined :: HsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
expr2Undefined myUndefined lexp@(L l expr) 
  | oshow expr == "undefined" = lexp
  | otherwise = L l myUndefined

simplifyType :: LHsType GhcPs -> LHsType GhcPs
simplifyType t@(L _ UnitTypeP)    = t
simplifyType t@(L _ (HsFunTy NoExt (L _ UnitTypeP) (L _ UnitTypeP))) = t
simplifyType (ForallTypeP l body) = L l body
simplifyType (QualTypeP l body)   = L l body
simplifyType (L l _)              = L l UnitTypeP

gadtDelForall :: LConDecl GhcPs -> LConDecl GhcPs
gadtDelForall (L l gadtDecl@(ConDeclGADT _ _ (L forallLoc _) _ _ _ _ _)) =
  L l gadtDecl{ con_forall = L forallLoc False} -- delete forall
gadtDelForall d = d

gadtDelCtxt :: LConDecl GhcPs -> LConDecl GhcPs
gadtDelCtxt (L l gadtDecl@ConDeclGADT{}) =
  L l gadtDecl{ con_mb_cxt = Nothing}
gadtDelCtxt d = d


deleteWhereClause :: LHsLocalBinds GhcPs -> LHsLocalBinds GhcPs
deleteWhereClause e@(L _ (EmptyLocalBinds _)) = e
deleteWhereClause (L l _) = L l (EmptyLocalBinds NoExt)

-- TODO: this doesn't fit, this deletes the WHOLE match if only one grhs is undefined
matchDelRhsUndef :: Located [LMatch GhcPs (LHsExpr GhcPs)]
                       -> Located [LMatch GhcPs (LHsExpr GhcPs)]
matchDelRhsUndef (L l []) = L l []
matchDelRhsUndef (L l mtchs) =
        L l $ filter (\(L _ (Match _ _ _ grhss@GRHSs{})) -> 
                 showSDocUnsafe (pprGRHSs LambdaExpr grhss) /= "-> undefined")
               mtchs 

matchDelIfUndefAnywhere :: Located [LMatch GhcPs (LHsExpr GhcPs)]
                        -> Located [LMatch GhcPs (LHsExpr GhcPs)]
matchDelIfUndefAnywhere (L l []) = L l []
matchDelIfUndefAnywhere (L l mtchs) =
        L l $ filter (\(L _ (Match _ _ _ (GRHSs _ grhs _))) -> 
               not 
                 (all 
                   (("undefined" `isSubsequenceOf`) 
                   . showSDocUnsafe 
                   . pprGRHS LambdaExpr 
                   . unLoc) 
                   grhs))
               mtchs 

simplifyLGRHS :: LGRHS GhcPs (LHsExpr GhcPs) -> LGRHS GhcPs (LHsExpr GhcPs)
simplifyLGRHS lgrhs@(L _ (GRHS _ [] _)) = lgrhs
simplifyLGRHS (L l (GRHS _ _ body))     = L l (GRHS NoExt [] body)
simplifyLGRHS lgrhs = lgrhs

simplifyExpr :: LHsExpr GhcPs -> LHsExpr GhcPs
simplifyExpr (SingleCase l body) = L l body
simplifyExpr (L l (HsIf _ _ _ (L _ ls) (L _ rs)))
  | oshow ls == "undefined" = L l rs
  | oshow rs == "undefined" = L l ls
simplifyExpr e = e

-- | getting undefined as an expression
getUndefined :: MonadIO m => m (Maybe (HsExpr GhcPs))
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

pattern MatchP ::  [LGRHS GhcPs (LHsExpr GhcPs)] 
               -> LHsLocalBinds GhcPs 
               -> Match GhcPs (LHsExpr GhcPs)
pattern MatchP grhss binds <- Match _ _ _ (GRHSs _ grhss binds)

pattern FunBindGRHSP :: GRHSs GhcPs (LHsExpr GhcPs) -> HsDecl GhcPs
pattern FunBindGRHSP grhs <- (ValD _ (FunBind _ _ (MG _ (L _ [L _ (Match _ _ _ grhs)]) _) _ _))

pattern ForallTypeP, QualTypeP :: SrcSpan -> HsType GhcPs -> LHsType GhcPs
pattern ForallTypeP l body <-  L l (HsForAllTy _ _ (L _ body))
pattern QualTypeP   l body <-  L l (HsQualTy _ _ (L _ body))

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
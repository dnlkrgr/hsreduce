module Reduce.Passes.Stubbing (reduce) where

import Data.List
import Control.Monad.State.Strict
import qualified Data.Text as T
import Ormolu.Config (defaultConfig)
import Ormolu.Parser (parseModule)
import Ormolu.Parser.Result as OPR (ParseResult, prParsedSource)
import Ormolu.Printer (printModule)
import "ghc-lib-parser" HsSyn
import "ghc-lib-parser" SrcLoc
import "ghc-lib-parser" Outputable
import Data.Generics (everywhereM, mkM)


import Util.Types
import Util.Util


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
                          (mkM (try (expr2Undefined myUndefined))
                          >=> mkM (try matchDelRhsUndef)
                          >=> mkM (try matchDelIfUndefAnywhere)
                          >=> mkM simplifyMatch
                          >=> mkM (try simplifyLGRHS)
                          >=> mkM filterLocalBindSigs
                          >=> mkM (try simplifyExpr)
                          >=> mkM (try simplifyType)
                          >=> mkM (try deleteGADTforall) 
                          >=> mkM (try deleteGADTctxt) 
                          >=> mkM (try deleteWhereClause)) 
                        oldModule
        return oldOrmolu { prParsedSource = newModule }

type LLocalBind = Located (HsLocalBindsLR GhcPs GhcPs)

filterLocalBindSigs :: LLocalBind -> R LLocalBind
filterLocalBindSigs = 
  reduceListOfSubelements lvalBinds2SrcSpans deleteLocalBindSig
  where 
    lvalBinds2SrcSpans =
      \case
        (HsValBinds _ (ValBinds _ _ sigs)) -> map getLoc sigs
        _ -> []
    deleteLocalBindSig sigLoc =
      \case 
        (HsValBinds _ (ValBinds _ binds sigs)) ->
            HsValBinds NoExt 
          . ValBinds NoExt binds 
          . filter ((/= sigLoc) . getLoc) 
          $ sigs
        hvb -> hvb

-- TODO: better name
simplifyMatch :: LMatch GhcPs (LHsExpr GhcPs) -> R (LMatch GhcPs (LHsExpr GhcPs))
simplifyMatch = 
  reduceListOfSubelements match2SrcSpans deleteGRHS
  where
    match2SrcSpans =
      \case
        MatchP iterGRHSs _ -> map getLoc . reverse $ iterGRHSs -- reverse because the lower have to be tried first
        _ -> []
    deleteGRHS grhsLoc =
      \case
        m@(Match _ _ _ (GRHSs _ grhss lb)) ->
          let newGRHSs = filter ((/= grhsLoc) . getLoc) grhss
          in case newGRHSs of
              [] -> m
              _  -> m { m_grhss = GRHSs NoExt newGRHSs lb }
        m -> m


-- | change an expression to `undefined`
expr2Undefined :: HsExpr GhcPs -> HsExpr GhcPs -> HsExpr GhcPs
expr2Undefined myUndefined expr
  | oshow expr == "undefined" = expr
  | otherwise = myUndefined

simplifyType :: HsType GhcPs -> HsType GhcPs
simplifyType t@UnitTypeP        = t
simplifyType t@(HsFunTy NoExt (L _ UnitTypeP) (L _ UnitTypeP)) = t
simplifyType (ForallTypeP body) = body
simplifyType (QualTypeP body)   = body
simplifyType _                  = UnitTypeP

deleteGADTforall :: ConDecl GhcPs -> ConDecl GhcPs
deleteGADTforall gadtDecl@(ConDeclGADT _ _ (L forallLoc _) _ _ _ _ _) =
  gadtDecl{ con_forall = L forallLoc False} -- delete forall
deleteGADTforall d = d

deleteGADTctxt :: ConDecl GhcPs -> ConDecl GhcPs
deleteGADTctxt gadtDecl@ConDeclGADT{} =
  gadtDecl{ con_mb_cxt = Nothing}
deleteGADTctxt d = d


deleteWhereClause :: HsLocalBinds GhcPs -> HsLocalBinds GhcPs
deleteWhereClause e@(EmptyLocalBinds _) = e
deleteWhereClause _ = EmptyLocalBinds NoExt

-- TODO: this doesn't fit, this deletes the WHOLE match if only one grhs is undefined
matchDelRhsUndef :: [LMatch GhcPs (LHsExpr GhcPs)]
                       -> [LMatch GhcPs (LHsExpr GhcPs)]
matchDelRhsUndef [] = []
matchDelRhsUndef mtchs =
  filter (\(L _ (Match _ _ _ grhss@GRHSs{})) -> 
           showSDocUnsafe (pprGRHSs LambdaExpr grhss) /= "-> undefined")
         mtchs 

matchDelIfUndefAnywhere :: [LMatch GhcPs (LHsExpr GhcPs)]
                        -> [LMatch GhcPs (LHsExpr GhcPs)]
matchDelIfUndefAnywhere [] = []
matchDelIfUndefAnywhere mtchs =
  filter (\(L _ (Match _ _ _ (GRHSs _ grhs _))) -> 
           not 
             (all 
               (("undefined" `isSubsequenceOf`) 
               . showSDocUnsafe 
               . pprGRHS LambdaExpr 
               . unLoc) 
               grhs))
             mtchs 

simplifyLGRHS :: GRHS GhcPs (LHsExpr GhcPs) -> GRHS GhcPs (LHsExpr GhcPs)
simplifyLGRHS grhs@((GRHS _ [] _)) = grhs
simplifyLGRHS (GRHS _ _ body)     = GRHS NoExt [] body
simplifyLGRHS grhs = grhs

simplifyExpr :: HsExpr GhcPs -> HsExpr GhcPs
simplifyExpr (SingleCase body) = body
simplifyExpr (HsIf _ _ _ (L _ ls) (L _ rs))
  | oshow ls == "undefined" = rs
  | oshow rs == "undefined" = ls
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

pattern ForallTypeP, QualTypeP :: HsType GhcPs -> HsType GhcPs
pattern ForallTypeP body <-  (HsForAllTy _ _ (L _ body))
pattern QualTypeP   body <-  (HsQualTy _ _ (L _ body))

pattern UnitTypeP :: HsType GhcPs
pattern UnitTypeP = HsTupleTy NoExt HsBoxedTuple []

pattern SingleCase :: HsExpr GhcPs -> HsExpr GhcPs
pattern SingleCase body <- 
  HsCase _ 
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
             _)
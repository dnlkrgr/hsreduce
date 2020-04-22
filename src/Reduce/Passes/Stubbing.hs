module Reduce.Passes.Stubbing (reduce) where

import Debug.Trace
import qualified Data.Text as T
import Data.List
import Control.Monad.State.Strict
import "ghc" GHC
import "ghc" Outputable
import "ghc" OccName
import Data.Generics.Uniplate.Data

import Util.Types
import Util.Util

-- | run a pass on the old module and return the new one if it's interesting
reduce :: R ()
reduce = do
  oldState  <- get
  liftIO $ putStrLn "\n***Stubbing expressions***"
  liftIO $ debugPrint $ "Size of old state: " ++ (show . T.length . T.pack . showGhc . _parsed $ oldState)
  stubbings oldState >> return ()

stubbings :: RState -> R ParsedSource
stubbings =
  (
        traceShow ("simplifyExpr" :: String)            . transformBiM (try simplifyExpr)
    >=> traceShow ("deleteGADTforall" :: String)        . transformBiM (try deleteGADTforall)
    >=> traceShow ("deleteGADTctxt" :: String)          . transformBiM (try deleteGADTctxt)
    >=> traceShow ("matchDelRhsUndef" :: String)        . transformBiM (try matchDelRhsUndef)
    >=> traceShow ("matchDelIfUndefAnywhere" :: String) . transformBiM (try matchDelIfUndefAnywhere)
    >=> traceShow ("simplifyMatch" :: String)           . transformBiM simplifyMatch
    >=> traceShow ("simplifyLGRHS" :: String)           . transformBiM (try simplifyLGRHS)
    >=> traceShow ("filterLocalBindSigs" :: String)     . transformBiM filterLocalBindSigs
    >=> traceShow ("simplifyType" :: String)            . transformBiM (try simplifyType)
    >=> traceShow ("deleteWhereClause" :: String)       . transformBiM (try deleteWhereClause))
  . _parsed



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
simplifyLGRHS (GRHS _ _ body)      = GRHS NoExt [] body
simplifyLGRHS grhs = grhs

simplifyExpr :: HsExpr GhcPs -> HsExpr GhcPs
simplifyExpr (SingleCase body) = body
simplifyExpr (HsIf _ _ _ (L _ ls) (L _ rs))
  | oshow ls == "undefined" = rs
  | oshow rs == "undefined" = ls
simplifyExpr expr
  | oshow expr == "undefined" = expr
  | otherwise = HsVar NoExt . noLoc . Unqual . mkOccName varName $ "undefined"


pattern MatchP ::  [LGRHS GhcPs (LHsExpr GhcPs)]
               -> LHsLocalBinds GhcPs
               -> Match GhcPs (LHsExpr GhcPs)
pattern MatchP grhss binds <- Match _ _ _ (GRHSs _ grhss binds)


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


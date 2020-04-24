module Reduce.Passes.Stubbing (reduce) where

import Data.Maybe
import Debug.Trace
import qualified Data.Text as T
import Data.List
import Control.Monad.State.Strict
import "ghc" BasicTypes
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
         traceShow ("filterRecordFields" :: String )     . transformBiM filterRecordFields
     >=> traceShow ("simplifyExpr" :: String)            . transformBiM (try simplifyExpr)
     >=> traceShow ("deleteGADTforall" :: String)        . transformBiM (try deleteGADTforall)
     >=> traceShow ("deleteGADTctxt" :: String)          . transformBiM (try deleteGADTctxt)
     >=> traceShow ("matchDelRhsUndef" :: String)        . transformBiM (try matchDelRhsUndef)
     >=> traceShow ("matchDelIfUndefAnywhere" :: String) . transformBiM (try matchDelIfUndefAnywhere)
     >=> traceShow ("simplifyMatch" :: String)           . transformBiM simplifyMatch
     >=> traceShow ("simplifyLGRHS" :: String)           . transformBiM (try simplifyLGRHS)
     >=> traceShow ("filterLocalBinds" :: String)        . transformBiM filterLocalBinds
     >=> traceShow ("simplifyType" :: String)            . transformBiM (try simplifyType)
     >=> traceShow ("deleteWhereClause" :: String)       . transformBiM (try deleteWhereClause)
     >=> traceShow ("inlineFunctions" :: String)         . transformBiM inlineFunctions)
  . _parsed


-- arst :: HsRecFields GhcPs a -> R (HsRecFields GhcPs a)
-- arst =
--   reduceListOfSubelements (map getLoc . rec_flds) undefined


filterRecordFields :: Located [LConDeclField GhcPs] -> R (Located [LConDeclField GhcPs])
filterRecordFields =
  reduceListOfSubelements (map getLoc . id) g
  where
    g loc decls = filter ((/= loc) . getLoc) $ traceShow ("decls " ++ showGhc decls) decls

type LLocalBind = Located (HsLocalBindsLR GhcPs GhcPs)

filterLocalBinds :: LLocalBind -> R LLocalBind
filterLocalBinds =
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


-- function inlining
-- TODO: check when to inline

-- 1. maybe even do this at funbinds / val decls?
-- -> if the function is small and often called => inline it
-- count occurences

-- 2. maybe just inline everytime?
inlineFunctions :: LHsExpr GhcPs -> R (LHsExpr GhcPs)
inlineFunctions old@(L _ (HsApp _ (L l1 (HsVar _ (L _ n))) expr)) = do

  p <- gets _parsed

  let funMatches =
          map snd
        . filter ((== n) . fst)
        . fromMaybe []
        . traverse getNameAndMatchGroup
        . getFunBinds
        $ p

  if (null funMatches)
    then return old
    else do

      let (MG _ (L l2 matches) _ : _) = funMatches
      liftIO $ putStrLn $ "\n\nlength matches: " ++ show (length matches)

      let (con, ctxt) = if length matches == 1
                        then (HsLam, LambdaExpr)
                        else (HsLamCase,CaseAlt)
          newMG = MG NoExt (L l2 $ map (changeMatchContext ctxt) matches) FromSource
          new   = HsApp NoExt (L l1 (HsPar NoExt (noLoc (con NoExt newMG)))) expr

      liftIO . putStrLn $ "new: " ++ showGhc new
      tryNewValue old new
inlineFunctions e = return e

changeMatchContext :: HsMatchContext RdrName
     -> LMatch GhcPs (LHsExpr GhcPs)
     -> LMatch GhcPs (LHsExpr GhcPs)
changeMatchContext ctxt (L l (Match _ _ p g)) =
  L l $ Match NoExt ctxt p g
changeMatchContext _ m = m

getFunBinds :: ParsedSource -> [HsBindLR GhcPs GhcPs]
getFunBinds p = [ f | f@(FunBind {}) <- universeBi p ]

getNameAndMatchGroup :: HsBindLR idL idR
                     -> Maybe (IdP idL, MatchGroup idR (LHsExpr idR))
getNameAndMatchGroup (FunBind _ (L _ n) mg _ _) = Just (n, mg)
getNameAndMatchGroup _ = Nothing

-- expr -> undefined
-- delete if-then-else branches
-- case expr with one case -> body
simplifyExpr :: HsExpr GhcPs -> HsExpr GhcPs
simplifyExpr (HsPar _ (L _ expr)) = expr
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


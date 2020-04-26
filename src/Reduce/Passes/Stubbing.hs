module Reduce.Passes.Stubbing (reduce) where

import Data.Ratio
import qualified Data.ByteString as BS
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
         traceShow ("recordCon" :: String)               . transformBiM recordCon
     >=> traceShow ("exprWithList" :: String)            . transformBiM exprWithList
     >=> traceShow ("simplifyExpr" :: String)            . transformBiM (fastTry simplifyExpr)
     >=> traceShow ("expr2Undefined" :: String)          . transformBiM (fastTry expr2Undefined)
     >=> traceShow ("deleteGADTforall" :: String)        . transformBiM (fastTry deleteGADTforall)
     >=> traceShow ("deleteGADTctxt" :: String)          . transformBiM (fastTry deleteGADTctxt)
     >=> traceShow ("matchDelRhsUndef" :: String)        . transformBiM (try matchDelRhsUndef)
     >=> traceShow ("matchDelIfUndefAnywhere" :: String) . transformBiM (try matchDelIfUndefAnywhere)
     >=> traceShow ("simplifyMatch" :: String)           . transformBiM simplifyMatch
     >=> traceShow ("simplifyLGRHS" :: String)           . transformBiM (fastTry simplifyLGRHS)
     >=> traceShow ("localBinds" :: String)              . transformBiM localBinds
     >=> traceShow ("simplifyType" :: String)            . transformBiM (fastTry simplifyType)
     >=> traceShow ("deleteWhereClause" :: String)       . transformBiM (fastTry deleteWhereClause)
     -- >=> traceShow ("inlineFunctions" :: String)         . transformBiM inlineFunctions
     >=> traceShow ("simplifyLit" :: String)            . transformBiM (try simplifyLit)
     >=> traceShow ("simplifyPat" :: String)            . transformBiM (try simplifyPat))
  . _parsed


recordCon :: LConDecl GhcPs -> R (LConDecl GhcPs)
recordCon =
  reduceListOfSubelements f g
  where
    f = \case
      XConDecl _ -> []
      c -> (\case
              RecCon (L _ flds) -> map getLoc flds
              _ -> []) . con_args $ c
    g loc = \case
      XConDecl _ -> XConDecl NoExt
      c -> c { con_args = case con_args c of
                 RecCon (L l flds) -> RecCon . L l $ filter ((/= loc) . getLoc) flds
                 c -> c }

-- simplifyin any expression with a list in it
exprWithList :: LHsExpr GhcPs -> R (LHsExpr GhcPs)
exprWithList = reduceListOfSubelements f g
  where f  = \case
          (RecordUpd _ _ fields)   -> map getLoc fields
          (RecordCon _ _ fields)   -> map getLoc . rec_flds $ fields
          (ExplicitTuple _ args _) -> map getLoc args
          (HsCase _ _ mg) -> map getLoc . unLoc . mg_alts $ mg
          (HsMultiIf _ es) -> map getLoc es
          (HsDo _ _ (L _ stmts)) -> map getLoc stmts
          (ExplicitList _ _ es) -> map getLoc es
          _ -> []
        g loc = \case
          (RecordUpd _ e fields) -> RecordUpd NoExt e $ filter ((/= loc) . getLoc) fields
          (RecordCon _ n fields)   -> RecordCon NoExt n $
            fields { rec_flds = filter ((/= loc) . getLoc) (rec_flds fields) }
          (ExplicitTuple _ args b) -> ExplicitTuple NoExt (filter ((/= loc) . getLoc) args) b
          (HsCase _ e mg) -> HsCase NoExt e $
            mg { mg_alts = fmap (filter ((/= loc) . getLoc)) (mg_alts mg) }
          (HsMultiIf _ es) -> HsMultiIf NoExt $ filter ((/= loc) . getLoc) es
          (HsDo _ ctxt (L l stmts)) -> HsDo NoExt ctxt $ L l $ filter ((/= loc) . getLoc) stmts
          (ExplicitList _ se es) -> ExplicitList NoExt se $ filter ((/= loc) . getLoc) es
          e -> e

type LLocalBind = Located (HsLocalBindsLR GhcPs GhcPs)

localBinds :: LLocalBind -> R LLocalBind
localBinds =
  reduceListOfSubelements lvalBinds2SrcSpans deleteLocalBindSig
  where
    lvalBinds2SrcSpans = \case
      (HsValBinds _ (ValBinds _ _ sigs)) -> map getLoc sigs
      _ -> []
    deleteLocalBindSig sigLoc = \case
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

simplifyType :: HsType GhcPs -> Maybe (HsType GhcPs)
simplifyType UnitTypeP        = Nothing
simplifyType (HsFunTy NoExt (L _ UnitTypeP) (L _ UnitTypeP)) = Nothing
simplifyType (ForallTypeP body) = Just body
simplifyType (QualTypeP body)   = Just body
simplifyType _                  = Just UnitTypeP

deleteGADTforall :: ConDecl GhcPs -> Maybe (ConDecl GhcPs)
deleteGADTforall gadtDecl@(ConDeclGADT _ _ (L forallLoc _) _ _ _ _ _) =
  Just $ gadtDecl{ con_forall = L forallLoc False} -- delete forall
deleteGADTforall _ = Nothing

deleteGADTctxt :: ConDecl GhcPs -> Maybe (ConDecl GhcPs)
deleteGADTctxt gadtDecl@ConDeclGADT{} =
  Just $ gadtDecl{ con_mb_cxt = Nothing}
deleteGADTctxt _ = Nothing

deleteWhereClause :: HsLocalBinds GhcPs -> Maybe (HsLocalBinds GhcPs)
deleteWhereClause (EmptyLocalBinds _) = Nothing
deleteWhereClause _ = Just $ EmptyLocalBinds NoExt

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

simplifyLGRHS :: GRHS GhcPs (LHsExpr GhcPs) -> Maybe (GRHS GhcPs (LHsExpr GhcPs))
simplifyLGRHS (GRHS _ _ body)      = Just $ GRHS NoExt [] body
simplifyLGRHS _ = Nothing


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

      let (MG _ (L l2 lmatches) _ : _) = funMatches
      let nPats = length . m_pats . unLoc . head $ lmatches
      let nMatches = length lmatches
      liftIO $ putStrLn $ "\n\nlength lmatches: " ++ show nMatches

      -- this is obviously not the best we can do
      -- but I don't know how to handle n matches with m patterns yet
      let app = \con ctxt f -> HsApp NoExt (L l1 (HsPar NoExt (noLoc (con NoExt $ MG NoExt (L l2 $ map (changeMatchContext ctxt) (f lmatches)) FromSource)))) expr
      let new = case (nMatches, nPats) of
                           (1, 0) -> unLoc old -- eta reduced function, how to handle multiple guards?
                           (1, _) -> app HsLam LambdaExpr (take 1)
                           (_, 1) -> app HsLamCase CaseAlt id
                           _      -> unLoc old

          -- if the user created overlapping patterns we only take the first match
          -- or should we do nothing when encountering overlapping patterns?

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
simplifyExpr :: HsExpr GhcPs -> Maybe (HsExpr GhcPs)
simplifyExpr (SingleCase body)     = Just body
simplifyExpr (HsIf _ _ _ (L _ ls) (L _ rs))
  | oshow ls == "undefined"        = Just $ rs
  | oshow rs == "undefined"        = Just $ ls
simplifyExpr (HsApp _ _ e)         = Just $ unLoc e
simplifyExpr (HsAppType _ e)       = Just $ unLoc e
simplifyExpr (OpApp _ _ l r)
  | lshow r == "undefined" = Just $ unLoc l
  | lshow l == "undefined" = Just $ unLoc r
simplifyExpr (NegApp _ e _)        = Just $ unLoc e
simplifyExpr (HsPar _ (L _ e))     = Just $ e
simplifyExpr (ExplicitTuple _ _ b) = Just $ ExplicitTuple NoExt [] b
simplifyExpr (ExplicitSum _ _ _ e) = Just $ unLoc e
simplifyExpr (HsMultiIf _ _)       = Just $ HsMultiIf NoExt []
simplifyExpr (HsDo _ ctxt (L l _)) = Just $ HsDo NoExt ctxt (L l [])      -- do reduceSubElements
simplifyExpr (ExplicitList _ _ _)  = Just $ ExplicitList NoExt Nothing [] -- do reduceSubElements
simplifyExpr (RecordUpd _ e _)     = Just $ unLoc e
simplifyExpr (ExprWithTySig _ e)   = Just $ unLoc e
simplifyExpr (HsSCC _ _ _ e)       = Just $ unLoc e
simplifyExpr (HsCoreAnn _ _ _ e)   = Just $ unLoc e -- this will probably never work, what else could be done here?
simplifyExpr (HsStatic _ e)        = Just $ unLoc e
simplifyExpr (HsArrApp _ _ e _ _)  = Just $ unLoc e
simplifyExpr (HsArrForm _ e _ _)   = Just $ unLoc e
simplifyExpr (HsTick _ _ e)        = Just $ unLoc e
simplifyExpr (HsBinTick _ _ _ e)   = Just $ unLoc e
simplifyExpr (EAsPat _ _ e)        = Just $ unLoc e
simplifyExpr (EViewPat _ _ e)      = Just $ unLoc e -- choosing right expression, is this correct?
simplifyExpr (ELazyPat _ e)        = Just $ unLoc e
simplifyExpr (HsWrap _ _ e)        = Just $ e
simplifyExpr _ = Nothing


expr2Undefined :: HsExpr GhcPs -> Maybe (HsExpr GhcPs)
expr2Undefined expr
  | oshow expr == "undefined" = Nothing
  | otherwise = Just $ HsVar NoExt . noLoc . Unqual . mkOccName varName $ "undefined"

simplifyPat :: Pat GhcPs -> Pat GhcPs
simplifyPat (ParPat _ p)     = unLoc p
simplifyPat (LazyPat _ p)    = unLoc p
simplifyPat (BangPat _ p)    = unLoc p
simplifyPat (AsPat _ _ p)    = unLoc p
simplifyPat (SigPat _ p)     = unLoc p
simplifyPat (ViewPat _ _ p)  = unLoc p
simplifyPat (ListPat _ _)    = ListPat NoExt []
simplifyPat (TuplePat _ _ b) = TuplePat NoExt [] b
simplifyPat (CoPat _ _ p _)  = p
simplifyPat _ = WildPat NoExt


-- are there more interesting things we can do here?
srcText :: SourceText
srcText = SourceText ""
simplifyLit :: HsLit GhcPs -> HsLit GhcPs
simplifyLit (HsChar _ _)       = HsChar srcText 'a'
simplifyLit (HsCharPrim _ _)   = HsCharPrim srcText 'a'
simplifyLit (HsString _ _)     = HsString srcText ""
simplifyLit (HsStringPrim _ _) = HsStringPrim srcText BS.empty
simplifyLit (HsInt _ _)        = HsInt NoExt $ IL srcText False 0
simplifyLit (HsIntPrim _ _)    = HsIntPrim srcText 0
simplifyLit (HsWordPrim _ _)   = HsWordPrim srcText 0
simplifyLit (HsInt64Prim _ _)  = HsInt64Prim srcText 0
simplifyLit (HsWord64Prim _ _) = HsWord64Prim srcText 0
simplifyLit (HsInteger _ _ t)  = HsInteger srcText 0 t
simplifyLit (HsRat _ _ t)      = HsRat NoExt (FL srcText False (0 % 1)) t
simplifyLit (HsFloatPrim _ _ ) = HsFloatPrim NoExt (FL srcText False (0 % 1))
simplifyLit (HsDoublePrim _ _) = HsDoublePrim NoExt (FL srcText False (0 % 1))
simplifyLit l = l


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


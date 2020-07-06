module Reduce.Passes.Stubbing where

import Bag
import Debug.Trace
import Control.Monad.Reader
import Data.Ratio
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.List
import Control.Monad.State.Strict
import BasicTypes
import GHC
import Outputable hiding ((<>))
import OccName
import Data.Generics.Uniplate.Data

import Util.Types
import Util.Util


-- | run a pass on the old module and return the new one if it's interesting
reduce :: R ()
reduce = do
    oldState  <- get

    liftIO $ putStrLn "\n***Stubbing expressions***"
    liftIO $ putStrLn $ "Size of old state: " ++ (show . T.length . showState $ oldState)

    allActions


allActions :: R ()
allActions = do
    (traceShow ("simplifyExpr"      :: String) $ runPass simplifyExpr)
    (traceShow ("simplifyConDecl"   :: String) $ runPass simplifyConDecl)
    (traceShow ("simplifyMatches"   :: String) $ runPass simplifyMatches)
    (traceShow ("simplifyMatch"     :: String) $ runPass simplifyMatch)
    (traceShow ("simplifyLGRHS"     :: String) $ runPass simplifyLGRHS)
    (traceShow ("familyResultSig"   :: String) $ runPass familyResultSig)
    (traceShow ("tyVarBndr"         :: String) $ runPass tyVarBndr)
    (traceShow ("simplifyType"      :: String) $ runPass simplifyType)
    (traceShow ("localBinds"        :: String) $ runPass localBinds)
    (traceShow ("pat2Wildcard"      :: String) $ runPass pat2Wildcard)


-- ***************************************************************************
-- REAL STUBBINGS
-- ***************************************************************************
pat2Wildcard :: WaysToChange (Pat GhcPs)
pat2Wildcard WildPat{}  = []
pat2Wildcard _          = [const (WildPat NoExt)]


-- ***************************************************************************
-- TYPES
-- ***************************************************************************
simplifyType :: WaysToChange (HsType GhcPs)
simplifyType UnitTypeP                                                         = []
simplifyType t@(ForallTypeP body)                                              = handleSubList fType pType t <> map const [UnitTypeP, body]
simplifyType t@(QualTypeP body)                                                = handleSubList fType pType t <> map const [UnitTypeP, body]
simplifyType (HsOpTy _ (L _ l) _ (L _ r))                                      = map const [UnitTypeP, l, r] -- doesn't work so far :-/
simplifyType (HsAppTy _ (L _ (HsAppTy _ _ (L _ t1))) (L _ (HsTupleTy _ _ []))) = map const [UnitTypeP, t1]
simplifyType (HsAppTy _ (L l _)  u@(L _ (HsTupleTy _ _ [])))                   = map const [UnitTypeP, HsAppTy NoExt (L l $ HsTyVar NoExt NotPromoted (noLoc $ Unqual $ mkVarOcc "Maybe")) u]
simplifyType (HsKindSig _ (L _ t) _)                                           = map const [UnitTypeP, t]
simplifyType _                                                                 = map const [UnitTypeP]

pattern UnitTypeP :: HsType GhcPs
pattern UnitTypeP = HsTupleTy NoExt HsBoxedTuple []

-- TODO: make this more local
pType :: HsType p -> [SrcSpan]
pType = \case
    (HsForAllTy _ bndrs _) -> map getLoc bndrs
    (HsQualTy _ ctxt _)    -> map getLoc $ unLoc ctxt
    _ -> []

fType :: SrcSpan -> HsType p -> HsType p
fType loc = \case
    (HsForAllTy x bndrs body) -> HsForAllTy x (filter ((/= loc) . getLoc) bndrs) body
    (HsQualTy x ctxt body)    -> HsQualTy   x (filter ((/= loc) . getLoc) <$> ctxt) body
    x -> x


-- ***************************************************************************
-- CON DECLS
-- ***************************************************************************
simplifyConDecl :: WaysToChange (ConDecl GhcPs)
simplifyConDecl gadtDecl@(ConDeclGADT _ _ (L forallLoc _) _ _ _ _ _) = map const [gadtDecl{ con_forall = L forallLoc False}, gadtDecl{ con_mb_cxt = Nothing}]
simplifyConDecl d 
    | isRecCon d = handleSubList f p d
    | otherwise  = []
  where
    isRecCon    = (\case
                      RecCon _ -> True
                      _        -> False)
                  . con_args

    p           = (\case
                      RecCon (L _ flds) -> map getLoc flds
                      _                 -> []) 
                  . con_args

    f loc       = \case
        XConDecl _ -> XConDecl NoExt
        c -> c { con_args = case con_args c of
            RecCon (L l flds) -> RecCon . L l $ filter ((/= loc) . getLoc) flds
            a                 -> a }


-- ***************************************************************************
-- BINDS
-- ***************************************************************************

data LocalBindSpan = Bind SrcSpan | Sig SrcSpan
    deriving Eq

localBinds :: WaysToChange (HsLocalBinds GhcPs) 
localBinds EmptyLocalBinds{}    = []
localBinds v@HsValBinds{}       = handleSubList f p v <> [const (EmptyLocalBinds NoExt)]
  where
      p = \case
          (HsValBinds _ (ValBinds _ binds sigs))    ->  let bindList = bagToList binds
                                                        in map (Sig . getLoc) sigs <> map (Bind . getLoc) bindList
          _                                         ->  []
      f (Sig loc) = \case
          (HsValBinds _ (ValBinds _ binds sigs))    ->  HsValBinds NoExt . ValBinds NoExt binds . filter ((/= loc) . getLoc) $ sigs
          hvb                                       ->  hvb
      f (Bind loc) = \case
          (HsValBinds _ (ValBinds _ binds sigs))    ->  HsValBinds NoExt . ValBinds NoExt (listToBag . filter ((/= loc) . getLoc) $ bagToList binds) $ sigs
          hvb                                       ->  hvb
localBinds v@HsIPBinds{}        =  handleSubList f p v <> [const (EmptyLocalBinds NoExt)]
  where
      p = \case
          (HsIPBinds _ (IPBinds _ binds))           ->  map getLoc binds
          _                                         ->  []
      f loc = \case
          (HsIPBinds _ (IPBinds _ binds))           ->  HsIPBinds NoExt . IPBinds NoExt . filter ((/= loc) . getLoc) $ binds
          hvb                                       ->  hvb
localBinds _                    = [const (EmptyLocalBinds NoExt)]


-- ***************************************************************************
-- MATCHES
-- ***************************************************************************
simplifyMatch :: WaysToChange (Match GhcPs (LHsExpr GhcPs))
simplifyMatch = handleSubList f p
  where
    p = \case
        -- reverse because the lower have to be tried first
        MatchP iterGRHSs _ -> map getLoc . reverse $ iterGRHSs
        _ -> []
    f grhsLoc = \case
        m@(Match _ _ _ (GRHSs _ grhss lb)) ->
          let newGRHSs = filter ((/= grhsLoc) . getLoc) grhss
          in case newGRHSs of
              [] -> m
              _  -> m { m_grhss = GRHSs NoExt newGRHSs lb }
        m -> m


-- TODO: this doesn't fit, this deletes the WHOLE match if only one grhs is undefined
simplifyMatches :: WaysToChange [LMatch GhcPs (LHsExpr GhcPs)]
simplifyMatches m = 
    handleSubList (\loc -> filter ((/= loc) . getLoc)) (map getLoc) (traceShow (unlines $ map lshow m) m)
    <> [ filter (\(L _ (Match _ _ _ grhss@GRHSs{})) -> showSDocUnsafe (pprGRHSs LambdaExpr grhss) /= "-> undefined")
    ,  filter (\(L _ (Match _ _ _ (GRHSs _ grhs _))) -> not (all ( ("undefined" `isSubsequenceOf`) . showSDocUnsafe . pprGRHS LambdaExpr . unLoc) grhs))]


simplifyLGRHS :: WaysToChange (GRHS GhcPs (LHsExpr GhcPs))
simplifyLGRHS g@(GRHS _ _ body) = [const (GRHS NoExt [] body)] <> handleSubList f p g
    where 
        p (GRHS _ stmts _)  = map getLoc stmts
        p _                 = []

        f loc (GRHS _ s b)  = GRHS NoExt (filter ((/= loc) . getLoc) s) b
        f _ _               = g
simplifyLGRHS _ = []


-- function inlining
-- TODO: check when to inline

-- 1. maybe even do this at funbinds / val decls?
-- -> if the function is small and often called => inline it
-- count occurences

-- 2. maybe just inline everytime?
-- inlineFunctions :: LHsExpr GhcPs -> R (LHsExpr GhcPs)
-- inlineFunctions old@(L _ (HsApp _ (L l1 (HsVar _ (L _ n))) expr)) = do
-- 
--   p <- gets _parsed
-- 
--   let funMatches =
--           map snd
--         . filter ((== n) . fst)
--         . fromMaybe []
--         . traverse getNameAndMatchGroup
--         . getFunBinds
--         $ p
-- 
--   if null funMatches
--     then return old
--     else do
-- 
--       let (MG _ (L l2 lmatches) _ : _) = funMatches
--       let nPats = length . m_pats . unLoc . head $ lmatches
--       let nMatches = length lmatches
--       liftIO $ putStrLn $ "\n\nlength lmatches: " ++ show nMatches
-- 
--       -- this is obviously not the best we can do
--       -- but I don't know how to handle n matches with m patterns yet
--       let app con ctxt f = HsApp NoExt (L l1 (HsPar NoExt (noLoc (con NoExt $ MG NoExt (L l2 $ map (changeMatchContext ctxt) (f lmatches)) FromSource)))) expr
--       let new = case (nMatches, nPats) of
--                            (1, 0) -> unLoc old -- eta reduced function, how to handle multiple guards?
--                            (1, _) -> app HsLam LambdaExpr (take 1)
--                            (_, 1) -> app HsLamCase CaseAlt id
--                            _      -> unLoc old
-- 
--           -- if the user created overlapping patterns we only take the first match
--           -- or should we do nothing when encountering overlapping patterns?
-- 
--       liftIO . putStrLn $ "new: " ++ oshow new
--       tryNewValue old new
-- inlineFunctions e = return e

changeMatchContext :: HsMatchContext RdrName -> LMatch GhcPs (LHsExpr GhcPs) -> LMatch GhcPs (LHsExpr GhcPs)
changeMatchContext ctxt (L l (Match _ _ p g)) = L l $ Match NoExt ctxt p g
changeMatchContext _ m = m

getFunBinds :: ParsedSource -> [HsBindLR GhcPs GhcPs]
getFunBinds p = [ f | f@FunBind {} <- universeBi p ]

getNameAndMatchGroup :: HsBindLR idL idR -> Maybe (IdP idL, MatchGroup idR (LHsExpr idR))
getNameAndMatchGroup (FunBind _ (L _ n) mg _ _) = Just (n, mg)
getNameAndMatchGroup _ = Nothing


-- ***************************************************************************
-- EXPRESSSIONS
-- ***************************************************************************

simplifyExpr :: WaysToChange (HsExpr GhcPs)
simplifyExpr (SingleCase body)  = map const [body, undefExpr]
simplifyExpr (HsIf _ _ _ (L _ ls) (L _ rs))
  | oshow ls == "undefined"     = map const [rs, undefExpr]
  | oshow rs == "undefined"     = map const [ls, undefExpr]
simplifyExpr e@RecordUpd{}      = handleSubList fExpr pExpr e <> [const undefExpr] 
simplifyExpr e@RecordCon{}      = handleSubList fExpr pExpr e <> [const undefExpr]
simplifyExpr e@ExplicitTuple{}  = handleSubList fExpr pExpr e <> [const undefExpr]
simplifyExpr e@HsCase{}         = handleSubList fExpr pExpr e <> [const undefExpr]
simplifyExpr e@HsMultiIf{}      = handleSubList fExpr pExpr e <> [const undefExpr]
simplifyExpr e@HsDo{}           = handleSubList fExpr pExpr e <> [const undefExpr]
simplifyExpr e@ExplicitList{}   = handleSubList fExpr pExpr e <> [const undefExpr]
simplifyExpr e                  
    | oshow e /= "undefined"    = [const undefExpr]
    | otherwise                 = []

undefExpr :: HsExpr GhcPs
undefExpr = HsVar NoExt . noLoc . Unqual . mkOccName varName $ "undefined"

pExpr :: HsExpr p -> [SrcSpan]
pExpr = \case
    (RecordUpd _ _ fields)      -> map getLoc fields
    (RecordCon _ _ fields)      -> map getLoc . rec_flds $ fields
    (ExplicitTuple _ args _)    -> map getLoc args
    (HsCase _ _ mg)             -> map getLoc . unLoc . mg_alts $ mg
    (HsMultiIf _ es)            -> map getLoc es
    (HsDo _ _ (L _ stmts))      -> map getLoc stmts
    (ExplicitList _ _ es)       -> map getLoc es
    _                           -> []

fExpr :: SrcSpan -> HsExpr GhcPs -> HsExpr GhcPs
fExpr loc = \case
    (RecordUpd _ e fields)      -> RecordUpd NoExt e $ filter ((/= loc) . getLoc) fields
    (RecordCon _ n fields)      -> RecordCon NoExt n $ fields { rec_flds = filter ((/= loc) . getLoc) (rec_flds fields) }
    (ExplicitTuple _ args b)    -> ExplicitTuple NoExt (filter ((/= loc) . getLoc) args) b
    (HsCase _ e mg)             -> HsCase NoExt e $ mg { mg_alts = fmap (filter ((/= loc) . getLoc)) (mg_alts mg) }
    (HsMultiIf _ es)            -> HsMultiIf NoExt $ filter ((/= loc) . getLoc) es
    (HsDo _ ctxt (L l stmts))   -> HsDo NoExt ctxt $ L l $ filter ((/= loc) . getLoc) stmts
    (ExplicitList _ se es)      -> ExplicitList NoExt se $ filter ((/= loc) . getLoc) es 
    e                           -> e

-- expr -> undefined
-- delete if-then-else branches
-- case expr with one case -> body
-- simplifyExpr :: WaysToChange (HsExpr GhcPs)
-- simplifyExpr (HsApp _ _ e)         = Just $ unLoc e
-- simplifyExpr (HsAppType _ e)       = Just $ unLoc e
-- simplifyExpr (OpApp _ _ l r)
--   | lshow r == "undefined" = Just $ unLoc l
--   | lshow l == "undefined" = Just $ unLoc r
-- simplifyExpr (NegApp _ e _)        = Just $ unLoc e
-- simplifyExpr (HsPar _ (L _ e))     = Just $ e
-- simplifyExpr (ExplicitTuple _ _ b) = Just $ ExplicitTuple NoExt [] b
-- simplifyExpr (ExplicitSum _ _ _ e) = Just $ unLoc e
-- simplifyExpr (HsMultiIf _ _)       = Just $ HsMultiIf NoExt []
-- simplifyExpr (HsDo _ ctxt (L l _)) = Just $ HsDo NoExt ctxt (L l [])      -- do reduceSubElements
-- simplifyExpr (ExplicitList _ _ _)  = Just $ ExplicitList NoExt Nothing [] -- do reduceSubElements
-- simplifyExpr (RecordUpd _ e _)     = Just $ unLoc e
-- simplifyExpr (ExprWithTySig _ e)   = Just $ unLoc e
-- simplifyExpr (HsSCC _ _ _ e)       = Just $ unLoc e
-- simplifyExpr (HsCoreAnn _ _ _ e)   = Just $ unLoc e -- this will probably never work, what else could be done here?
-- simplifyExpr (HsStatic _ e)        = Just $ unLoc e
-- simplifyExpr (HsArrApp _ _ e _ _)  = Just $ unLoc e
-- simplifyExpr (HsArrForm _ e _ _)   = Just $ unLoc e
-- simplifyExpr (HsTick _ _ e)        = Just $ unLoc e
-- simplifyExpr (HsBinTick _ _ _ e)   = Just $ unLoc e
-- simplifyExpr (EAsPat _ _ e)        = Just $ unLoc e
-- simplifyExpr (EViewPat _ _ e)      = Just $ unLoc e -- choosing right expression, is this correct?
-- simplifyExpr (ELazyPat _ e)        = Just $ unLoc e
-- simplifyExpr (HsWrap _ _ e)        = Just $ e
-- simplifyExpr _ = []


-- ***************************************************************************
-- LITERALS
-- ***************************************************************************

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
simplifyLit l                  = l

-- ***************************************************************************
-- MISC
-- ***************************************************************************

familyResultSig :: WaysToChange (FamilyResultSig GhcPs)
familyResultSig (NoSig _)               = []
familyResultSig (XFamilyResultSig _)    = []
familyResultSig _                       = [const (NoSig NoExt)]

tyVarBndr :: WaysToChange (HsTyVarBndr GhcPs)
tyVarBndr (KindedTyVar _ lId _) = [const (UserTyVar NoExt lId)]
tyVarBndr _                     = []




-- ***************************************************************************
-- PATTERN SYNONYMS
-- ***************************************************************************

pattern MatchP ::  [LGRHS GhcPs (LHsExpr GhcPs)] -> LHsLocalBinds GhcPs -> Match GhcPs (LHsExpr GhcPs)
pattern MatchP grhss binds <- Match _ _ _ (GRHSs _ grhss binds)


pattern ForallTypeP, QualTypeP :: HsType GhcPs -> HsType GhcPs
pattern ForallTypeP body <- HsForAllTy _ _ (L _ body)
pattern QualTypeP   body <- HsQualTy _ _ (L _ body)

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
  

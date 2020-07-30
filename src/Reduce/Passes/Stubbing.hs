module Reduce.Passes.Stubbing (fast, medium, slow, slowest) where

import BasicTypes
import GHC
import Bag (bagToList, listToBag)
import OccName (mkVarOcc, mkOccName, varName)

import Util.Types
import Util.Util

printStubbingInfo :: R ()
printStubbingInfo = printInfo "Stubbing Expressions"

fast :: R ()
fast = do
    printStubbingInfo
    runPass "expr2Undefined" expr2Undefined


medium :: R ()
medium = do
    fast
    runPass "type2Unit" type2Unit


slow :: R ()
slow = do
    medium
    runPass "filterExprSubList"      filterExprSubList
    runPass "simplifyConDecl"        simplifyConDecl
    runPass "simplifyMatches"        simplifyMatches
    runPass "simplifyMatch"          simplifyMatch
    runPass "simplifyLGRHS"          simplifyLGRHS
    runPass "localBinds"             localBinds
    runPass "familyResultSig"        familyResultSig
    runPass "tyVarBndr"              tyVarBndr

slowest :: R ()
slowest = do
    slow
    runPass "simplifyType"           simplifyType
    runPass "simplifyExpr"           simplifyExpr
    runPass "pat2Wildcard"           pat2Wildcard
    runPass "simplifyConDecl"        simplifyConDecl
    runPass "simplifyDeriving"       simplifyDeriving
    runPass "simplifyDerivingClause" simplifyDerivingClause
    runPass "unqualNames"            unqualNames


-- ******************************
-- DATA TYPE /W 1 CONS -> NEWTYPE
-- ******************************

-- change datadecl into newtype
-- change occurrences / pattern matches to not have the data constructor
-- function sigs don't need to be changed




-- ******************************
-- INLINE NEWTYPE
-- ******************************

-- change occurrences / pattern matches to not have the data constructor
-- Patterns
-- Function Sigs

-- 1. go over all data decls
-- 2. check if they are newtypes
-- 3. inline patterns that match the constructor name
-- 4. inline type sigs that match the type name


-- ***************************************************************************
-- DERIVING CLAUSES
-- ***************************************************************************
simplifyDeriving :: WaysToChange [LHsDerivingClause GhcPs]
simplifyDeriving = handleSubList f p
  where
    p = map getLoc
    f loc = filter ((/= loc) . getLoc)

unIB :: HsImplicitBndrs pass thing -> thing
unIB (HsIB _ b) = b
unIB _ = error "Stubbing:unIB - trying to work with NoExt"

simplifyDerivingClause :: WaysToChange (HsDerivingClause GhcPs)
simplifyDerivingClause = handleSubList f p
  where
    p (HsDerivingClause _ _ t) = map (getLoc . unIB) $ unLoc t
    p _ = []

    -- f :: SrcSpan -> HsDerivingClause GhcPs -> HsDerivingClause GhcPs
    f loc  (HsDerivingClause x s t)= HsDerivingClause x s (filter ((/= loc) . getLoc . unIB) <$> t)
    f _  d = d


-- ***************************************************************************
-- PATTERNS
-- ***************************************************************************
pat2Wildcard :: WaysToChange (Pat GhcPs)
pat2Wildcard WildPat{}  = []
pat2Wildcard _          = [const (WildPat NoExt)]


-- ***************************************************************************
-- TYPES
-- ***************************************************************************
type2Unit :: WaysToChange (HsType GhcPs)
type2Unit UnitTypeP     = []
type2Unit _             = map const [UnitTypeP]

simplifyType :: WaysToChange (HsType GhcPs)
simplifyType UnitTypeP                                                         = []
simplifyType t@(ForallTypeP body)                                              = handleSubList fType pType t <> map const [body]
simplifyType t@(QualTypeP body)                                                = handleSubList fType pType t <> map const [body]
simplifyType (HsAppTy _ (L _ (HsAppTy _ _ (L _ t1))) (L _ (HsTupleTy _ _ []))) = map const [t1]

-- simplifyType at@HsAppTy{}                   = map const [HsAppTy NoExt (L l $ HsTyVar NoExt NotPromoted (noLoc $ Unqual $ mkVarOcc "Maybe")) u]
simplifyType (HsAppTy _ (L l _)  u@(L _ (HsTupleTy _ _ [])))                   = map const [HsAppTy NoExt (L l $ HsTyVar NoExt NotPromoted (noLoc $ Unqual $ mkVarOcc "Maybe")) u]

simplifyType (HsOpTy _ (L _ l) _ (L _ r))                                      = map const [l, r]
simplifyType (HsKindSig _ (L _ t) _)                                           = map const [t]
simplifyType _                                                                 = []

pattern UnitTypeP :: HsType GhcPs
pattern UnitTypeP = HsTupleTy NoExt HsBoxedTuple []

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
    | isRecCon d = handleSubList f p d <> [const (d { con_args = recCon2Prefix $ con_args d})]
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

recCon2Prefix :: HsConDetails (LBangType GhcPs) (Located [LConDeclField GhcPs]) 
              -> HsConDetails (LBangType GhcPs) (Located [LConDeclField GhcPs])
recCon2Prefix (RecCon rec) = PrefixCon . map (cd_fld_type . unLoc) $ unLoc rec
recCon2Prefix c = c

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
localBinds v@HsIPBinds{}        = handleSubList f p v <> [const (EmptyLocalBinds NoExt)]
  where
      p = \case
          (HsIPBinds _ (IPBinds _ binds))   ->  map getLoc binds
          _                                 ->  []
      f loc = \case
          (HsIPBinds _ (IPBinds _ binds))   ->  HsIPBinds NoExt . IPBinds NoExt . filter ((/= loc) . getLoc) $ binds
          hvb                               ->  hvb
localBinds _                    = [const (EmptyLocalBinds NoExt)]


-- ***************************************************************************
-- MATCHES
-- ***************************************************************************
simplifyMatch :: WaysToChange (Match GhcPs (LHsExpr GhcPs))
simplifyMatch (Match _ _ _ (GRHSs _ [] _)) = []
simplifyMatch mm = handleSubList f p mm
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


simplifyMatches :: WaysToChange [LMatch GhcPs (LHsExpr GhcPs)]
simplifyMatches = handleSubList (\loc -> filter ((/= loc) . getLoc)) (map getLoc)
    -- <> [ filter (\(L _ (Match _ _ _ grhss@GRHSs{})) -> showSDocUnsafe (pprGRHSs LambdaExpr grhss) /= "-> undefined")
    -- ,  filter (\(L _ (Match _ _ _ (GRHSs _ grhs _))) -> not (all ( ("undefined" `isSubsequenceOf`) . showSDocUnsafe . pprGRHS LambdaExpr . unLoc) grhs))]


simplifyLGRHS :: WaysToChange (GRHS GhcPs (LHsExpr GhcPs))
simplifyLGRHS (GRHS _ [] _) = []
simplifyLGRHS g@(GRHS _ _  body) = [const (GRHS NoExt [] body)] <> handleSubList f p g
    where 
        p (GRHS _ stmts _)  = map getLoc stmts
        p _                 = []

        f loc (GRHS _ s b)  = GRHS NoExt (filter ((/= loc) . getLoc) s) b
        f _ _               = g
simplifyLGRHS _ = []



-- ***************************************************************************
-- EXPRESSSIONS
-- ***************************************************************************
expr2Undefined :: WaysToChange (HsExpr GhcPs)
expr2Undefined e
    | oshow e /= "undefined"    = [const (HsVar NoExt . noLoc . Unqual . mkOccName varName $ "undefined")]
    | otherwise                 = []

filterExprSubList :: WaysToChange (HsExpr GhcPs)
filterExprSubList (SingleCase body)                  = map const [body]
filterExprSubList (HsIf _ _ _ (L _ ls) (L _ rs))     = map const [ls, rs]
filterExprSubList e@RecordUpd{}                      = handleSubList fExpr pExpr e
filterExprSubList e@RecordCon{}                      = handleSubList fExpr pExpr e
filterExprSubList e@ExplicitTuple{}                  = handleSubList fExpr pExpr e
filterExprSubList e@HsCase{}                         = handleSubList fExpr pExpr e
filterExprSubList e@HsMultiIf{}                      = handleSubList fExpr pExpr e
-- filterExprSubList e@HsDo{}                           = handleSubList fExpr pExpr e       <-- this creates unprintable expressions
filterExprSubList e@ExplicitList{}                   = handleSubList fExpr pExpr e
filterExprSubList _                                  = []

pExpr :: HsExpr p -> [SrcSpan]
pExpr = \case
    (RecordUpd _ _ fields)      -> map getLoc fields
    (RecordCon _ _ fields)      -> map getLoc . rec_flds $ fields
    (ExplicitTuple _ args _)    -> map getLoc args
    (HsCase _ _ mg)             -> map getLoc . unLoc . mg_alts $ mg
    (HsMultiIf _ es)            -> map getLoc es
    (HsDo _ _ (L _ stmts))      -> map getLoc stmts
    (ExplicitList _ _ es)       -> map getLoc es
    (HsArrForm _ _ _ cmds)      -> map getLoc cmds
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
    (HsArrForm x e f cmds)      -> HsArrForm x e f (filter ((/= loc) . getLoc) cmds)
    e                           -> e


simplifyExpr :: WaysToChange (HsExpr GhcPs)
simplifyExpr (HsApp _ (L _ l) (L _ r))          = map const [l, r]
simplifyExpr (HsAppType _ (L _ e) _)              = [const e]
simplifyExpr (OpApp _ _ (L _ l) (L _ r))        = map const [l, r]
simplifyExpr (HsLet _ _ (L _ e))                = [const e]
simplifyExpr (ExprWithTySig _ (L _ e) _)          = [const e]
simplifyExpr (HsStatic _ (L _ e))               = [const e]
simplifyExpr (HsArrApp _ (L _ l) (L _ r) _ _)   = map const [l, r]
simplifyExpr (HsTick _ _ (L _ e))               = [const e]
simplifyExpr (HsBinTick _ _ _ (L _ e))          = [const e]
simplifyExpr (EAsPat _ _ (L _ e))               = [const e]
simplifyExpr (EViewPat _ _ (L _ e))             = [const e]
simplifyExpr (ELazyPat _ (L _ e))               = [const e]
simplifyExpr (HsWrap _ _ e)                     = [const e]
simplifyExpr _                                  = []

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

unqualNames :: WaysToChange RdrName
unqualNames (Qual _ on) = [const (Unqual on)]
unqualNames _ = []


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
  

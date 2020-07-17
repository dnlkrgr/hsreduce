module Reduce.Passes.Stubbing (fast, medium, slow, slowest, rmvUnusedParams) where

import Lens.Micro.Platform
import Data.Generics.Uniplate.Data
import Control.Monad.State
import Control.Monad.Reader
import Bag
import GHC
import OccName

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
    runPass "filterExprSubList"     filterExprSubList
    runPass "simplifyConDecl"       simplifyConDecl
    runPass "simplifyMatches"       simplifyMatches
    runPass "simplifyMatch"         simplifyMatch
    runPass "simplifyLGRHS"         simplifyLGRHS
    runPass "localBinds"            localBinds
    runPass "familyResultSig"       familyResultSig
    runPass "tyVarBndr"             tyVarBndr

slowest :: R ()
slowest = do
    slow
    runPass "simplifyType"          simplifyType
    runPass "simplifyExpr"          simplifyExpr
    runPass "pat2Wildcard"          pat2Wildcard


-- this should prob. live in its own module
-- *** BEGINNING OF NEW STUFF
-- arst :: HsBindLR GhcPs GhcPs
rmvUnusedParams :: R ()
rmvUnusedParams = do
    printInfo "rmvUnusedParams"

    ast <- gets _parsed

    forM_ [ (funId, funMG) | (FunBind _ (L _ funId) funMG _ _ :: HsBindLR GhcPs GhcPs) <- universeBi ast] rmvUnusedParams_

rmvUnusedParams_ :: (RdrName,  MatchGroup GhcPs (LHsExpr GhcPs)) -> R ()
rmvUnusedParams_ (funId, funMG@(MG _ (L matchesLoc _) _)) = do
    case matchgroup2WildPatPositions funMG of
        Nothing        -> return ()
        Just (n, is)   -> do
            conf     <- ask
            oldState <- get
            let oldAST = oldState ^. parsed

            let 
                proposedChanges :: [ParsedSource -> ParsedSource]
                proposedChanges = 
                       [ transformBi $ overwriteAtLoc l (rmvWildPatExpr n is) 
                       | L l e <- universeBi oldAST, exprContainsFunId funId e ]
                    <> [ transformBi $ overwriteAtLoc l (rmvWildPatTypes is) 
                       | (L _ (SigD _ s@(TypeSig _ _ (HsWC _ (HsIB _ (L l (HsFunTy _ _ (L _ _))))))) :: LHsDecl GhcPs) <- universeBi oldAST
                       , sigContainsFunId funId s ]
                    <> [ transformBi (overwriteAtLoc matchesLoc rmvWildPatMatches)]

                newAST = foldr ($) oldAST proposedChanges

            let 
                sizeDiff    = length (lshow oldAST) - length (lshow newAST)
                newState    = oldState & parsed .~ newAST
 
            liftIO (tryNewValue conf newState) >>= \case
                True  -> do
                    parsed  .= newAST
                    isAlive %= (|| oshow oldAST /= oshow newAST)
 
                    updateStatistics "rmvUnusedParams" True sizeDiff

                False -> do
                    updateStatistics "rmvUnusedParams" False 0
 
rmvUnusedParams_ _ = return ()

-- simplifyTySigs
rmvWildPatTypes :: [Int] -> HsType GhcPs -> HsType GhcPs
rmvWildPatTypes [] t = t
rmvWildPatTypes (0:is) (HsFunTy _ _ (L _ t))   = rmvWildPatTypes is t
rmvWildPatTypes (_:is) (HsFunTy x a lt)        = HsFunTy x a (rmvWildPatTypes (map (\n -> n - 1) is) <$> lt)
rmvWildPatTypes _ t = t

-- simplifyFunctionCalls
-- here the pat indexes need to be reversed
-- we also need the total number of pats
rmvWildPatExpr :: Int -> [Int] -> HsExpr GhcPs -> HsExpr GhcPs
rmvWildPatExpr _ [] e                           = e
rmvWildPatExpr n (i:is) (HsApp x la@(L _ a) b)
    | n == i    = rmvWildPatExpr (n-1) is a
    | otherwise = HsApp x (rmvWildPatExpr (n-1) is <$> la) b
rmvWildPatExpr _ _ e                           = e

rmvWildPatMatches :: [LMatch GhcPs (LHsExpr GhcPs)] -> [LMatch GhcPs (LHsExpr GhcPs)]
rmvWildPatMatches mg = [ L l (Match NoExt ctxt (filter (p . unLoc) pats) grhss) | L l (Match _ ctxt pats grhss) <- mg ]
  where
    p (WildPat _) = True
    p _ = False

matchgroup2WildPatPositions :: MatchGroup  GhcPs (LHsExpr GhcPs) -> Maybe (Int, [Int])
matchgroup2WildPatPositions mg 
    | not (null pats) && all (== head pats) pats    = Just $ head pats
    | otherwise                                     = Nothing
  where
    pats = map (match2WildPatPositions . unLoc) . unLoc $ mg_alts mg

match2WildPatPositions :: Match GhcPs (LHsExpr GhcPs) -> (Int, [Int])
match2WildPatPositions m = (length pats, map fst . filter (p . unLoc . snd) $ zip [1..] pats)
  where 
    pats = m_pats m
    p (WildPat _)   = True
    p _             = False

sigContainsFunId :: RdrName -> Sig GhcPs -> Bool
sigContainsFunId n (TypeSig _ ids _)    = n `elem` (map unLoc ids)
sigContainsFunId _ _                    = False

exprContainsFunId :: RdrName -> HsExpr GhcPs -> Bool
exprContainsFunId n (HsApp _ (L _ (HsVar _ (L _ a))) _) = n == a
exprContainsFunId n (HsApp _ (L _ e) _)                 = exprContainsFunId n e
exprContainsFunId _ _                                   = False
-- *** END OF NEW STUFF








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
type2Unit UnitTypeP                                                      = []
type2Unit _                                                              = map const [UnitTypeP]

simplifyType :: WaysToChange (HsType GhcPs)
simplifyType UnitTypeP                                                         = []
simplifyType t@(ForallTypeP body)                                              = handleSubList fType pType t <> map const [body]
simplifyType t@(QualTypeP body)                                                = handleSubList fType pType t <> map const [body]
simplifyType (HsAppTy _ (L _ (HsAppTy _ _ (L _ t1))) (L _ (HsTupleTy _ _ []))) = map const [t1]
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
simplifyMatches m = handleSubList (\loc -> filter ((/= loc) . getLoc)) (map getLoc) m
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
filterExprSubList e@HsDo{}                           = handleSubList fExpr pExpr e
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
simplifyExpr (HsAppType _ (L _ e))              = [const e]
simplifyExpr (OpApp _ _ (L _ l) (L _ r))        = map const [l, r]
simplifyExpr (HsLet _ _ (L _ e))                = [const e]
simplifyExpr (ExprWithTySig _ (L _ e))          = [const e]
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
  

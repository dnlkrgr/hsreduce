module Reduce.Passes.Stubbing where

import Data.List.Split
import Control.Concurrent.Async
import Control.Monad.Reader
import Data.Ratio
import qualified Data.ByteString as BS
import Debug.Trace
import qualified Data.Text as T
import Data.List
import Control.Monad.State.Strict
import BasicTypes
import GHC
import Outputable hiding ((<>))
import OccName
import Data.Generics.Uniplate.Data
import Data.Data

import Util.Types
import Util.Util


-- | run a pass on the old module and return the new one if it's interesting
reduce :: R ()
reduce = do
    oldState  <- get
    liftIO $ putStrLn "\n***Stubbing expressions***"
    liftIO $ putStrLn $ "Size of old state: " ++ (show . T.length . showState $ oldState)
    -- void $ stubbings oldState
    newParsed <- stubbings $ _parsed oldState
    modify $ \s -> s { _parsed = newParsed }

type WaysToChange a = a -> [a -> a]

-- ***mock inverting intensifies***
stubbings :: ParsedSource -> R ParsedSource
stubbings = 
    runPass recordCon 
    >=> runPass exprWithList 
    >=> runPass simplifyExpr 
    >=> runPass expr2Undefined 
    -- >=> runPass deleteGADTforall 
    -- >=> runPass deleteGADTctxt 
    -- >=> runPass matchDelRhsUndef 
    -- >=> runPass matchDelIfUndefAnywhere 
    -- >=> runPass rmvMatches 
    -- >=> runPass simplifyMatch 
    -- >=> runPass simplifyLGRHS 
    -- >=> runPass valBinds 
    -- >=> runPass familyResultSig 
    -- >=> runPass tyVarBndr 
    -- >=> runPass simplifyType 
    -- >=> runPass simplifyTypeR 
    -- >=> runPass type2Unit 
    -- >=> runPass deleteWhereClause 
    -- -- >=> runPass inlineFunctions 
    -- -- >=> runPass simplifyLit 
    -- >=> runPass pat2Wildcard 

runPass :: Data a => (a -> [a -> a]) -> ParsedSource -> R ParsedSource
runPass pass ast = do 
    let arst = getProposedChanges ast pass

    results <- filter snd <$> getInterestingChanges ast arst

    let newAST =  applyChanges ast . map fst $ results

    -- modify $ \s -> s {
    --     _isAlive = 
    --         if _isAlive s && not (null results)
    --             then traceShow ("+" :: String) $ True
    --             else 
    --                 let e = oshow newAST /= oshow ast
    --                 in if e then traceShow ("+" :: String) e else e
    -- }

    return newAST


getProposedChanges :: Data a => ParsedSource -> (a -> [a -> a]) -> [Located a -> Located a]
getProposedChanges ast pass = concat [map (overwriteAtLoc' l) $ pass e | L l e <- universeBi ast]


getInterestingChanges :: Data a => ParsedSource -> [Located a -> Located a] -> R [(Located a -> Located a, Bool)]
getInterestingChanges ast proposedChanges = do
    oldState        <- get
    oldConf         <- ask
    numberOfThreads <- asks _numberOfThreads

    let l       = length proposedChanges
    let batches = let temp@(x:y:xs) = chunksOf (div l numberOfThreads) proposedChanges
                  in 
                      if length temp > numberOfThreads
                          then (x ++ y) : xs
                          else temp
    
    -- sourceDir <- getTempDir

    changes <- liftIO . fmap concat . forConcurrently batches $ \batch -> do
        -- create temp dir 
        -- copy test and source file there
        -- files      <- listDirectory $ fromAbsDir sourceDir

        -- tempDir <- fst <$> runR oldConf oldState getTempDir
  
        -- forM_ files $ \f -> do
        --     iterFile <- parseRelFile f
        --     let oldPath = fromAbsFile $ sourceDir </> iterFile
        --     status   <- getFileStatus oldPath
  
        --     if isDirectory status
        --         then copyDirectoryRecursive normal oldPath (fromAbsFile $ tempDir </> filename iterFile)
        --         else copyFile oldPath (fromAbsFile $ tempDir </> filename iterFile)

        -- let newConf = RConf test sourceFile numberOfThreads undefined

        forM batch $ \c -> do
            b <- tryNewValue' oldConf oldState ast c
            -- liftIO $ print b
            return (c, b)

    -- -- clean up the assignment of 
    -- gets _tempDirPaths >>= \case
    --     []   -> return ()
    --     x:xs -> modify $ \s -> s { _tempDirPaths = x : xs }

    return changes


applyChanges :: Data a => ParsedSource -> [Located a -> Located a] -> ParsedSource
applyChanges ast changes = foldr transformBi ast changes 
-- *** mocking ends **    

-- get ways to change an expression that contains a list as an subexpression
-- p: preprocessing (getting to the list)
-- f: filtering and returning a valid expression again
h :: Eq e => (a -> [e]) -> (e -> a -> a) -> WaysToChange a
h p f = map (\l o -> f l o) . p


class HasList a where
    hasList :: a -> Bool

instance HasList (HsType p) where
    hasList HsForAllTy {} = True
    hasList HsQualTy {}   = True
    hasList _ = False

-- *********************************************************************************************************************************************************************
-- PASSES
-- *********************************************************************************************************************************************************************

-- ***************************************************************************
-- REAL STUBBINGS
-- ***************************************************************************


type2Unit :: WaysToChange (HsType GhcPs)
type2Unit UnitTypeP = []
type2Unit _         = [const UnitTypeP]

pat2Wildcard :: WaysToChange (Pat GhcPs)
pat2Wildcard (WildPat{}) = []
pat2Wildcard _ = [const (WildPat NoExt)]


-- ***************************************************************************
-- TYPES
-- ***************************************************************************


simplifyType :: WaysToChange (HsType GhcPs)
simplifyType (HsFunTy NoExt (L _ UnitTypeP) (L _ UnitTypeP)) = []
-- simplifyType (ForallTypeP body) = Just body
simplifyType (QualTypeP body)           = [const ( body)]
simplifyType o@(HsOpTy _ (L _ t) _ _)   = traceShow ("HsOpTy: " <> oshow o <> " " <> oshow t) $ [const t] -- doesn't work so far :-/
simplifyType (HsAppTy _ (L _ (HsAppTy _ _ (L _ t1))) (L _ (HsTupleTy _ _ []))) = [const t1]
-- simplifyType (HsAppTy _ (L l t1@(HsTyVar{})) _ ) = traceShow ("TyVar: " <> oshow t1) Nothing
-- simplifyType (HsAppTy _ (L l t1@(HsSumTy{})) _ ) = traceShow ("HsSumTy: " <> oshow t1) Nothing
simplifyType (HsAppTy _ (L l _)  u@(L _ (HsTupleTy _ _ [])))    = [const (HsAppTy NoExt (L l $ HsTyVar NoExt NotPromoted (noLoc $ Unqual $ mkVarOcc "Maybe")) u)]
simplifyType (HsKindSig _ (L _ t) _)                            = [const t]
simplifyType _ = []



-- ***************************************************************************
-- CON DECLS
-- ***************************************************************************


deleteGADTforall :: WaysToChange (ConDecl GhcPs)
deleteGADTforall gadtDecl@(ConDeclGADT _ _ (L forallLoc _) _ _ _ _ _) = [const (gadtDecl{ con_forall = L forallLoc False})]
deleteGADTforall _ = []

deleteGADTctxt :: WaysToChange (ConDecl GhcPs) 
deleteGADTctxt gadtDecl@ConDeclGADT{} = [const (gadtDecl{ con_mb_cxt = Nothing})]
deleteGADTctxt _ = []

-- ***************************************************************************
-- BINDS
-- ***************************************************************************

type LocalBind = HsLocalBindsLR GhcPs GhcPs

valBinds :: WaysToChange LocalBind
valBinds x
    | isValBinds x = h p f x
    | otherwise    = []
  where
      isValBinds = (\case
                       HsValBinds _ ValBinds {} -> True
                       _ -> False )
      p = \case
          (HsValBinds _ (ValBinds _ _ sigs)) -> map getLoc sigs
          _ -> []
      f sigLoc = \case
          (HsValBinds _ (ValBinds _ binds sigs)) ->
              HsValBinds NoExt
            . ValBinds NoExt binds
            . filter ((/= sigLoc) . getLoc)
            $ sigs
          hvb -> hvb


deleteWhereClause :: WaysToChange (HsLocalBinds GhcPs) 
deleteWhereClause (EmptyLocalBinds _) = []
deleteWhereClause _                   = [const (EmptyLocalBinds NoExt)]

-- ***************************************************************************
-- MATCHES
-- ***************************************************************************

-- TODO: better name
simplifyMatch :: WaysToChange (Match GhcPs (LHsExpr GhcPs))
simplifyMatch = h p f
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
matchDelRhsUndef :: WaysToChange [LMatch GhcPs (LHsExpr GhcPs)]
matchDelRhsUndef _ = [filter (\(L _ (Match _ _ _ grhss@GRHSs{})) -> showSDocUnsafe (pprGRHSs LambdaExpr grhss) /= "-> undefined")]


-- TODO: filter out matches one by one
rmvMatches :: WaysToChange [LMatch GhcPs (LHsExpr GhcPs)]
rmvMatches = h (map getLoc) f
  where f loc = filter ((/= loc) . getLoc)

-- TODO: can we use -Wunused-... for this?
-- also: return Nothing for uninteresting cases
recordCon :: WaysToChange (ConDecl GhcPs)
-- recordCon (RecCon (L _ flds)) = map (\l o -> g l o) $ map getLoc flds
recordCon (XConDecl _) = []
recordCon x
 | isRecCon x = h p f x
 | otherwise  = []
  where
    isRecCon = (\case
                   RecCon _ -> True
                   _        -> False)
               . con_args
    p = \case
      c -> (\case
              RecCon (L _ flds) -> map getLoc flds
              _                 -> []) . con_args $ c
    f loc = \case
      XConDecl _ -> XConDecl NoExt
      c -> c { con_args = case con_args c of
                 RecCon (L l flds) -> RecCon . L l $ filter ((/= loc) . getLoc) flds
                 a                 -> a }

simplifyTypeR :: WaysToChange (HsType GhcPs)
simplifyTypeR t
    | hasList t = h p f t
    | otherwise = []
  where 
      p = \case
          (HsForAllTy _ bndrs _) -> map getLoc bndrs
          (HsQualTy _ ctxt _)    -> map getLoc $ unLoc ctxt
          _ -> []
      f loc = \case
          (HsForAllTy x bndrs body) -> HsForAllTy x (filter ((/= loc) . getLoc) bndrs) body
          (HsQualTy x ctxt body)    -> HsQualTy   x (filter ((/= loc) . getLoc) <$> ctxt) body
          x -> x


expr2Undefined :: WaysToChange (HsExpr GhcPs)
expr2Undefined expr
    | oshow expr == "undefined" = []
    | otherwise = [const (HsVar NoExt . noLoc . Unqual . mkOccName varName $ "undefined") ]

matchDelIfUndefAnywhere :: WaysToChange [LMatch GhcPs (LHsExpr GhcPs)]
matchDelIfUndefAnywhere _ =
  [filter (\(L _ (Match _ _ _ (GRHSs _ grhs _))) ->
           not
             (all
               ((\b -> traceShow (show b) b)
               . ("undefined" `isSubsequenceOf`)
               . trace'
               . showSDocUnsafe
               . pprGRHS LambdaExpr
               . unLoc)
               grhs))]

simplifyLGRHS :: WaysToChange (GRHS GhcPs (LHsExpr GhcPs))
simplifyLGRHS (GRHS _ _ body) = [const (GRHS NoExt [] body)]
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

changeMatchContext :: HsMatchContext RdrName
     -> LMatch GhcPs (LHsExpr GhcPs)
     -> LMatch GhcPs (LHsExpr GhcPs)
changeMatchContext ctxt (L l (Match _ _ p g)) =
  L l $ Match NoExt ctxt p g
changeMatchContext _ m = m

getFunBinds :: ParsedSource -> [HsBindLR GhcPs GhcPs]
getFunBinds p = [ f | f@FunBind {} <- universeBi p ]

getNameAndMatchGroup :: HsBindLR idL idR
                     -> Maybe (IdP idL, MatchGroup idR (LHsExpr idR))
getNameAndMatchGroup (FunBind _ (L _ n) mg _ _) = Just (n, mg)
getNameAndMatchGroup _ = Nothing


-- ***************************************************************************
-- EXPRESSSIONS
-- ***************************************************************************

-- simplifyin any expression with a list in it
exprWithList :: WaysToChange (HsExpr GhcPs)
exprWithList = h p f
  where p  = \case
          (RecordUpd _ _ fields)   -> map getLoc fields
          (RecordCon _ _ fields)   -> map getLoc . rec_flds $ fields
          (ExplicitTuple _ args _) -> map getLoc args
          (HsCase _ _ mg) -> map getLoc . unLoc . mg_alts $ mg
          (HsMultiIf _ es) -> map getLoc es
          (HsDo _ _ (L _ stmts)) -> map getLoc stmts
          (ExplicitList _ _ es) -> map getLoc es
          _ -> []
        f loc = \case
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

-- expr -> undefined
-- delete if-then-else branches
-- case expr with one case -> body
simplifyExpr :: WaysToChange (HsExpr GhcPs)
simplifyExpr (SingleCase body)     = [const body]
simplifyExpr (HsIf _ _ _ (L _ ls) (L _ rs))
  | oshow ls == "undefined"        = [const rs]
  | oshow rs == "undefined"        = [const ls]
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
simplifyExpr _ = []


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
simplifyLit l = l

-- ***************************************************************************
-- MISC
-- ***************************************************************************

familyResultSig :: WaysToChange (FamilyResultSig GhcPs)
familyResultSig (NoSig _) = []
familyResultSig (XFamilyResultSig _) = []
familyResultSig _ = [const (NoSig NoExt)]

tyVarBndr :: WaysToChange (HsTyVarBndr GhcPs)
tyVarBndr (KindedTyVar _ lId _) = [const (UserTyVar NoExt lId)]
tyVarBndr _ = []




-- ***************************************************************************
-- PATTERN SYNONYMS
-- ***************************************************************************

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


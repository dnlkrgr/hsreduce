module Reduce.Passes.Extensions.TypeFamilies (rmvEquations, inline) where

-- import CoAxiom
-- import qualified Control.Exception as CE
-- import Data.Maybe
-- import FamInst
-- import FamInstEnv
-- import TcHsType
-- import TcRnMonad

import BasicTypes
import OccName
import SrcLoc
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Generics.Uniplate.Data
import Data.List
import Debug.Trace
import GHC
import Lens.Micro.Platform
import Outputable hiding ((<>))
import Util.Types
import Util.Util

rmvEquations :: WaysToChange (HsDecl GhcPs)
rmvEquations = handleSubList f p
    where
        p (TyClD _ FamDecl {tcdFam = d}) = case fdInfo d of
            ClosedTypeFamily (Just equations) -> map getLoc equations
            _ -> []
        p _ = []
        f loc (TyClD _ t@FamDecl {tcdFam = d}) = case fdInfo d of
            ClosedTypeFamily (Just equations) ->
                TyClD NoExt $ t {tcdFam = d {fdInfo = ClosedTypeFamily . Just $ filter ((/= loc) . getLoc) equations}}
            _ -> TyClD NoExt t
        f _ t = t

inline :: R ()
inline = do
    let passId = "inline type families"
    printInfo passId

    ast <- fmap _parsed . liftIO . readTVarIO =<< asks _tState

    void $ transformBiM arst ast

arstP :: (Outputable.Outputable a1, Outputable.Outputable a2) => a1 -> a2 -> Bool
arstP feqn_rhs = (oshow feqn_rhs `isInfixOf`) . oshow

arst :: p ~ GhcPs => FamEqn p (HsTyPats p) (LHsType p) -> R (FamEqn p (HsTyPats p) (LHsType p))
arst f@(FamEqn {..})
    -- the rhs is one of the patterns
    -- get the index of the pattern
    -- find occurrences of the type family
    -- replace them by nth pattern
    | any (arstP feqn_rhs) feqn_pats = do
        conf <- ask
        let index = fst . head . filter (arstP feqn_rhs . snd) $ zip [1 ..] feqn_pats
        liftIO $ do
            putStrLn "\n\n\n\n"
            print $ oshow feqn_tycon
            print $ oshow feqn_pats
            print $ oshow feqn_rhs
            print index
        ast <- liftIO . fmap _parsed $ readTVarIO (_tState conf)
        newAST <- transformBiM (brst (unLoc feqn_tycon) (length feqn_pats) index) ast
        liftIO $
            tryNewState
                "inlineTypeFamilies"
                ( \oldState ->
                      let -- oldAST = oldState ^. parsed
                          newState =
                              oldState
                                  & parsed .~ newAST
                                  & numRmvdArgs +~ 1
                       in newState
                )
                conf
        return f
    -- the rhs is not found in any of the patterns
    -- find occurrences of the type family
    -- replace them by rhs
    | any ((== oshow feqn_rhs) . oshow) feqn_pats = return f
arst f = return f

-- the rhs is one of the patterns sub type expressions
-- get the index of the pattern
-- find occurrences of the sub type expression
-- replace them by the right hand side

brst :: p ~ GhcPs => IdP p -> Int -> Int -> LHsType p -> R (LHsType p)
brst tycon n i t
    -- | ("IfEq" `isInfixOf` oshow t || "IfEq" `isPrefixOf` oshow t), "IfEq" `isInfixOf` oshow tycon = do
    | "IfEq" `isInfixOf` oshow tycon = do
        liftIO $ putStrLn "\n\n\n"
        liftIO $ print (oshow t) 
        liftIO $ print (gshow t)
        let a = crst n i t
        return a

    | otherwise = return t

crst_ann :: Int -> Int -> LHsType GhcPs -> R (LHsType GhcPs)
crst_ann n i (unLoc -> HsParTy _ t) = crst_ann n i t
crst_ann n i t@(unLoc -> HsAppTy _ a b)
    | n == i = do
        liftIO $ print ("exiting: " <> oshow b) 
        return b
    | otherwise = do
        liftIO $ print ("descending: ") 
        liftIO $ print ("a: " <> oshow a) 
        liftIO $ print ("b: " <> oshow b) 
        crst_ann (n -1) i a
crst_ann _ _ e = do
    liftIO $ print ("doing nothing :/ " <> oshow e) 
    return e

crst :: Int -> Int -> LHsType GhcPs -> LHsType GhcPs
crst n i (unLoc -> HsParTy _ t) = crst n i t
crst n i (unLoc -> HsAppTy _ a b)
    | n == i = b
    | otherwise = crst (n -1) i a
crst _ _ e = e

mkApp :: p ~ GhcPs => (LHsType p) -> (LHsType p) -> LHsType p
mkApp l r = noLoc $ HsAppTy NoExt l r

mkTyVar :: String -> LHsType GhcPs
mkTyVar = noLoc . HsTyVar NoExt NotPromoted . noLoc . Unqual . mkVarOcc 

typeContainsId :: p ~ GhcPs => HsType p -> IdP p -> Bool
typeContainsId t tycon = 
    let b1 = oshow tycon `isInfixOf` oshow t 
        b2 = oshow tycon `isPrefixOf` oshow t
    in (if "If" `isInfixOf` oshow t then traceShow (show b1) . traceShow (show b2) else id) $ b1 || b2

-- typeContainsId (HsTyVar _ _ (unLoc -> n)) tycon = n == tycon
-- typeContainsId (HsAppTy _ (unLoc -> t1) _) tycon = typeContainsId t1 tycon
-- typeContainsId _ _ = False

-- forM_
--     ( filter
--           (not . null)
--           [ map (hsib_body . unLoc) equations
--             | ((ClosedTypeFamily (Just equations)) :: FamilyInfo GhcPs) <- universeBi ast
--           ]
--     )
--     $ mapM_ (inlineHelper passId)

-- inlineHelper :: p ~ GhcPs => String -> FamEqn p (HsTyPats p) (LHsType p) -> R ()
-- inlineHelper passId (FamEqn _ (unLoc -> tfName) _ pats Prefix (unLoc -> rhs)) = do
--     conf <- ask
--
--     useTypeChecker
-- liftIO $
--     tryNewState
--         passId
--         ( \oldState ->
--               let oldAST = oldState ^. parsed
--                   newAST =
--                           transformBi (inlineAtType tfName pats rhs)
--                           $ oldAST
--                   newState =
--                       oldState
--                           & parsed .~ newAST
--                           & isAlive .~ (oldState ^. isAlive || oshow oldAST /= oshow newAST)
--                in newState
--         )
--         conf
-- inlineHelper _ _ = return ()

-- inlineAtType :: p ~ GhcPs => IdP p -> HsTyPats p -> HsType p -> HsType p -> HsType p
-- inlineAtType tfName pats rhs t@(HsAppTy _ tLHS _)
--     | tLHS `typeContainsId` tfName = unify rhs pats t
--     | otherwise = t
-- inlineAtType _ _ _ t = t
--
-- unify :: p ~ GhcPs => HsType p -> HsTyPats p -> HsType p -> HsType p
-- unify rhs pats (HsAppTy _ tLHS tRHS) = rhs

-- typeContainsId :: p ~ GhcPs => LHsType p -> IdP p -> Bool
-- typeContainsId (unLoc -> HsTyVar _ _ (unLoc -> typeName)) n = n == typeName
-- typeContainsId (unLoc -> HsAppTy _ lhs _) n = typeContainsId lhs n
-- typeContainsId _ _ = False

-- useTypeChecker :: R ()
-- useTypeChecker = do
--     conf <- ask
--     state <- liftIO $ readTVarIO (_tState conf)
--
--     let renamedAST = fromJust . tm_renamed_source . fromJust . _typechecked $ state
--         tcGlblEnv = fst . tm_internals_ . fromJust . _typechecked $ state
--         hEnv = _hscEnv state
--         loc = (\(RealSrcSpan r) -> r) . getLoc . _parsed $ state
--
--     forM_ [t | (t :: LHsType GhcRn) <- universeBi renamedAST] (transformBiM (liftIO . arst hEnv tcGlblEnv loc))

-- forM_ [t | (t :: LHsType GhcRn) <- universeBi renamedAST] (transformBiM (liftIO . arst hEnv tcGlblEnv loc) . (\t -> traceShow (oshow t) t))

-- arst :: Maybe HscEnv -> TcGblEnv -> RealSrcSpan -> LHsType GhcRn -> IO (LHsType GhcRn)
-- arst (Just hEnv) glblEnv loc t = do
--     CE.try (fmap snd (initTcWithGbl hEnv glblEnv loc (doStuff t))) >>= \case
--         Left (_ :: CE.SomeException) -> return ()
--         Right (Just tk) ->
--             when (oshow t /= "Type" && oshow t /= oshow tk)
--                 . putStrLn
--                 $ "lhsType2Type: " <> oshow t <> " -> " <> oshow tk
--         _ -> return ()
--     return t
-- arst _ _ _ _ = error "implement me"

-- see for every type that we normalise type family redexes inside of it
-- doStuff :: LHsType GhcRn -> TcM Type
-- doStuff t = do
--     famInsts <- tcGetFamInstEnvs
--     tcType <- fst <$> tcLHsType t
--     return . snd $ normaliseType famInsts Nominal tcType

-- inlineFunction :: (RdrName,  Located [LMatch GhcPs (LHsExpr GhcPs)]) -> R ()
-- inlineFunction (funName, lmatches) = liftIO . tryNewState "inlineFunctions" (parsed %~ transformBi (inlineFunctionHelper funName lmatches)) =<< ask
--
-- inlineFunctionHelper :: RdrName -> Located [LMatch GhcPs (LHsExpr GhcPs)] -> HsExpr GhcPs -> HsExpr GhcPs
-- inlineFunctionHelper funName (L l2 lmatches) old@(HsApp _ (L l1 (HsVar _ (L _ n))) expr)
--     | funName == n  = new
--     | otherwise     = old
--
--   where
--     nPats = length . m_pats . unLoc . head $ lmatches
--     nMatches = length lmatches
--             -- this is obviously not the best we can do
--             -- but I don't know how to handle n matches with m patterns yet
--     app = \con ctxt f -> HsApp NoExt (L l1 (HsPar NoExt (noLoc (con NoExt $ MG NoExt (L l2 $ map (changeMatchContext ctxt) (f lmatches)) FromSource)))) expr
--     new = case (nMatches, nPats) of
--             (1, 0) -> old -- eta reduced function, how to handle multiple guards?
--             (1, _) -> app HsLam LambdaExpr (take 1)
--             (_, 1) -> app HsLamCase CaseAlt id
--             _      -> old
-- inlineFunctionHelper _ _ old = old
--
-- changeMatchContext :: HsMatchContext RdrName -> LMatch GhcPs (LHsExpr GhcPs) -> LMatch GhcPs (LHsExpr GhcPs)
-- changeMatchContext ctxt (L l (Match _ _ p g)) = L l $ Match NoExt ctxt p g
-- changeMatchContext _ m = m
module Reduce.Passes.Extensions.TypeFamilies (rmvEquations, inline) where

import CoAxiom
import Control.Concurrent.STM
import qualified Control.Exception as CE
import Control.Monad.Reader
import Data.Generics.Uniplate.Data
import Data.Maybe
import FamInst
import FamInstEnv
import GHC
import TcHsType
import TcRnMonad
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

    _ <- fmap _parsed . liftIO . readTVarIO =<< asks _tState

    useTypeChecker

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

useTypeChecker :: R ()
useTypeChecker = do
    conf <- ask
    state <- liftIO $ readTVarIO (_tState conf)

    let renamedAST = fromJust . tm_renamed_source . fromJust . _typechecked $ state
        tcGlblEnv = fst . tm_internals_ . fromJust . _typechecked $ state
        hEnv = _hscEnv state
        loc = (\(RealSrcSpan r) -> r) . getLoc . _parsed $ state

    forM_ [t | (t :: LHsType GhcRn) <- universeBi renamedAST] (transformBiM (liftIO . arst hEnv tcGlblEnv loc))

-- forM_ [t | (t :: LHsType GhcRn) <- universeBi renamedAST] (transformBiM (liftIO . arst hEnv tcGlblEnv loc) . (\t -> traceShow (oshow t) t))

arst :: HscEnv -> TcGblEnv -> RealSrcSpan -> LHsType GhcRn -> IO (LHsType GhcRn)
arst hEnv glblEnv loc t = do
    CE.try (fmap snd (initTcWithGbl hEnv glblEnv loc (doStuff t))) >>= \case
        Left (_ :: CE.SomeException) -> return ()
        Right (Just tk) ->
            when (oshow t /= "Type" && oshow t /= oshow tk)
                . putStrLn
                $ "lhsType2Type: " <> oshow t <> " -> " <> oshow tk
        _ -> return ()
    return t

-- see for every type that we normalise type family redexes inside of it
doStuff :: LHsType GhcRn -> TcM Type
doStuff t = do
    famInsts <- tcGetFamInstEnvs
    tcType <- fst <$> tcLHsType t
    return . snd $ normaliseType famInsts Nominal tcType

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
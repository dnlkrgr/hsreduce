module Reduce.Passes.TypeFamilies where

import Debug.Trace
import Lexer
import Parser
import CoAxiom
-- import Outputable (Outputable)
-- import Data.List (isInfixOf)

import Control.Concurrent.STM.TVar.Lifted
import qualified Control.Exception as CE
import Control.Monad.Reader
import Data.Generics.Uniplate.Data
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import FamInst
import FamInstEnv
import GHC hiding (Pass)
import Parser.Parser
import Path
import TcHsType
import TcRnMonad
-- import TcRnTypes
import Util.Types
import Util.Util

familyResultSig :: Pass
familyResultSig = mkPass "familyResultSig" f
    where
        f :: WaysToChange (FamilyResultSig GhcPs)
        f (NoSig _) = []
        f (XFamilyResultSig _) = []
        f _ = [const (NoSig NoExt)]

rmvEquations :: Pass
rmvEquations = mkPass "typefamilies:rmvEquations" f
    where
        f :: WaysToChange (HsDecl GhcPs)
        f = handleSubList g p
            where
                p (TyClD _ FamDecl {tcdFam = d}) = case fdInfo d of
                    ClosedTypeFamily (Just equations) -> map getLoc equations
                    _ -> []
                p _ = []
                g loc (TyClD _ t@FamDecl {tcdFam = d}) = case fdInfo d of
                    ClosedTypeFamily (Just equations) ->
                        TyClD NoExt $ t {tcdFam = d {fdInfo = ClosedTypeFamily . Just $ filter ((/= loc) . getLoc) equations}}
                    _ -> TyClD NoExt t
                g _ t = t

apply :: R IO ()
apply = do
    conf <- ask
    state <- liftIO $ readTVarIO (_tState conf)

    let renamedAST = fromJust . tm_renamed_source . fromJust . _typechecked $ state

    forM_ [t | (t :: LHsType GhcRn) <- universeBi renamedAST] $ \t@(L l _) -> do
        state <- liftIO $ readTVarIO (_tState conf)

        -- GOTTA GO FAST
        let tcGlblEnv = fst . tm_internals_ . fromJust . _typechecked $ state
            hEnv = fromJust $ _hscEnv state
            loc = (\(RealSrcSpan r) -> r) . getLoc . _parsed $ state
            renamedAST = fromJust . tm_renamed_source . fromJust . _typechecked $ state
            dflags = fromJust $ _dflags state
            ast = _parsed state
            
        -- get change for location loc
        -- apply it to renamedAST

        -- overwriteAtLoc :: SrcSpan -> (a -> a) -> Located a -> Located a
        -- overwriteAtLoc loc f oldValue@(L oldLoc a)
        --     | loc == oldLoc = L loc $ f a
        --     | otherwise = oldValue

        -- mkPass :: Data a => String -> WaysToChange a -> Pass
        -- mkPass name pass = AST name (\ast -> concat [map (transformBi . overwriteAtLoc l) $ pass e | L l e <- universeBi ast])

        liftIO $ print $ oshow t
        liftIO (arst hEnv tcGlblEnv loc t) >>= \case
            Nothing -> return ()
            Just tk -> do
                let POk _ (unLoc -> newT) = runParser dflags parseType $ oshow tk
                liftIO $ print $ "newT: " <> oshow newT

                -- let (group, imports, _, _) = 
                --         transformBi (overwriteAtLoc l (const newT)) renamedAST
                let tChan = _tempDirs conf
                let newState = state { _parsed = transformBi ((\e -> traceShow ("OVERWRITTEN: " <> oshow e) e) . overwriteAtLoc l (const newT)) ast }
                liftIO $ print $ oshow l
                liftIO $ TIO.putStrLn $ showState newState

                -- liftIO $ print $ oshow group

                newState <- withTempDir tChan $ \tempDir -> do
                    let sourceFile = tempDir </> _sourceFile conf
                    let newFileContent = showState newState

                    liftIO $ TIO.writeFile (fromAbsFile sourceFile) newFileContent
                    liftIO $ parse sourceFile

                tryNewState "TypeFamilies:apply" $ const newState

-- get new renamed ast
-- write it to temp file
-- test it
-- if interesting, parse new state

-- forM_ [t | (t :: LHsType GhcRn) <- universeBi renamedAST] (transformBiM (liftIO . arst hEnv tcGlblEnv loc) . (\t -> traceShow (oshow t) t))

arst :: HscEnv -> TcGblEnv -> RealSrcSpan -> LHsType GhcRn -> IO (Maybe Type)
arst hEnv glblEnv loc t = do
    CE.try (fmap snd (initTcWithGbl hEnv glblEnv loc (doStuff t))) >>= \case
        Left (e :: CE.SomeException) -> return Nothing
        Right (Just tk) -> do
            -- when (oshow t /= "Type" && oshow t /= oshow tk)
            --     . putStrLn
            --     $ "lhsType2Type: " <> oshow t <> " ==> " <> oshow tk
            return $ Just tk
        _ -> return Nothing

-- see for every type that we normalise type family redexes inside of it
doStuff :: LHsType GhcRn -> TcM Type
doStuff t = do
    famInsts <- tcGetFamInstEnvs
    tcType <- fst <$> tcLHsType t
    let arst = snd $ normaliseType famInsts Nominal tcType
    -- liftIO $ print $ "doStuff: " <> oshow t <> " |-> " <> oshow arst
    return arst

-- apply :: Pass
-- apply = AST "typefamilies:apply" $ \ast ->
--     concatMap
--         ( \FamEqn {..} ->
--               let index = fst . head . filter (isContainedIn feqn_rhs . snd) $ zip [1 ..] feqn_pats
--                   tycon = unLoc feqn_tycon
--                in map
--                       ( \(L l _) ->
--                             let c =
--                                     if any (isContainedIn feqn_rhs) feqn_pats
--                                         then -- the rhs is one of the patterns
--                                         -- get the index of the pattern
--                                         -- find occurrences of the type family
--                                         -- replace them by nth pattern
--
--                                         -- traceShow (oshow tTemp)
--                                         -- . traceShow (oshow tycon)
--                                         -- . traceShow (oshow feqn_pats)
--                                         -- . traceShow (oshow feqn_rhs)
--                                         -- . traceShow (oshow $ takeNthArgument (length feqn_pats) index tTemp)
--                                             takeNthArgument (length feqn_pats) index
--                                         else replaceWithRHs tycon feqn_rhs
--                              in transformBi (overwriteAtLoc l c)
--                       )
--                       [t | (t :: LHsType GhcPs) <- universeBi ast, typeContainsTyCon tycon (unLoc t)]
--         )
--         [f | f@FamEqn {} :: FamEqn GhcPs (HsTyPats GhcPs) (LHsType GhcPs) <- universeBi ast]
--
-- isContainedIn :: (Outputable.Outputable a1, Outputable.Outputable a2) => a1 -> a2 -> Bool
-- isContainedIn feqn_rhs = (oshow feqn_rhs `isInfixOf`) . oshow
--
-- replaceWithRHs :: p ~ GhcPs => IdP p -> LHsType p -> HsType p -> HsType p
-- replaceWithRHs tycon (unLoc -> rhs) t
--     | not $ tycon `isContainedIn` rhs = rhs
--     | otherwise = t
--
-- -- the rhs is one of the patterns sub type expressions
-- -- get the index of the pattern
-- -- find occurrences of the sub type expression
-- -- replace them by the right hand side
--
-- -- TODO: see if we have a HsAppTy and count the arguments
-- takeNthArgument :: Int -> Int -> HsType GhcPs -> HsType GhcPs
-- takeNthArgument n i (HsAppTy _ (L _ a) (L _ b))
--     | n == i = b
--     | otherwise = takeNthArgument (n -1) i a
-- takeNthArgument _ _ t = t
--
-- typeContainsTyCon :: RdrName -> HsType GhcPs -> Bool
-- typeContainsTyCon tycon (HsTyVar _ _ (L _ name)) = tycon == name
-- typeContainsTyCon tycon (HsAppTy _ (L _ t) _) = typeContainsTyCon tycon t
-- typeContainsTyCon _ _ = False
module Reduce.Passes.TypeFamilies where

import CoAxiom
    ( Branches (unMkBranches),
      CoAxBranch (cab_rhs),
      CoAxiom (CoAxiom, co_ax_branches),
      Role (Nominal),
    )
import Control.Concurrent.STM.TVar.Lifted (readTVarIO)
import qualified Control.Exception as CE
import Control.Monad.Reader
    ( MonadIO (liftIO),
      MonadReader (ask),
      asks,
      forM_,
      void,
    )
import Data.Array (elems)
import Data.Char (isUpper)
import Data.Generics.Uniplate.Data
    ( transformBi,
      transformBiM,
      universeBi,
    )
import Data.IORef (modifyIORef)
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import FamInst (tcGetFamInstEnvs)
import FamInstEnv (normaliseType)
import GHC hiding (Pass)
import GhcPlugins
    ( HscEnv,
      Outputable,
      Role (Nominal),
      SrcSpan (RealSrcSpan),
      TyThing (ACoAxiom),
      getLoc,
      liftIO,
      nameEnvElts,
      unLoc,
    )
import Lens.Micro.Platform ((&), (.~), (^.))
import Lexer
import Parser
import Parser.Parser
import Path
import TcHsType
import TcInstDcls (tcInstDecls1)
import TcRnMonad
import UniqDFM (udfmToList)
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

-- from commit 66bb499
apply :: R IO ()
apply = do
    let passId = "apply type families"
    printInfo passId

    ast <- fmap _parsed . liftIO . readTVarIO =<< asks _tState

    void $ transformBiM applyHelper ast

isContainedIn :: (Outputable a1, Outputable a2) => a1 -> a2 -> Bool
isContainedIn feqn_rhs = (oshow feqn_rhs `isInfixOf`) . oshow

applyHelper :: p ~ GhcPs => FamEqn p (HsTyPats p) (LHsType p) -> R IO (FamEqn p (HsTyPats p) (LHsType p))
applyHelper f@(FamEqn {..}) = do
    conf <- ask
    ast <- liftIO . fmap _parsed $ readTVarIO (_tState conf)

    let index = fst . head . filter (isContainedIn feqn_rhs . snd) $ zip [1 ..] feqn_pats
        tycon = unLoc feqn_tycon

    forM_ [t | (t :: LHsType GhcPs) <- universeBi ast] $ \(L l _) -> do
        tryNewState
            "apply type families"
            ( \oldState ->
                  let oldAST = oldState ^. parsed
                      c =
                          if any (isContainedIn feqn_rhs) feqn_pats
                              then -- the rhs is one of the patterns
                              -- get the index of the pattern
                              -- find occurrences of the type family
                              -- replace them by nth pattern
                                  takeNthArgument tycon (length feqn_pats) index
                              else replaceWithRHs tycon feqn_rhs
                      newAST = transformBi (overwriteAtLoc l c) oldAST
                      newState =
                          oldState
                              & parsed .~ newAST
                   in if T.length (showState newState) < T.length (showState oldState)
                          then newState
                          else oldState
            )
    return f
-- the rhs is not found in any of the patterns
-- find occurrences of the type family
-- replace them by rhs
applyHelper f = return f

replaceWithRHs :: p ~ GhcPs => IdP p -> LHsType p -> HsType p -> HsType p
replaceWithRHs tycon (unLoc -> rhs) t
    | oshow tycon `isPrefixOf` oshow t = rhs
    | oshow tycon `isInfixOf` oshow t = rhs
    | otherwise = t

-- the rhs is one of the patterns sub type expressions
-- get the index of the pattern
-- find occurrences of the sub type expression
-- replace them by the right hand side

-- TODO: see if we have a HsAppTy and count the arguments
takeNthArgument :: p ~ GhcPs => IdP p -> Int -> Int -> HsType p -> HsType p
takeNthArgument tycon n i t
    | oshow tycon `isInfixOf` oshow t = takeNthArgumentHelper n i t
    | oshow tycon `isPrefixOf` oshow t = takeNthArgumentHelper n i t
    | otherwise = t

takeNthArgumentHelper :: Int -> Int -> HsType GhcPs -> HsType GhcPs
takeNthArgumentHelper n i (HsAppTy _ (L _ a) (L _ b))
    | n == i = b
    | otherwise = takeNthArgumentHelper (n -1) i a
takeNthArgumentHelper _ _ t = t

-- USING TYPE CHECKER
notWorking :: R IO ()
notWorking = do
    printInfo "TypeFamilies:apply"

    conf <- ask
    state <- liftIO $ readTVarIO (_tState conf)

    let renamedAST = fromJust . tm_renamed_source . fromJust . _typechecked $ state

    -- let tcGblEnv = fst . tm_internals_ . fromJust . _typechecked $ state
    --     hEnv = fromJust $ _hscEnv state
    --     loc = (\(RealSrcSpan r) -> r) . getLoc . _parsed $ state
    -- _ <- liftIO $ transformBiM (brst hEnv tcGblEnv loc) renamedAST

    forM_ [t | t :: LHsType GhcRn <- universeBi renamedAST, isUpper (head $ oshow t), length (words $ oshow t) >= 2] $ \t@(L l _) -> do
        -- forM_ [t | t :: LHsType GhcRn <- universeBi renamedAST, any (`isInfixOf` oshow t) ["LookupParam", "ReplaceArg", "If", "MaybeAdd", "ArgAt", "Unify", "Infer", "Indexed", "Zip", "++", "Param"] ] $ \t@(L l _) -> do
        -- forM_ [t | t :: LHsType GhcRn <- universeBi renamedAST] $ \t@(L l _) -> do
        state <- liftIO $ readTVarIO (_tState conf)

        -- GOTTA GO FAST
        let tcGblEnv = fst . tm_internals_ . fromJust . _typechecked $ state
            hEnv = fromJust $ _hscEnv state
            loc = (\(RealSrcSpan r) -> r) . getLoc . _parsed $ state
            dflags = fromJust $ _dflags state
            ast = _parsed state

        -- liftIO $ print "hallo"
        -- forM_ (tcg_fam_inst_env tcGblEnv) $ \i -> do
        --     liftIO $ print $ oshow $ udfmToList i
        -- forM_ (nameEnvElts $ tcg_type_env tcGblEnv) $ \case
        --     ACoAxiom (ca@CoAxiom {}) -> mapM_ (liftIO . print . oshow . cab_rhs) $ elems $ unMkBranches $ co_ax_branches ca
        --     _ -> return ()
        -- liftIO $ print $ oshow $ (tcg_inst_env tcGblEnv)
        -- liftIO $ print $ oshow $ (tcg_fam_inst_env tcGblEnv)
        -- liftIO $ print $ oshow $ (tcg_sigs tcGblEnv)
        -- liftIO $ print $ oshow $ udfmToList (tcg_fam_inst_env tcGblEnv)

        -- get change for location loc
        -- apply it to renamedAST

        -- overwriteAtLoc :: SrcSpan -> (a -> a) -> Located a -> Located a
        -- overwriteAtLoc loc f oldValue@(L oldLoc a)
        --     | loc == oldLoc = L loc $ f a
        --     | otherwise = oldValue

        -- mkPass :: Data a => String -> WaysToChange a -> Pass
        -- mkPass name pass = AST name (\ast -> concat [map (transformBi . overwriteAtLoc l) $ pass e | L l e <- universeBi ast])

        liftIO $ modifyIORef (_logRef conf) (oshow t :)

        liftIO (arst hEnv tcGblEnv loc t) >>= \case
            Nothing -> return ()
            Just tk -> do
                -- let (group, imports, _, _) =
                --         transformBi (overwriteAtLoc l (const newT)) renamedAST
                let POk _ (unLoc -> newT) = runParser dflags parseType $ oshow tk
                    nChars = 20
                    tChan = _tempDirs conf
                    newState = state {_parsed = transformBi (overwriteAtLoc l (const newT)) ast}
                    tString = oshow t
                    fromString = tString <> (replicate (nChars - length tString) ' ')
                    newTString = oshow newT
                    toString = (replicate (nChars - length newTString) ' ') <> newTString

                liftIO $ modifyIORef (_logRef conf) ((fromString <> " ===> " <> toString) :)
                -- liftIO $ print $ oshow l
                -- liftIO $ TIO.putStrLn $ showState newState

                -- liftIO $ print $ oshow group

                newState <- withTempDir tChan $ \tempDir -> do
                    let sourceFile = tempDir </> _sourceFile conf
                    let newFileContent = showState newState

                    liftIO $ TIO.writeFile (fromAbsFile sourceFile) newFileContent
                    liftIO $ parse sourceFile

                tryNewState "TypeFamilies:apply" $ const newState

brst :: HscEnv -> TcGblEnv -> RealSrcSpan -> [LInstDecl GhcRn] -> IO [LInstDecl GhcRn]
brst hEnv tcGblEnv loc instDecls = do
    initTcWithGbl hEnv tcGblEnv loc (crst instDecls)
    return instDecls

crst :: [LInstDecl GhcRn] -> TcM ()
crst instDecls = do
    (tcGblEnv, _, _) <- tcInstDecls1 instDecls
    liftIO $ print "hallo"
    -- forM_ (tcg_fam_inst_env tcGblEnv) $ \i -> do
    --     liftIO $ print $ oshow $ udfmToList i
    forM_ (nameEnvElts $ tcg_type_env tcGblEnv) $ \case
        ACoAxiom (ca@CoAxiom {}) -> mapM_ (liftIO . print . oshow . cab_rhs) $ elems $ unMkBranches $ co_ax_branches ca
        _ -> return ()
    liftIO $ print $ oshow $ (tcg_inst_env tcGblEnv)
    liftIO $ print $ oshow $ (tcg_fam_inst_env tcGblEnv)
    liftIO $ print $ oshow $ (tcg_sigs tcGblEnv)
    liftIO $ print $ oshow $ udfmToList (tcg_fam_inst_env tcGblEnv)
    error "ciao"
    return ()

arst :: HscEnv -> TcGblEnv -> RealSrcSpan -> LHsType GhcRn -> IO (Maybe Type)
arst hEnv glblEnv loc t = do
    -- liftIO $ print "##################################"
    -- liftIO $ print $ oshow t
    CE.try ((initTcWithGbl hEnv glblEnv loc (doStuff t))) >>= \case
        Left (_ :: CE.SomeException) -> return Nothing
        Right (messages, (Just mtk)) -> return $ Just mtk
        Right (messages, Nothing) -> do
            -- print $ oshow t
            -- liftIO $ print $ bagToList $ fst messages
            -- liftIO $ print $ bagToList $ snd messages
            -- liftIO $ print "##################################"
            return Nothing

-- when (oshow t /= "Type" && oshow t /= oshow tk)
--     . putStrLn
--     $ "lhsType2Type: " <> oshow t <> " ==> " <> oshow tk

-- see for every type that we normalise type family redexes inside of it
doStuff :: LHsType GhcRn -> TcM Type
doStuff t = do
    famInsts <- tcGetFamInstEnvs
    tcType <- fst <$> tcLHsType t
    let arst = snd $ normaliseType famInsts Nominal tcType
    return arst

-- liftIO . print $ oshow t

-- liftIO $ print "six"
-- tcType <- fst <$> tcLHsTypeUnsaturated t
-- liftIO $ print "one"
-- tcType <- fst <$> tcLHsType t
-- liftIO $ print "two"
-- tcType <- tcHsLiftedType t
-- liftIO $ print "three"
-- tcType <- tcHsOpenType t
-- liftIO $ print "four"
-- tcType <- tcHsLiftedTypeNC t
-- liftIO $ print "five"
-- tcType <- tcHsOpenTypeNC t

-- when ("IfEq" `isInfixOf` oshow t || "IfEq" `isInfixOf` oshow arst) $ do
--     liftIO . print $ oshow t
--     liftIO . print $ oshow arst

-- let brst = snd (normaliseType famInsts Representational tcType)
-- let crst = snd (normaliseType famInsts Phantom tcType)

-- liftIO $ print $ "doStuff: " <> oshow t <> " |-> " <> oshow arst
-- case (arst, brst, crst) of
--     (tk, _, _) -> undefined

-- #######################################
-- liftIO $ do
--     messages <- readIORef (_logRef conf)
--     putStrLn "proposed changes: "
--     forM_ messages putStrLn

-- get new renamed ast
-- write it to temp file
-- test it
-- if interesting, parse new state

-- forM_ [t | (t :: LHsType GhcRn) <- universeBi renamedAST] (transformBiM (liftIO . arst hEnv tcGblEnv loc) . (\t -> traceShow (oshow t) t))
-- initTcWithGbl :: HscEnv
--               -> TcGblEnv
--               -> RealSrcSpan
--               -> TcM r
--               -> IO (Messages, Maybe r)
-- initTcWithGbl hsc_env gbl_env loc do_this
--  = do { lie_var      <- newIORef emptyWC
--       ; errs_var     <- newIORef (emptyBag, emptyBag)
--       ; let lcl_env = TcLclEnv {
--                 tcl_errs       = errs_var,
--                 tcl_loc        = loc,     -- Should be over-ridden very soon!
--                 tcl_ctxt       = [],
--                 tcl_rdr        = emptyLocalRdrEnv,
--                 tcl_th_ctxt    = topStage,
--                 tcl_th_bndrs   = emptyNameEnv,
--                 tcl_arrow_ctxt = NoArrowCtxt,
--                 tcl_env        = emptyNameEnv,
--                 tcl_bndrs      = [],
--                 tcl_lie        = lie_var,
--                 tcl_tclvl      = topTcLevel
--                 }
--
--       ; maybe_res <- initTcRnIf 'a' hsc_env gbl_env lcl_env $
--                      do { r <- tryM do_this
--                         ; case r of
--                           Right res -> return (Just res)
--                           Left message    -> do
--                               liftIO $ do
--                                   print "message"
--                                   print message
--                               return Nothing }
--
--       ; liftIO $ print "isJust $ maybe_res"
--       ; liftIO $ print $ isJust maybe_res
--
--       -- Check for unsolved constraints
--       -- If we succeed (maybe_res = Just r), there should be
--       -- no unsolved constraints.  But if we exit via an
--       -- exception (maybe_res = Nothing), we may have skipped
--       -- solving, so don't panic then (#13466)
--       ; lie <- readIORef (tcl_lie lcl_env)
--
--       ; liftIO $ print "tcl_lie"
--       ; liftIO $ print $ oshow lie
--       ; liftIO $ print $ isEmptyWC lie
--
--       ; when (isJust maybe_res && not (isEmptyWC lie)) $
--         pprPanic "initTc: unsolved constraints" (ppr lie)
--
--         -- Collect any error messages
--       ; msgs <- readIORef (tcl_errs lcl_env)
--
--       ; liftIO $ print "errorsFound"
--       ; liftIO $ print $ errorsFound dflags msgs
--
--       ; let { final_res | errorsFound dflags msgs = Nothing
--                         | otherwise               = maybe_res }
--
--       ; return (msgs, final_res)
--       }
--   where dflags = hsc_dflags hsc_env

-- OLD STUFF THAT DOESN"T WORK SO WELL ON 16979
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
-- isContainedIn :: (Outputable a1, Outputable a2) => a1 -> a2 -> Bool
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
--
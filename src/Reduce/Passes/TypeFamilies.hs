module Reduce.Passes.TypeFamilies where

import Debug.Trace
import Data.Char (isUpper)
import Data.Generics.Uniplate.Data
import Data.List (isInfixOf, isPrefixOf)
import GHC hiding (Parsed, Pass, parser)
import GhcPlugins hiding ((<>))
import Reduce.Passes.Parameters
import Util.Types
import Util.Util
    
-- might become unused later
import Data.IORef (modifyIORef)
import Data.Maybe (fromJust)
import qualified Data.Text.IO as TIO
import FamInst (tcGetFamInstEnvs)
import FamInstEnv (normaliseType)
import StringBuffer ( stringToStringBuffer )
import CoAxiom
import Control.Concurrent.STM.TVar.Lifted (readTVarIO)
import qualified Control.Exception as CE
import Control.Monad.Reader
import Data.Array (elems)
import Lexer
import Parser
import Parser.Parser
import Path
import TcRnTypes
import TcHsType
import TcInstDcls (tcInstDecls1)
import TcRnMonad
import UniqDFM (udfmToList)


rmvUnusedParams :: Pass
rmvUnusedParams =
    reduce
        "TypeFamilies.rmvUnusedParams"
        (\ast -> [(unLoc $ fdLName tcdFam, length . hsq_explicit $ fdTyVars tcdFam) | FamDecl {..} :: TyClDecl GhcPs <- universeBi ast])
        getPatsLength
        ( \famId _ _ newLenArgs newI ->
              transformBi (handleFamEqn famId newI)
              . transformBi (handleFamDecl famId newI)
              . transformBi (rmvArgsFromType famId newLenArgs newI)
        )

handleFamEqn :: GhcPs ~ p => RdrName -> Int -> FamEqn p (HsTyPats p) (LHsType p) -> FamEqn p (HsTyPats p) (LHsType p) 
handleFamEqn name i f@FamEqn {..}
    | unLoc feqn_tycon == name = let a = f {feqn_pats = deleteAt i feqn_pats} in traceShow (gshow a) a
    | otherwise = f
handleFamEqn _ _ d = d


-- n: total number of patterns
-- i: index of pattern we wish to remove
rmvArgsFromType :: RdrName -> Int -> Int -> HsType GhcPs -> HsType GhcPs
rmvArgsFromType conId n i e@(HsAppTy x la@(L _ a) b)
   | typeContainsId conId e,
     typeFitsNumberOfPatterns n e,
     n == i =
       a
   | typeContainsId conId e,
     typeFitsNumberOfPatterns n e =
       HsAppTy x (rmvArgsFromType conId (n - 1) i <$> la) b
   | otherwise = e
rmvArgsFromType _ _ _ e = e

typeFitsNumberOfPatterns :: Int -> HsType GhcPs -> Bool
typeFitsNumberOfPatterns n (HsAppTy _ l _) = typeFitsNumberOfPatterns (n -1) (unLoc l)
typeFitsNumberOfPatterns 0 _ = True
typeFitsNumberOfPatterns _ _ = False

typeContainsId :: RdrName -> HsType GhcPs -> Bool
typeContainsId n (HsAppTy _ (L _ (HsTyVar _ _ (L _ a))) _) = n == a
typeContainsId n (HsAppTy _ (L _ e) _) = typeContainsId n e
typeContainsId _ _ = False

getPatsLength :: RdrName -> ParsedSource -> [Int]
getPatsLength name ast =
    [ length . hsq_explicit $ fdTyVars tcdFam
      | FamDecl {..} :: TyClDecl GhcPs <- universeBi ast,
        name == unLoc (fdLName tcdFam)
    ]

-- handleFamDecl
handleFamDecl :: GhcPs ~ p => RdrName -> Int -> TyClDecl p -> TyClDecl p
handleFamDecl name i f@FamDecl {..}
    | unLoc (fdLName tcdFam) == name,
      HsQTvs {} <- fdTyVars tcdFam =
        f {tcdFam = tcdFam {fdTyVars = HsQTvs NoExt . deleteAt i . hsq_explicit $ fdTyVars tcdFam}}
    | otherwise = f
handleFamDecl _ _ d = d

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
apply :: Pass
apply = AST "TypeFamilies:apply" $ \oldAST ->
    concatMap
        (applyHelper oldAST)
        [f | f@FamEqn {} :: FamEqn GhcPs (HsTyPats GhcPs) (LHsType GhcPs) <- universeBi oldAST]

applyHelper :: p ~ GhcPs => ParsedSource -> FamEqn p (HsTyPats p) (LHsType p) -> [ParsedSource -> ParsedSource]
applyHelper ast (FamEqn {..}) =
    let index = fst . head . filter (isContainedIn feqn_rhs . snd) $ zip [1 ..] feqn_pats
        tycon = unLoc feqn_tycon
     in map
            ( \(L l _) oldAST ->
                  let c =
                          if any (isContainedIn feqn_rhs) feqn_pats
                              then -- the rhs is one of the patterns
                              -- get the index of the pattern
                              -- find occurrences of the type family
                              -- replace them by nth pattern
                                  takeNthArgument tycon (length feqn_pats) index
                              else replaceWithRHs tycon feqn_rhs
                      newAST = transformBi (overwriteAtLoc l c) oldAST
                   in if length (oshow newAST) < length (oshow oldAST)
                          then newAST
                          else oldAST
            )
            [t | t :: LHsType GhcPs <- universeBi ast, length (words (oshow t)) >= 2, let first = head . words $ oshow t, isUpper (head first) || head first == '\'']
-- the rhs is not found in any of the patterns
-- find occurrences of the type family
-- replace them by rhs
applyHelper _ _ = []

isContainedIn :: (Outputable a1, Outputable a2) => a1 -> a2 -> Bool
isContainedIn feqn_rhs = (oshow feqn_rhs `isInfixOf`) . oshow

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

    renamedAST <- fromJust . tm_renamed_source . _typechecked <$> (liftIO $ readTVarIO (_tState conf))

    forM_ [t | t :: LHsType GhcRn <- universeBi renamedAST, isUpper (head $ oshow t), length (words $ oshow t) >= 2] $ \t@(L l _) -> do
        state <- liftIO $ readTVarIO (_tState conf)

        -- GOTTA GO FAST
        let tcGblEnv = fst . tm_internals_ . _typechecked $ state
            hEnv = _hscEnv state
            loc = (\(RealSrcSpan r) -> r) . getLoc . _parsed $ state
            tempFlags = _dflags state
            ast = _parsed state


        liftIO $ modifyIORef (_logRef conf) (oshow t :)

        liftIO (arst hEnv tcGblEnv loc t) >>= \case
            Nothing -> return ()
            Just tk -> do
                let POk _ (unLoc -> newT) = runParser tempFlags parseType $ oshow tk
                    nChars = 20
                    tChan = _tempDirs conf
                    midState = state {_parsed = transformBi (overwriteAtLoc l (const newT)) ast}
                    tString = oshow t
                    fromString = tString <> (replicate (nChars - length tString) ' ')
                    newTString = oshow newT
                    toString = (replicate (nChars - length newTString) ' ') <> newTString

                liftIO $ modifyIORef (_logRef conf) ((fromString <> " ===> " <> toString) :)

                newState <- withTempDir tChan $ \tempDir -> do
                    let sourceFile = tempDir </> _sourceFile conf
                    let newFileContent = showState Parsed midState

                    liftIO $ TIO.writeFile (fromAbsFile sourceFile) newFileContent
                    liftIO $ parse sourceFile

                tryNewState "TypeFamilies:apply" $ const newState

brst :: HscEnv -> TcGblEnv -> RealSrcSpan -> [LInstDecl GhcRn] -> IO [LInstDecl GhcRn]
brst hEnv tcGblEnv loc instDecls = do
    _ <- initTcWithGbl hEnv tcGblEnv loc (crst instDecls)
    return instDecls

crst :: [LInstDecl GhcRn] -> TcM ()
crst instDecls = do
    (tcGblEnv, _, _) <- tcInstDecls1 instDecls
    liftIO $ print @String "hallo"
    forM_ (nameEnvElts $ tcg_type_env tcGblEnv) $ \case
        ACoAxiom (ca@CoAxiom {}) -> mapM_ (liftIO . print . oshow . cab_rhs) $ elems $ unMkBranches $ co_ax_branches ca
        _ -> return ()
    liftIO $ print $ oshow $ (tcg_inst_env tcGblEnv)
    liftIO $ print $ oshow $ (tcg_fam_inst_env tcGblEnv)
    liftIO $ print $ oshow $ (tcg_sigs tcGblEnv)
    liftIO $ print $ oshow $ udfmToList (tcg_fam_inst_env tcGblEnv)
    error "ciao"

arst :: HscEnv -> TcGblEnv -> RealSrcSpan -> LHsType GhcRn -> IO (Maybe Type)
arst hEnv glblEnv loc t = do
    CE.try ((initTcWithGbl hEnv glblEnv loc (doStuff t))) >>= \case
        Left (_ :: CE.SomeException) -> return Nothing
        Right (_, (Just mtk)) -> return $ Just mtk
        Right (_, Nothing) -> return Nothing

-- see for every type that we normalise type family redexes inside of it
doStuff :: LHsType GhcRn -> TcM Type
doStuff t = do
    famInsts <- tcGetFamInstEnvs
    tcType <- fst <$> tcLHsType t
    return . snd $ normaliseType famInsts Nominal tcType


runParser :: DynFlags -> P a -> String -> ParseResult a
runParser flags parser str = unP parser parseState
    where
      location = mkRealSrcLoc (mkFastString "<interactive>") 1 1
      buffer = stringToStringBuffer str
      parseState = mkPState flags buffer location
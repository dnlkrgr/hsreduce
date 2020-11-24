module Util.Util where

import Data.Word
import DynFlags hiding (GhcMode)
import Control.Applicative
import Control.Concurrent.Async.Lifted (forConcurrently_)
import Control.Concurrent.STM.Lifted
    ( STM,
      TChan,
      atomically,
      modifyTVar,
      readTChan,
      readTVar,
      readTVarIO,
      writeTChan,
      writeTVar,
    )
import qualified Control.Exception.Lifted as CE
import Control.Monad.Reader
import Data.Aeson (decodeStrict)
import Data.Algorithm.Diff (PolyDiff (Both), getDiff)
import Data.Char (isAlphaNum)
import Data.Data (Data (gmapQ, toConstr), showConstr)
import Data.Either (fromRight, isRight)
import Data.Generics.Aliases (extQ)
import Data.Generics.Uniplate.Data
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe, isJust)
import Data.String (IsString (fromString))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Debug.Trace (traceShow)
import FastString (mkFastString)
import GHC hiding (Parsed, Severity, Pass, GhcMode, Renamed)
    
import Katip
    ( Severity (DebugS, ErrorS, InfoS, NoticeS, WarningS),
      logTM,
    )
import Lens.Micro.Platform ((%~), (&), at)
import Outputable hiding ((<>))
import Parser.Parser
import Path
    ( (</>),
      Abs,
      Dir,
      File,
      Path,
      filename,
      fromAbsDir,
      fromAbsFile,
      fromRelFile,
      parent,
    )
import SrcLoc as SL
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process
    ( CreateProcess (cwd),
      readCreateProcessWithExitCode,
      shell,
    )
import System.Timeout.Lifted (timeout)
import Util.Types
import qualified Util.Types as UT

getASTLengthDiff :: RState -> RState -> Integer
getASTLengthDiff newState oldState = getASTLength newState - getASTLength oldState

getASTLength :: RState -> Integer
getASTLength = fromIntegral . T.length . showState Parsed 

getTokenDiff :: (MonadIO m, Num a) => RState -> RState -> m a
getTokenDiff newState oldState = 
    liftIO $ do
        mn1 <- fmap fromIntegral <$> (countTokens $ T.unpack $ showState Parsed newState)
        mn2 <- fmap fromIntegral <$> (countTokens $ T.unpack $ showState Parsed oldState)
        pure $ fromMaybe 0 $ (-) <$> mn1 <*> mn2

tryNewState :: String -> (RState -> RState) -> R IO ()
tryNewState passId f = do
    conf <- ask
    oldState <- liftIO . readTVarIO $ _tState conf

    let newState = f oldState
        oldStateS = showState Parsed oldState
        newStateS = showState Parsed newState
        isStateNew = oldStateS /= newStateS
        sizeDiff = getASTLengthDiff newState oldState

    -- passes might have produced garbage, so we need to be extra careful here
    CE.try (do
        tokenDiff <- getTokenDiff newState oldState

        if isStateNew
            then do
                testNewState conf newState >>= \case
                    Interesting -> do
                        success <- atomically $ do
                            currentCommonStateS <- showState Parsed <$> (readTVar $ _tState conf)

                            -- is the state we worked still the same?
                            if oldStateS == currentCommonStateS
                                then do
                                    writeTVar (_tState conf) $ newState {_isAlive = True}

                                    updateStatisticsHelper conf passId 1 sizeDiff tokenDiff
                                    return True
                                else return False

                        if success
                            then logStateDiff NoticeS oldStateS newStateS
                            else tryNewState passId f
                    Uninteresting -> do
                        when (_debug conf) $ logStateDiff DebugS oldStateS newStateS
                        updateStatistics conf passId 0 0 0
            else updateStatistics conf passId 0 0 0) >>= \case
        Left (_ :: CE.SomeException) -> traceShow ("tryNewState / isStateNew, EXCEPTION:" :: String) $ pure ()
        _ -> pure ()

logStateDiff :: Severity -> T.Text -> T.Text -> R IO ()
logStateDiff severity oldStateS newStateS = do
    let title = case severity of
            DebugS -> "TRIED CHANGE"
            _ -> "APPLIED CHANGE"

    $(logTM) severity title
    forM_
        ( filter
              ( \case
                    Both l r -> l /= r
                    _ -> True
              )
              $ getDiff (T.lines oldStateS) (T.lines newStateS)
        )
        $ \line ->
            $(logTM) severity (fromString (show line))

overwriteAtLoc :: SrcSpan -> (a -> a) -> Located a -> Located a
overwriteAtLoc loc f oldValue@(L oldLoc a)
    | loc == oldLoc = L loc $ f a
    | otherwise = oldValue

mkPass :: Data a => String -> WaysToChange a -> Pass
mkPass name pass = AST name (\ast -> concat [map (transformBi . overwriteAtLoc l) $ pass e | L l e <- universeBi ast])

overwriteWithEq :: Eq a => a -> (a -> a) -> a -> a
overwriteWithEq a f oldValue
    | a == oldValue = f oldValue
    | otherwise = oldValue

mkCabalPass :: (Eq a, Data a) => String -> WaysToChange a -> Pass
mkCabalPass name pass = CabalPass name (\ast -> concat [map (transformBi . overwriteWithEq e) $ pass e | e <- universeBi ast])

runPass :: Pass -> R IO ()
runPass (AST name pass) = do
    printInfo name
    -- unless isInProduction $ do
    --     isTestStillFresh name

    ask
    >>= fmap (map (parsed %~) . pass . _parsed) . liftIO . readTVarIO . _tState
    >>= applyInterestingChanges name
runPass (CabalPass name pass) = do
    printInfo name
    -- unless isInProduction $ do
    --     isTestStillFresh name

    ask
    >>= fmap (map (pkgDesc %~) . pass . _pkgDesc) . liftIO . readTVarIO . _tState
    >>= applyInterestingChanges name
runPass _ = return ()

applyInterestingChanges :: String -> [RState -> RState] -> R IO ()
applyInterestingChanges name proposedChanges = do
    $(logTM) InfoS (fromString $ "# proposed changes: " <> (show $ length proposedChanges))

    numberOfThreads <- asks _numberOfThreads

    let chunkSize = div (length proposedChanges) numberOfThreads
        -- length of proposedChanges might be smaller than number of threads which might
        -- result in chunkSize of 0, so we need to check for that!
        batches = case chunkSize of
            0 -> [proposedChanges]
            _ ->
                let temp = chunksOf chunkSize proposedChanges
                 in case reverse temp of
                        (x : y : xs) ->
                            if length temp > numberOfThreads
                                then (x <> y) : xs
                                else temp
                        _ -> temp

    $(logTM) InfoS (fromString $ "# batches: " <> (show $ length batches))
    $(logTM) InfoS (fromString $ "batch sizes: " <> (show $ map length batches))

    -- run one thread for each batch
    forConcurrently_ batches $ \batch -> do
        -- within a thread check out all the changes
        forM_ batch $ \c -> tryNewState name c

updateStatistics :: RConf -> String -> Word64 -> Integer -> Integer -> R IO ()
updateStatistics conf name i sizeDiff tokenDiff = atomically $ updateStatisticsHelper conf name i sizeDiff tokenDiff

-- 1. if the interestingness test was successful:
--      a) increment number of successful invocations
--      b) increment number of total invocations
--      c) add current number of removed bytes to the total
--         number of removed bytes of this pass
-- 2. if the interestingness test failed:
--      a) increment number of total invocations
updateStatisticsHelper :: RConf -> String -> Word64 -> Integer -> Integer -> STM ()
updateStatisticsHelper conf name i sizeDiff tokenDiff =
    modifyTVar (_tState conf) $ \s ->
        s & statistics . passStats . at name %~ \case
            Nothing -> Just $ PassStats name i 1 sizeDiff tokenDiff
            Just (PassStats _ n d r t) ->
                Just $ PassStats name (n + i) (d + 1) (r + sizeDiff) (t + tokenDiff)

testNewState :: RConf -> RState -> R IO Interesting
testNewState conf newState =
    withTempDir (_tempDirs conf) $ \tempDir -> do
        let sourceFile = tempDir </> _sourceFile conf
            test = tempDir </> _test conf

        CE.try
            ( do
                  liftIO . TIO.writeFile (fromAbsFile sourceFile) $ showState Parsed newState
                  runTest test (UT._timeout conf)
            )
            >>= \case
                Left (_ :: CE.SomeException) -> traceShow ("testNewState, EXCEPTION:" :: String) $ return Uninteresting
                Right i -> return i

runTest :: Path Abs File -> Word64 -> R IO Interesting
runTest test duration = do
    let dirName = parent test
    let testName = filename test

    CE.try (timeout (fromIntegral duration) $ liftIO $ flip readCreateProcessWithExitCode "" $ (shell $ "./" <> fromRelFile testName) {cwd = Just $ fromAbsDir dirName}) >>= \case
        Left (_ :: CE.SomeException) -> traceShow ("runTest, EXCEPTION:" :: String) $ return Uninteresting
        Right m -> case m of
            Nothing -> do
                $(logTM) WarningS "runTest: timed out"
                return Uninteresting
            Just (exitCode, _, _) -> return $ case exitCode of
                ExitFailure _ -> Uninteresting
                ExitSuccess -> Interesting

printInfo :: String -> R IO ()
printInfo context = do
    tState <- asks _tState
    oldState <- liftIO . atomically $ readTVar tState

    let info = "state size: " <> (show . T.length . showState Parsed $ oldState)

    $(logTM) InfoS "****************************************"
    $(logTM) InfoS (fromString context)
    $(logTM) InfoS (fromString info)

isTestStillFresh :: String -> R IO ()
isTestStillFresh context = do
    conf <- ask
    newState <- liftIO . atomically $ readTVar $ _tState conf
    testNewState conf newState >>= \case
        Uninteresting -> do
            results <- replicateM 3 (testNewState conf newState)
            when (all (== Uninteresting) results) $ do
                liftIO . TIO.writeFile ((fromRelFile $ _sourceFile conf) <> ".stale_state") $ showState Parsed newState
                $(logTM) ErrorS (fromString $ "potential hsreduce bug: Test case is broken at >>>" <> context <> "<<<")
                error $ 
                    "potential hsreduce bug: Test case is broken at >>>" <> context <> "<<<"
                    <> "\n" 
                    <> "sourceFile: " <> show (_sourceFile conf)
        _ -> return ()

-- get ways to change an expression that contains a list as an subexpression
-- p: preprocessing (getting to the list)
-- f: filtering and returning a valid expression again
handleSubList :: Eq e => (e -> a -> a) -> (a -> [e]) -> WaysToChange a
handleSubList f p = map f . p

gshow :: Data a => a -> String
gshow x = gshows x ""

gshows :: Data a => a -> ShowS
gshows = render `extQ` (shows :: String -> ShowS)
    where
        render t
            -- \| isTuple = showChar '('
            --           . drop 1
            --           . commaSlots
            --           . showChar ')'
            -- \| isNull = showString "[]"
            | isList =
                showChar '['
                    . drop 1
                    . listSlots
                    . showChar ']'
            | otherwise =
                showChar '('
                    . constructor
                    . slots
                    . showChar ')'
            where
                constructor = showString . showConstr . toConstr $ t
                slots = foldr (.) id . gmapQ ((showChar ' ' .) . gshows) $ t
                -- commaSlots  = foldr (.) id . gmapQ ((showChar ',' .) . gshows) $ t
                listSlots = foldr (.) id . init . gmapQ ((showChar ',' .) . gshows) $ t
                -- isTuple     = all (==',') (filter (not . flip elem "()") (constructor ""))
                -- isNull      = null (filter (not . flip elem "[]") (constructor ""))
                isList = constructor "" == "(:)"

withTempDir :: TChan (Path Abs Dir) -> (Path Abs Dir -> R IO a) -> R IO a
withTempDir tChan action = do
    tempDir <- atomically $ readTChan tChan
    a <- action tempDir
    atomically $ writeTChan tChan tempDir
    return a

-- | run ghc with -Wunused-binds -ddump-json and delete decls that are mentioned there
getGhcOutput :: GhcMode -> Path Abs File -> IO (Maybe [(T.Text, SL.RealSrcSpan)])
getGhcOutput ghcMode sourcePath = do
    allPragmas <- getPragmas sourcePath

    let dirName = parent sourcePath
        fileName = filename sourcePath
        command = "ghc " <> ghcModeString <> " -ddump-json " <> unwords (("-X" ++) . T.unpack . showExtension <$> allPragmas) <> " " <> fromRelFile fileName

    (_, stdout, _) <-
        fromMaybe (error "getGhcOutput timeout!")
            <$> timeout (fromIntegral @Word64 10 * 60 * 1000 * 1000) (flip readCreateProcessWithExitCode "" ((shell command) {cwd = Just $ fromAbsDir dirName}))

    return $ case stdout of
        "" -> Nothing
        _ -> fromRight [] . sequence . filter isRight . concat . mapFunc . filterFunc . filter (isJust . UT.span) <$> (sequence . filter isJust . map (decodeStrict . TE.encodeUtf8) . T.lines . T.pack $ stdout)
    where
        ghcModeString = case ghcMode of
            UnusedBinds -> "-Wunused-binds"
            UnusedImports -> "-Wunused-imports"
            _ -> ""
        filterFunc = case ghcMode of
            MissingImport -> filter ((T.isPrefixOf "Not in scope:" <&&> (T.isInfixOf "imported from" <||> T.isInfixOf "No module named")) . doc)
            NotInScope -> filter (T.isPrefixOf "Not in scope:" . doc)
            PerhapsYouMeant -> id -- filter ((T.isPrefixOf "Not in scope:" <&&> (T.isInfixOf "Perhaps you meant")) . doc)
            HiddenImport -> filter (T.isInfixOf "is a hidden module in the package" . doc)
            Indent -> filter (T.isInfixOf "parse error (possibly incorrect indentation" . doc)
            _ -> filter ((isJust . UT.span) <&&> (isJust . reason) <&&> (maybe False (T.isPrefixOf "Opt_WarnUnused") . reason))
        mapFunc = case ghcMode of
            MissingImport ->
                map
                    ( \o ->
                          let importSuggestion = fmap (removeInternal id) . useP importedFromP $ doc o
                              noModuleNamed = fmap (removeInternal id) . useP noModuleNamedP $ doc o
                              pos = UT.span o
                           in maybe [] (\jpos -> [(,span2SrcSpan jpos) <$> importSuggestion, (,span2SrcSpan jpos) <$> noModuleNamed]) pos
                    )
            NotInScope -> map (\o -> maybe [] (\jpos -> [fmap ((,span2SrcSpan jpos) . removeUseOfHidden) . useP notInScopeP $ doc o]) $ UT.span o)
            PerhapsYouMeant -> map (\o -> maybe [] (\jpos -> [fmap (,span2SrcSpan jpos) . useP perhapsYouMeantP $ doc o]) $ UT.span o)
            Indent -> map (\o -> maybe [] (\jpos -> [Right ("", span2SrcSpan jpos)]) $ UT.span o)
            HiddenImport -> map (\o -> maybe [] (\jpos -> [fmap ((,span2SrcSpan jpos) . (removeInternal init)) . useP hiddenImportP $ doc o]) $ UT.span o)
            _ -> map (\o -> maybe [] (\jpos -> [(Right $ ((T.takeWhile (/= '’') . T.drop 1 . T.dropWhile (/= '‘') $ doc o), (span2SrcSpan jpos)))]) $ UT.span o)

getStuffFromGhc :: GhcMode -> R IO (Maybe [T.Text])
getStuffFromGhc mode = do
    conf <- ask
    let sourceFilePath = _sourceFile conf
    mstuff <- withTempDir (_tempDirs conf) $ \dir -> 
        liftIO $ getGhcOutput mode (dir </> sourceFilePath)
    pure $ map fst <$> mstuff


span2SrcSpan :: Span -> SL.RealSrcSpan
span2SrcSpan (Span f sl' sc el ec) = SL.mkRealSrcSpan (SL.mkRealSrcLoc n sl' sc) (SL.mkRealSrcLoc n el ec)
    where
        n = mkFastString $ T.unpack f

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)

infixr 8 <&&>


mshow :: Outputable a => DynFlags -> a -> String
mshow dynflags = showSDocForUser dynflags alwaysQualify . ppr

oshow :: Outputable a => a -> String
oshow = showSDocUnsafe . ppr

banner :: MonadIO m => String -> m ()
banner s = liftIO $ putStrLn $ "\n" <> s' <> s <> s'
    where
        n = 80 - length s
        s' = replicate (div n 2) '='

lshow :: Outputable a => Located a -> String
lshow = showSDocUnsafe . ppr . unLoc

-- | too naive check if something is an operator
-- TODO: use syntax from Haskell2010 report
isOperator :: String -> Bool
isOperator = not . any isAlphaNum

-- delete at Index i, starting from 1 not from 0
deleteAt :: Int -> [a] -> [a]
deleteAt i as = take (i -1) as <> drop i as

exprContainsId :: RdrName -> HsExpr GhcPs -> Bool
exprContainsId n (HsApp _ (L _ (HsVar _ (L _ a))) _) = n == a
exprContainsId n (HsApp _ (L _ e) _) = exprContainsId n e
exprContainsId _ _ = False

-- n: total number of patterns
-- i: index of pattern we wish to remove
rmvArgsFromExpr :: RdrName -> Int -> Int -> HsExpr GhcPs -> HsExpr GhcPs
rmvArgsFromExpr conId n i e@(HsApp x la@(L _ a) b)
    | exprContainsId conId e,
      exprFitsNumberOfPatterns n e,
      n == i =
        a
    | exprContainsId conId e,
      exprFitsNumberOfPatterns n e =
        HsApp x (rmvArgsFromExpr conId (n - 1) i <$> la) b
    | otherwise = e
rmvArgsFromExpr _ _ _ e = e

exprFitsNumberOfPatterns :: Int -> HsExpr GhcPs -> Bool
exprFitsNumberOfPatterns n (HsApp _ l _) = exprFitsNumberOfPatterns (n -1) (unLoc l)
exprFitsNumberOfPatterns 0 _ = True
exprFitsNumberOfPatterns _ _ = False

handleSigs :: RdrName -> Int -> Sig GhcPs -> Sig GhcPs
handleSigs funId i ts@(TypeSig _ [sigId] (HsWC _ (HsIB _ (L l t))))
    | funId == unLoc sigId = TypeSig NoExt [sigId] . HsWC NoExt . HsIB NoExt . L l $ handleTypes i t
    | otherwise = ts
handleSigs funId i ts@(ClassOpSig _ b [sigId] (HsIB _ (L l t)))
    | funId == unLoc sigId = ClassOpSig NoExt b [sigId] . HsIB NoExt . L l $ handleTypes i t
    | otherwise = ts
handleSigs _ _ d = d

handleTypes :: Int -> HsType GhcPs -> HsType GhcPs
handleTypes 1 (HsFunTy _ _ (L _ t)) = t
handleTypes i (HsFunTy x a lt) = HsFunTy x a (handleTypes (i -1) <$> lt)
handleTypes _ t = t

handleFunBinds :: RdrName -> Int -> HsBind GhcPs -> HsBind GhcPs
handleFunBinds funId i (FunBind _ bindId (MG _ (L l m) o) a b)
    | funId == unLoc bindId = FunBind NoExt bindId (MG NoExt (L l (handleMatches i m)) o) a b
handleFunBinds _ _ b = b

handleMatches :: Int -> [LMatch GhcPs (LHsExpr GhcPs)] -> [LMatch GhcPs (LHsExpr GhcPs)]
handleMatches i mg = [L l (Match NoExt ctxt (f pats) grhss) | L l (Match _ ctxt pats grhss) <- mg]
    where
        f =
            map snd
                . filter ((/= i) . fst)
                . zip [1 ..]
module Util.Util where

import StringBuffer
import Lexer
import DynFlags hiding (GhcMode)
import Control.Applicative
    ( Alternative ((<|>)),
      Applicative (liftA2),
    )
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
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader
    ( MonadReader (ask),
      asks,
      forM_,
      replicateM,
      unless,
      void,
      when,
    )
import Data.Aeson (decodeStrict)
import Data.Algorithm.Diff (PolyDiff (Both), getDiff)
import Data.Char (isAlphaNum)
import Data.Data (Data (gmapQ, toConstr), showConstr)
import Data.Either (fromRight, isRight)
import Data.Generics.Aliases (extQ)
import Data.Generics.Uniplate.Data (transformBi, universeBi)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe, isJust)
import Data.String (IsString (fromString))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Debug.Trace (traceShow)
import FastString (mkFastString)
import GHC
    ( GenLocated (L),
      GhcPs,
      HsExpr (HsApp, HsVar),
      Located,
      ParsedSource,
      RdrName,
      SrcSpan,
      unLoc,
    )
import Katip
    ( Severity (DebugS, ErrorS, InfoS, NoticeS, WarningS),
      logTM,
    )
import Lens.Micro.Platform ((%~), (&), at)
import Outputable ( Outputable(ppr), showSDocUnsafe )
import Parser.Parser (getPragmas)
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
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import Util.Types
    ( GhcMode (..),
      GhcOutput (doc, reason),
      Interesting (..),
      Parser,
      Pass (AST),
      PassStats (PassStats),
      R,
      RConf (_numberOfThreads, _sourceFile, _tState, _tempDirs, _test),
      RState (_isAlive, _parsed),
      Span (Span),
      Tool (..),
      WaysToChange,
      parsed,
      passStats,
      showExtension,
      showState,
      statistics,
    )
import qualified Util.Types as UT

tryNewState :: String -> (RState -> RState) -> R IO ()
tryNewState passId f = do
    conf <- ask
    oldState <- liftIO . readTVarIO $ _tState conf

    let newState = f oldState
        oldStateS = showState oldState
        newStateS = showState newState
        isStateNew = oldStateS /= newStateS
        sizeDiff = T.length newStateS - T.length oldStateS

    if isStateNew
        then do
            testNewState conf newState >>= \case
                Interesting -> do
                    success <- atomically $ do
                        currentCommonStateS <- showState <$> (readTVar $ _tState conf)

                        -- is the state we worked still the same?
                        if oldStateS == currentCommonStateS
                            then do
                                writeTVar (_tState conf) $ newState {_isAlive = True}

                                updateStatistics_ conf passId 1 sizeDiff
                                return True
                            else return False

                    if success
                        then logStateDiff NoticeS oldStateS newStateS
                        else tryNewState passId f
                Uninteresting -> do
                    -- logStateDiff DebugS oldStateS newStateS
                    updateStatistics conf passId 0 0
        else updateStatistics conf passId 0 0

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

runPass :: Pass -> R IO ()
runPass (AST name pass) =
    do
        unless isInProduction $ do
            printInfo name
            isTestStillFresh name

        ask
        >>= fmap (pass . _parsed) . liftIO . readTVarIO . _tState
        >>= applyInterestingChanges name
runPass _ = return ()

applyInterestingChanges :: String -> [ParsedSource -> ParsedSource] -> R IO ()
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
        forM_ batch $ \c -> tryNewState name (parsed %~ c)

updateStatistics :: RConf -> String -> Word -> Int -> R IO ()
updateStatistics conf name i sizeDiff = atomically $ updateStatistics_ conf name i sizeDiff

-- 1. if the interestingness test was successful:
--      a) increment number of successful invocations
--      b) increment number of total invocations
--      c) add current number of removed bytes to the total
--         number of removed bytes of this pass
-- 2. if the interestingness test failed:
--      a) increment number of total invocations
updateStatistics_ :: RConf -> String -> Word -> Int -> STM ()
updateStatistics_ conf name i sizeDiff =
    modifyTVar (_tState conf) $ \s ->
        s & statistics . passStats . at name %~ \case
            Nothing -> Just $ PassStats name i 1 sizeDiff
            Just (PassStats _ n d r) ->
                Just $ PassStats name (n + i) (d + 1) (r + sizeDiff)

testNewState :: RConf -> RState -> R IO Interesting
testNewState conf newState =
    withTempDir (_tempDirs conf) $ \tempDir -> do
        let sourceFile = tempDir </> _sourceFile conf
            test = tempDir </> _test conf

        CE.try
            ( do
                  liftIO . TIO.writeFile (fromAbsFile sourceFile) $ showState newState
                  runTest test defaultDuration
            )
            >>= \case
                Left (_ :: CE.SomeException) -> traceShow ("tryNewState, EXCEPTION:" :: String) $ return Uninteresting
                Right i -> return i

runTest :: Path Abs File -> Word -> R IO Interesting
runTest test duration = do
    let dirName = parent test
    let testName = filename test

    (timeout (fromIntegral duration) $ liftIO $ flip readCreateProcessWithExitCode "" $ (shell $ "./" <> fromRelFile testName) {cwd = Just $ fromAbsDir dirName}) >>= \case
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

    let info = "state size: " <> (show . T.length . showState $ oldState)

    -- liftIO $ putStrLn $ "\n***" <> context <> "***"
    -- liftIO $ putStrLn info

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
                liftIO . TIO.writeFile ((fromRelFile $ _sourceFile conf) <> ".stale_state") $ showState newState
                $(logTM) ErrorS (fromString $ "potential hsreduce bug: Test case is broken at >>>" <> context <> "<<<")
                error $ "potential hsreduce bug: Test case is broken at >>>" <> context <> "<<<"
        _ -> return ()

-- get ways to change an expression that contains a list as an subexpression
-- p: preprocessing (getting to the list)
-- f: filtering and returning a valid expression again
handleSubList :: Eq e => (e -> a -> a) -> (a -> [e]) -> WaysToChange a
handleSubList f p = map f . p

modname2components :: T.Text -> [T.Text]
modname2components = T.words . T.map (\c -> if c == '.' then ' ' else c)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

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
getGhcOutput :: Tool -> GhcMode -> Path Abs File -> IO (Maybe [(T.Text, SL.RealSrcSpan)])
getGhcOutput tool ghcMode sourcePath = do
    allPragmas <- getPragmas sourcePath

    let dirName = parent sourcePath
        fileName = filename sourcePath
        command = case tool of
            Ghc -> "ghc " <> ghcModeString <> " -ddump-json " <> unwords (("-X" ++) . T.unpack . showExtension <$> allPragmas) <> " " <> fromRelFile fileName
            Cabal -> "nix-shell --run 'cabal new-build'"

    (_, stdout, _) <-
        fromMaybe (error "getGhcOutput timeout!")
            <$> timeout (fromIntegral $ 60 * defaultDuration) (flip readCreateProcessWithExitCode "" ((shell command) {cwd = Just $ fromAbsDir dirName}))

    return $ case stdout of
        "" -> Nothing
        _ -> fromRight [] . sequence . filter isRight . concat . mapFunc . filterFunc . filter (isJust . UT.span) <$> (sequence . filter isJust . map (decodeStrict . TE.encodeUtf8) . T.lines . T.pack $ stdout)
    where
        ghcModeString = case ghcMode of
            Binds -> "-Wunused-binds"
            Imports -> "-Wunused-imports"
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

useP :: M.Parsec e s a -> s -> Either (M.ParseErrorBundle s e) a
useP = flip M.parse ""

notInScopeP :: Parser T.Text
notInScopeP = do
    void $ M.chunk "Not in scope:"
    somethingInTicksP

perhapsYouMeantP :: Parser T.Text
perhapsYouMeantP = do
    M.try
        ( do
              void $ M.chunk "Not in scope:"
              void $ somethingInTicksP
              void $ MC.char '\8217'
        )
        <|> M.try (dotsP "Variable not in scope:")
        <|> dotsP "Data constructor not in scope:"
    MC.space
    void $ (M.try (M.chunk "Perhaps you meant") <|> M.chunk "Perhaps you meant one of these:")
    somethingInTicksP

somethingInTicksP :: Parser T.Text
somethingInTicksP = do
    void $ M.some (M.satisfy (/= '\8216'))
    void $ MC.char '\8216'
    T.pack <$> M.some (M.satisfy (/= '\8217'))

dotsP :: T.Text -> Parser ()
dotsP s = do
    void $ MC.char '\8226'
    MC.space
    void $ M.chunk s
    void $ M.some (M.satisfy (/= '\8226'))
    void $ MC.char '\8226'

removeUseOfHidden :: T.Text -> T.Text
removeUseOfHidden s
    | length components > 2 =
        removeInternal init (T.intercalate "." $ init components) <> "." <> (last components)
    | otherwise = s
    where
        components = modname2components s

removeInternal :: ([T.Text] -> [T.Text]) -> T.Text -> T.Text
removeInternal f s
    | length components > 2 = T.intercalate "." . (\wrds -> if "Internal" `elem` wrds then takeWhile (/= "Internal") wrds else f wrds) $ components
    | otherwise = s
    where
        components = modname2components s

hiddenImportP :: Parser T.Text
hiddenImportP = do
    void $ M.chunk "Could not load module"
    MC.space
    void $ MC.char '‘'
    T.pack <$> M.some (M.satisfy (/= '’'))

noModuleNamedP :: Parser T.Text
noModuleNamedP = do
    void $ M.chunk "Not in scope:"
    MC.space
    go
    void $ MC.char '‘'
    T.pack <$> M.some (M.satisfy (/= '’'))
    where
        go = do
            M.try (M.chunk "No module named" >> MC.space)
                <|> do
                    void $ M.some (M.satisfy (/= '\n'))
                    MC.space
                    go

importedFromP :: Parser T.Text
importedFromP = do
    void $ M.some $ M.satisfy (/= '(')
    void $ MC.char '('
    void $ M.chunk "imported"
    MC.space
    void $ M.chunk "from"
    MC.space
    fmap T.pack . M.some $ M.satisfy (/= ')')

span2SrcSpan :: Span -> SL.RealSrcSpan
span2SrcSpan (Span f sl' sc el ec) = SL.mkRealSrcSpan (SL.mkRealSrcLoc n sl' sc) (SL.mkRealSrcLoc n el ec)
    where
        n = mkFastString $ T.unpack f

isInProduction :: Bool
isInProduction = False

-- default duration: 30 seconds
defaultDuration :: Word
defaultDuration = 60 * 1000 * 1000

longDuration :: Word
longDuration = 360 * 1000 * 1000

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)

infixr 8 <&&>

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

insertTextAtLocation :: (T.Text, (RealSrcLoc, RealSrcLoc)) -> T.Text -> T.Text
insertTextAtLocation (newName, (startLoc, endLoc)) fileContent =
    T.unlines $ prevLines <> [traceShow ("changing at line " <> show (currentIndex + 1) <> ": " <> show oldName <> " -> " <> show realNewName) newLineContent] <> succLines
    where
        contentLines = T.lines fileContent
        lineStart = srcLocLine startLoc
        colStart = srcLocCol startLoc
        colEnd = srcLocCol endLoc
        currentIndex = lineStart -1
        prevLines = take currentIndex contentLines
        succLines = drop lineStart contentLines
        currentLine = contentLines !! currentIndex
        oldName = T.take (colEnd - colStart) $ T.drop (colStart - 1) currentLine
        prefix = T.take (colStart -1) currentLine
        suffix = T.drop (colEnd -1) currentLine
        isInfix = (== 2) . T.length . T.filter (== '`') . T.drop (colStart -1) . T.take (colEnd -1) $ currentLine
        realNewName =
            if "Internal" `elem` T.words oldName
                then removeInternal id oldName
                else newName
        newLineContent =
            if isInfix
                then prefix <> "`" <> realNewName <> "`" <> suffix
                else prefix <> realNewName <> suffix

runParser :: DynFlags -> P a -> String -> ParseResult a
runParser flags parser str = unP parser parseState
    where
      filename = "<interactive>"
      location = mkRealSrcLoc (mkFastString filename) 1 1
      buffer = stringToStringBuffer str
      parseState = mkPState flags buffer location
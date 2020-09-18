module Util.Util where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import qualified Control.Exception as CE
import Control.Monad.Reader
import Data.Aeson (decodeStrict)
import Data.Char
import Data.Data
import Data.Either
import Data.Generics.Aliases (extQ)
import Data.Generics.Uniplate.Data
import Data.List.Split
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Debug.Trace
import FastString
import GHC hiding (GhcMode, Pass, ghcMode)
import Lens.Micro.Platform
import Outputable hiding ((<>))
import Parser.Parser
import Path
import SrcLoc as SL
import System.Exit
import System.Process
import System.Timeout
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import Util.Types as UT

tryNewState :: String -> (RState -> RState) -> RConf -> IO ()
tryNewState passId f conf = do
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
                                writeTVar (_tState conf) $ newState { _isAlive = True }

                                updateStatistics_ conf passId 1 sizeDiff
                                traceShow ("+" :: String) $ return True
                            else return False

                    unless success $ tryNewState passId f conf
                Uninteresting -> updateStatistics conf passId 0 0
        else updateStatistics conf passId 0 0

overwriteAtLoc :: SrcSpan -> (a -> a) -> Located a -> Located a
overwriteAtLoc loc f oldValue@(L oldLoc a)
    | loc == oldLoc = L loc $ f a
    | otherwise = oldValue

mkPass :: Data a => String -> WaysToChange a -> Pass
mkPass name pass = AST name (\ast -> concat [map (transformBi . overwriteAtLoc l) $ pass e | L l e <- universeBi ast])

runPass :: Pass -> R ()
runPass (AST name pass) = do
    unless isInProduction $ do
        printInfo name
        isTestStillFresh name

    ask
    >>= fmap (pass . _parsed) . liftIO . readTVarIO . _tState 
    >>= applyInterestingChanges name 
runPass _ = return ()

-- getProposedChanges :: Data a => WaysToChange a -> ParsedSource -> [ParsedSource -> ParsedSource]
-- getProposedChanges pass ast = pass ast

applyInterestingChanges :: String -> [ParsedSource -> ParsedSource] -> R ()
applyInterestingChanges name proposedChanges = do
    conf <- ask
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

    -- run one thread for each batch
    liftIO . forConcurrently_ batches $ \batch -> do
        -- within a thread check out all the changes
        forM_ batch $ \c -> tryNewState name (parsed %~ c) conf

updateStatistics :: RConf -> String -> Word -> Int -> IO ()
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

testNewState :: RConf -> RState -> IO Interesting
testNewState conf newState =
    withTempDir (_tempDirs conf) $ \tempDir -> do
        let sourceFile = tempDir </> _sourceFile conf
            test = tempDir </> _test conf

        (CE.try . TIO.writeFile (fromAbsFile sourceFile) . showState $ newState) >>= \case
            Left (e :: CE.SomeException) -> traceShow ("testNewStateFlex, EXCEPTION:" :: String) . traceShow (show e) $ return Uninteresting
            Right _ -> runTest test defaultDuration

runTest :: Path Abs File -> Word -> IO Interesting
runTest test duration = do
    let dirName = parent test
    let testName = filename test

    (timeout (fromIntegral duration) $ flip readCreateProcessWithExitCode "" $ (shell $ "./" <> fromRelFile testName) {cwd = Just $ fromAbsDir dirName}) >>= \case
        Nothing -> do
            putStrLn "error - runTest: timed out"
            return Uninteresting
        Just (exitCode, _, _) -> return $ case exitCode of
            ExitFailure _ -> Uninteresting
            ExitSuccess -> Interesting

printInfo :: String -> R ()
printInfo context = do
    tState <- asks _tState
    oldState <- liftIO . atomically $ readTVar tState

    liftIO . putStrLn $ "\n***" <> context <> "***"
    liftIO $ putStrLn $ "Size of old state: " <> (show . T.length . showState $ oldState)

isTestStillFresh :: String -> R ()
isTestStillFresh context = do
    conf <- ask
    newState <- liftIO . atomically $ readTVar $ _tState conf
    liftIO $
        testNewState conf newState >>= \case
            Uninteresting -> do
                liftIO . TIO.writeFile ((fromRelFile $ _sourceFile conf) <> ".stale_state") $ showState newState
                error $ "Test case is broken at >>>" <> context <> "<<<"
            _ -> return ()

-- get ways to change an expression that contains a list as an subexpression
-- p: preprocessing (getting to the list)
-- f: filtering and returning a valid expression again
handleSubList :: Eq e => (e -> a -> a) -> (a -> [e]) -> WaysToChange a
handleSubList f p = map f . p

-- minireduce :: Data a => (Located a -> R (Located a)) -> R ()
-- minireduce pass = void . (transformBiM pass) =<< gets _parsed

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


withTempDir :: TChan (Path Abs Dir) -> (Path Abs Dir -> IO a) -> IO a
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
span2SrcSpan (Span f sl sc el ec) = SL.mkRealSrcSpan (SL.mkRealSrcLoc n sl sc) (SL.mkRealSrcLoc n el ec)
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

pattern UnitTypeP :: HsType GhcPs
pattern UnitTypeP = HsTupleTy NoExt HsBoxedTuple []

-- delete at Index i, starting from 1 not from 0
delete :: Int -> [a] -> [a]
delete i as = take (i -1) as <> drop i as

exprContainsId :: RdrName -> HsExpr GhcPs -> Bool
exprContainsId n (HsApp _ (L _ (HsVar _ (L _ a))) _) = n == a
exprContainsId n (HsApp _ (L _ e) _) = exprContainsId n e
exprContainsId _ _ = False

-- n: total number of patterns
-- i: index of pattern we wish to remove
rmvArgsFromExpr :: RdrName -> Int -> Int -> HsExpr GhcPs -> HsExpr GhcPs
rmvArgsFromExpr conId n i e@(HsApp x la@(L _ a) b)
    | exprContainsId conId e, n == i = a
    | exprContainsId conId e = HsApp x (rmvArgsFromExpr conId (n -1) i <$> la) b
    | otherwise = e
rmvArgsFromExpr _ _ _ e = e

-- realFloor :: T.Text
-- realFloor = "Not in scope: ‘GHC.Real.floor’\nNo module named ‘GHC.Real’ is imported."
--
-- hiddenModule :: T.Text
-- hiddenModule = "Could not load module ‘Data.HashMap.Base’\nit is a hidden module in the package ‘unordered-containers-0.2.10.0’\nit is a hidden module in the package ‘unordered-containers-0.2.10.0’\nUse -v to see a list of the files searched for."
--
-- "Not in scope:\n  type constructor or class \u2018Data.Aeson.Types.ToJSON.ToJSON\u2019\nPerhaps you meant one of these:\n  \u2018Data.Aeson.Types.GToJSON\u2019 (imported from Data.Aeson.Types),\n  \u2018Data.Aeson.Types.GToJSON\u2019 (imported from Data.Aeson.Types),\n  \u2018Data.Aeson.Types.ToJSON\u2019 (imported from Data.Aeson.Types)\nNo module named \u2018Data.Aeson.Types.ToJSON\u2019 is imported."
--
-- -- noSuchModule :: T.Text
-- -- noSuchModule = "Not in scope: ‘Data.Binary.Put.runPutM’\nNo module named ‘Data.Binary.Put’ is imported."
--
--
dataConstructorNotInScope :: T.Text
dataConstructorNotInScope = "\8226 Data constructor not in scope:\n    Closed :: (UnBounded Integer :+ ()) -> EndPoint (UnBounded r :+ ())\n\8226 Perhaps you meant one of these:\n    ‘Data.Range.Closed’ (imported from Data.Range),\n    variable ‘Data.Range.isClosed’ (imported from Data.Range)"

--
--
simple :: T.Text
simple = "Not in scope:\n type constructor or class ‘Text.ProtocolBuffers.Extensions.GPB’\n Perhaps you meant ‘Text.ProtocolBuffers.Header.GPB’ (imported from Text.ProtocolBuffers.Header)\n No module named ‘Text.ProtocolBuffers.Extensions’ is imported."

--
-- noModuleNamed :: T.Text
-- noModuleNamed = "Not in scope:\n type constructor or class ‘Text.ProtocolBuffers.TextMessage.TextType’\n Perhaps you meant one of these:\n ‘Text.ProtocolBuffers.Header.TextType’ (imported from Text.ProtocolBuffers.Header),\n ‘Text.ProtocolBuffers.WireMessage.Get’ (imported from Text.ProtocolBuffers.WireMessage)\n No module named ‘Text.ProtocolBuffers.TextMessage’ is imported."
--
-- importedFrom :: T.Text
-- importedFrom = "Not in scope: type constructor or class ‘Data’\nPerhaps you meant ‘Text.ProtocolBuffers.Header.Data’ (imported from Text.ProtocolBuffers.Header)"

variableNotInScope :: T.Text
variableNotInScope =
    "\8226 Variable not in scope:\n    maybe\n      :: t0\n         -> t1 -> Maybe Int -> Either Int Utf8_TextProtocolBuffersBasic\n  "
        <> "\8226 Perhaps you meant ‘Prelude.maybe’ (imported from Prelude)"

--
--
--
-- -- getModuleName :: T.Text -> T.Text
-- -- getModuleName = T.intercalate "." . fromMaybe [] . safeInit . T.words . T.map (\c -> if c == '.' then ' ' else c) . trace'' "getModuleName" show
--
-- -- this isn't exactly like the init from Prelude
-- -- safeInit :: [a] -> Maybe [a]
-- -- safeInit [] = Nothing
-- -- safeInit xs = Just $ init xs

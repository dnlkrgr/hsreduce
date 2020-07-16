module Util.Util where

import qualified Data.Map as M
import Control.Concurrent.STM
import GHC hiding (GhcMode, ghcMode)
import Data.Void
import Data.Generics.Aliases (extQ)
import Parser.Parser
import qualified Control.Exception  as CE
import qualified Data.Text.Encoding as TE
import Path
import Control.Applicative
import Text.RE.TDFA.Text
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import SrcLoc as SL
import Outputable hiding ((<>))
import FastString
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as LBS
import Data.Data
import Data.Maybe
import Debug.Trace
import System.Exit
import System.Process
import System.Timeout
import Util.Types as UT
import Data.Generics.Uniplate.Data
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import System.Directory
import System.Posix.Files
import Data.List.Split

printInfo :: String -> R ()
printInfo context = do
    oldState  <- get

    liftIO . putStrLn $ "\n***" <> context <> "***"
    liftIO $ putStrLn $ "Size of old state: " ++ (show . T.length . showState $ oldState)

isTestStillFresh :: String -> R ()
isTestStillFresh context = do
    conf     <- ask
    newState <- get
    liftIO $ testAndUpdateStateFlex conf False True newState >>= \case
        False -> do
            liftIO . TIO.writeFile ((fromRelFile $ _sourceFile conf) <> ".stale_state") $ showState newState
            error $ "Test case is broken at >>>" <> context <> "<<<"
        True  -> putStrLn $ context <> ": Test case is still fresh :-)"


type WaysToChange a = a -> [a -> a]


runPass :: Data a => String -> WaysToChange a -> R ()
runPass passName pass = do
    liftIO $ putStrLn passName

    conf     <- ask
    oldState <- get

    let
        ast             = _parsed oldState
        proposedChanges = getProposedChanges ast pass

    -- 1. fold
    -- take the current AST and the next proposed change
    -- apply the change
    -- call testAndUpdateStateFlex conf oldAST newAST currentState
    void $ foldM (\oldAST c -> do
        let 
            newAST = transformBi c oldAST
            sizeDifference = length (lshow oldAST) - length (lshow newAST)

        liftIO (tryNewValue conf oldState { _parsed = newAST }) >>= \case
            True  -> do
                modify $ \s -> s {
                      _parsed       = newAST
                    , _isAlive      = _isAlive s || oshow oldAST /= oshow newAST
                    , _statistics   = updateStatistics True passName s sizeDifference
                    }

                return newAST

            False -> do
                modify $ \s -> s { _statistics = updateStatistics False passName s 0 }
                return oldAST

        ) ast $ proposedChanges


updateStatistics :: Bool -> String -> RState -> Int -> M.Map String Statistics
updateStatistics success passName s dR =
    let m = (_statistics s) 
        i = if success
            then 1
            else 0
    in case M.lookup passName m  of
        Nothing                     -> M.insert passName (Statistics i 1 dR) m
        Just (Statistics n d r)  -> 
            let newStatistics = Statistics (n + i) (d + 1) (r + dR)
            in  M.insert passName newStatistics m


getProposedChanges :: Data a => ParsedSource -> WaysToChange a -> [Located a -> Located a]
getProposedChanges ast pass = concat [map (overwriteAtLoc l) $ pass e| L l e <- universeBi ast]


getInterestingChanges :: Data a => [[Located a -> Located a]] -> R [(Located a -> Located a, Bool)]
getInterestingChanges proposedChanges = do
    oldState        <- get
    oldConf         <- ask
    numberOfThreads <- asks _numberOfThreads
    tAST            <- asks _tAST

    let
        chunkSize       = div (length proposedChanges) numberOfThreads
        -- number of proposedChanges might be smaller than number of threads which might 
        -- result in chunkSize of 0, so we need to check for that!
        batches         = case chunkSize of
            0 -> proposedChanges
            _ -> let
                    temp = map concat . chunksOf chunkSize $ proposedChanges
                 in case reverse temp of
                     (x:y:xs) ->
                         if length temp > numberOfThreads
                         then (x ++ y) : xs
                         else temp
                     _ -> temp

    -- run one thread for each batch
    changes <- liftIO . fmap concat . forM batches $ \batch -> do

        -- within a thread check out all the changes
        forM batch $ \c -> do
            currentAST   <- transformBi c <$> atomically $ readTVar tAST
            b  <- tryNewValue oldConf (oldState { _parsed = currentAST})
            when b $ atomically $ writeTVar tAST currentAST

            return (c, b)

    return changes



-- get ways to change an expression that contains a list as an subexpression
-- p: preprocessing (getting to the list)
-- f: filtering and returning a valid expression again
handleSubList :: Eq e => (e -> a -> a) -> (a -> [e]) -> WaysToChange a
handleSubList f p = map f . p

minireduce :: Data a => (Located a -> R (Located a)) -> R ()
minireduce pass = void . (transformBiM pass) =<< gets _parsed

modname2components :: T.Text -> [T.Text]
modname2components = T.words . T.map (\c -> if c == '.' then ' ' else c)

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

gshow :: Data a => a -> String
gshow x = gshows x ""

gshows :: Data a => a -> ShowS
gshows = render `extQ` (shows :: String -> ShowS) where
  render t
    -- | isTuple = showChar '('
    --           . drop 1
    --           . commaSlots
    --           . showChar ')'
    -- | isNull = showString "[]"
    | isList = showChar '['
             . drop 1
             . listSlots
             . showChar ']'
    | otherwise = showChar '('
                . constructor
                . slots
                . showChar ')'

    where constructor = showString . showConstr . toConstr $ t
          slots       = foldr (.) id . gmapQ ((showChar ' ' .) . gshows) $ t
          -- commaSlots  = foldr (.) id . gmapQ ((showChar ',' .) . gshows) $ t
          listSlots   = foldr (.) id . init . gmapQ ((showChar ',' .) . gshows) $ t
          -- isTuple     = all (==',') (filter (not . flip elem "()") (constructor ""))
          -- isNull      = null (filter (not . flip elem "[]") (constructor ""))
          isList      = constructor "" == "(:)"


recListDirectory :: FilePath -> IO [FilePath]
recListDirectory dir = listDirectory dir >>=
   fmap concat . mapM
       (\f ->
           let f' = dir <> "/" <> f
           in (isDirectory <$> getFileStatus f') >>= \case
               False -> return [f']
               True  -> recListDirectory f')

trace'' :: String -> (a -> String) -> a -> a
trace'' s f a = traceShow (s <> ": " <> f a) a

overwriteAtLoc :: SrcSpan -> (a -> a) -> Located a -> Located a
overwriteAtLoc loc f oldValue@(L oldLoc a)
    | loc == oldLoc = L loc $ f a
    | otherwise = oldValue

tryNewValue :: RConf -> RState -> IO Bool
tryNewValue conf = testAndUpdateStateFlex conf False True

testAndUpdateStateFlex :: RConf -> a -> a -> RState -> IO a
testAndUpdateStateFlex conf a b newState =
    withTempDir (_tempDirs conf) $ \tempDir -> do
        let sourceFile = tempDir </> _sourceFile conf
        let test       = tempDir </> _test conf

        (CE.try . TIO.writeFile (fromAbsFile sourceFile) . showState $ newState) >>= \case
            Left (e :: CE.SomeException) -> traceShow  (show e) $ return a
            Right _ -> do
                runTest test defaultDuration >>= return . \case
                    Uninteresting -> a
                    Interesting   -> b


runTest :: Path Abs File -> Word -> IO Interesting
runTest test duration = do
    let dirName  = parent   test
    let testName = filename test

    (timeout (fromIntegral duration) $ flip readCreateProcessWithExitCode "" $ (shell $ "./" ++ fromRelFile testName) {cwd = Just $ fromAbsDir dirName}) >>= \case
         Nothing -> do
             errorPrint "runTest: timed out"
             return Uninteresting
         Just (exitCode,_,_) -> return $ case exitCode of
             ExitFailure _ -> Uninteresting
             ExitSuccess   -> Interesting


withTempDir :: TChan (Path Abs Dir) -> (Path Abs Dir -> IO a) -> IO a
withTempDir tChan action = do
    tempDir <- atomically $ readTChan tChan
    a <- action tempDir
    atomically $ writeTChan tChan tempDir
    return a


-- | run ghc with -Wunused-binds -ddump-json and delete decls that are mentioned there
getGhcOutput :: Tool -> GhcMode -> Path Abs File -> IO (Maybe [(Either (M.ParseErrorBundle T.Text Void) T.Text, SL.RealSrcSpan)])
getGhcOutput tool ghcMode sourcePath = do
    pragmas <- getPragmas sourcePath

    let dirName  = parent sourcePath
        fileName = filename sourcePath
        command = case tool of
            Ghc   -> "ghc " ++ ghcModeString ++ " -ddump-json " ++ unwords (("-X" ++) . T.unpack . showExtension <$> pragmas) ++ " " ++ fromRelFile fileName
            Cabal -> "nix-shell --run 'cabal new-build'"

    (_, stdout, _) <- 
        fromMaybe (error "getGhcOutput timeout!") 
        <$> timeout (fromIntegral $ 60 * defaultDuration) (flip readCreateProcessWithExitCode "" ((shell command) {cwd = Just $ fromAbsDir dirName}))

    return $ case stdout of
        "" -> Nothing
        _  ->   concat . mapFunc . filterFunc <$> (sequence . filter isJust . map (decode . LBS.fromStrict . TE.encodeUtf8) . T.lines . T.pack $ stdout)

  where
      ghcModeString = case ghcMode of
          Binds   -> "-Wunused-binds"
          Imports -> "-Wunused-imports"
          _       -> ""

      filterFunc = case ghcMode of
          MissingImport   -> filter ((T.isPrefixOf "Not in scope:" <&&> (T.isInfixOf "imported from" <||> T.isInfixOf "No module named")) . doc)
          NotInScope      -> filter ( T.isPrefixOf "Not in scope:" . doc)
          PerhapsYouMeant -> filter ((T.isPrefixOf "Not in scope:" <&&> (T.isInfixOf "Perhaps you meant")) . doc)
          HiddenImport    -> filter (T.isInfixOf "is a hidden module in the package" . doc)
          Indent          -> filter (T.isInfixOf "parse error (possibly incorrect indentation" . doc)
          _               -> filter ((isJust . UT.span) <&&> (isJust . reason) <&&> (T.isPrefixOf "Opt_WarnUnused" . fromJust . reason))

      mapFunc = case ghcMode of

          MissingImport   -> 
              map (\o -> let importSuggestion = fmap (removeInternal id) . useP importedFromP  $ doc o 
                             noModuleNamed    = fmap (removeInternal id) . useP noModuleNamedP $ doc o 
                             pos = span2SrcSpan . fromJust . UT.span $ o 
                         in [(importSuggestion, pos), (noModuleNamed, pos)])
          NotInScope      -> map (\o -> [(fmap removeUseOfHidden . useP notInScopeP $ doc o , span2SrcSpan . fromJust . UT.span $ o)])
          PerhapsYouMeant -> map (\o -> [(useP perhapsYouMeantP $ doc o , span2SrcSpan . fromJust . UT.span $ o)])
          Indent          -> map (\o -> [(Right "", span2SrcSpan . fromJust . UT.span $ o)])
          HiddenImport    -> map ((:[]) . ((,) <$> (fmap (removeInternal init) . useP hiddenImportP . doc) <*> (span2SrcSpan . fromJust . UT.span)))
          _               -> map ((:[]) . ((,) <$> (Right . T.takeWhile (/= '’') . T.drop 1 . T.dropWhile (/= '‘') . doc) <*> (span2SrcSpan . fromJust . UT.span)))


useP :: M.Parsec e s a -> s -> Either (M.ParseErrorBundle s e) a
useP = flip M.parse ""

notInScopeP :: Parser T.Text
notInScopeP = do
    void $ M.chunk "Not in scope:"
    void $ M.some (M.satisfy (/= '‘'))
    void $ MC.char '‘'
    T.pack <$> M.some (M.satisfy (/= '’'))

perhapsYouMeantP :: Parser T.Text
perhapsYouMeantP = do
    void $ M.chunk "Not in scope:"
    void $ M.some (M.satisfy (/= '‘'))
    void $ MC.char '‘'
    void $ M.some (M.satisfy (/= '’'))
    void $ MC.char '’'
    MC.space
    void $ (M.try (M.chunk "Perhaps you meant") <|> M.chunk "Perhaps you meant one of these:")
    void $ M.some (M.satisfy (/= '‘'))
    void $ MC.char '‘'
    T.pack <$> M.some (M.satisfy (/= '’'))
   
removeUseOfHidden :: T.Text -> T.Text
removeUseOfHidden s 
    | length components > 2 
    = removeInternal init (T.intercalate "." $ init components) <> "." <> (last components)
    | otherwise = s
    where components = modname2components s 

removeInternal :: ([T.Text] -> [T.Text]) -> T.Text -> T.Text
removeInternal f s
    | length components > 2 = T.intercalate "." . (\wrds -> if "Internal" `elem` wrds then takeWhile (/= "Internal") wrds else f wrds) $ components
    | otherwise = s
  where components = modname2components s 

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
  where n = mkFastString $ T.unpack f

isQual :: T.Text -> Bool
isQual = matched . (?=~ [re|([A-Za-z]+\.)+[A-Za-z0-9#_]+|])

debug :: MonadIO m => (a -> m ()) -> a -> m ()
debug f s
    | isInProduction = return ()
    | otherwise = f s

errorPrint :: MonadIO m => String -> m ()
errorPrint = debug (liftIO . putStrLn . ("[error] " ++))

isInProduction :: Bool
isInProduction = False

-- default duration: 30 seconds
defaultDuration :: Word
defaultDuration = 30 * 1000 * 1000

longDuration :: Word
longDuration = 120 * 1000 * 1000

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)

infixr 8 <&&>

oshow :: Outputable a => a -> String
oshow = showSDocUnsafe . ppr

banner :: MonadIO m => String -> m ()
banner s = liftIO $ putStrLn $ "\n" ++ s' ++ s ++ s'
  where
      n = 80 - length s
      s' = replicate (div n 2) '='

lshow :: Outputable a => Located a -> String
lshow = showSDocUnsafe . ppr . unLoc

trace' :: Show a => a -> a
trace' a = traceShow a a



-- realFloor :: T.Text
-- realFloor = "Not in scope: ‘GHC.Real.floor’\nNo module named ‘GHC.Real’ is imported."
-- 
-- hiddenModule :: T.Text
-- hiddenModule = "Could not load module ‘Data.HashMap.Base’\nit is a hidden module in the package ‘unordered-containers-0.2.10.0’\nit is a hidden module in the package ‘unordered-containers-0.2.10.0’\nUse -v to see a list of the files searched for."
-- 
-- -- "Not in scope:\n  type constructor or class \u2018Data.Aeson.Types.ToJSON.ToJSON\u2019\nPerhaps you meant one of these:\n  \u2018Data.Aeson.Types.GToJSON\u2019 (imported from Data.Aeson.Types),\n  \u2018Data.Aeson.Types.GToJSON\u2019 (imported from Data.Aeson.Types),\n  \u2018Data.Aeson.Types.ToJSON\u2019 (imported from Data.Aeson.Types)\nNo module named \u2018Data.Aeson.Types.ToJSON\u2019 is imported."
-- 
-- -- noSuchModule :: T.Text
-- -- noSuchModule = "Not in scope: ‘Data.Binary.Put.runPutM’\nNo module named ‘Data.Binary.Put’ is imported."
-- 
-- 
--   
-- 
-- simple :: T.Text
-- simple = "Not in scope:\n type constructor or class ‘Text.ProtocolBuffers.Extensions.GPB’\n Perhaps you meant ‘Text.ProtocolBuffers.Header.GPB’ (imported from Text.ProtocolBuffers.Header)\n No module named ‘Text.ProtocolBuffers.Extensions’ is imported."
-- 
-- noModuleNamed :: T.Text
-- noModuleNamed = "Not in scope:\n type constructor or class ‘Text.ProtocolBuffers.TextMessage.TextType’\n Perhaps you meant one of these:\n ‘Text.ProtocolBuffers.Header.TextType’ (imported from Text.ProtocolBuffers.Header),\n ‘Text.ProtocolBuffers.WireMessage.Get’ (imported from Text.ProtocolBuffers.WireMessage)\n No module named ‘Text.ProtocolBuffers.TextMessage’ is imported."
-- 
-- importedFrom :: T.Text
-- importedFrom = "Not in scope: type constructor or class ‘Data’\nPerhaps you meant ‘Text.ProtocolBuffers.Header.Data’ (imported from Text.ProtocolBuffers.Header)"
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

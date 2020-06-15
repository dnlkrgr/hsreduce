module Util.Util where

import Data.Void
import Data.Generics.Aliases (extQ)
import Parser.Parser
import qualified Control.Exception as CE
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

fastTryR :: Data a => (Located a -> Maybe (R (Located a))) -> Located a -> R (Located a)
fastTryR f e = fromMaybe (return e) . f $ e

fastTry :: Data a => (a -> Maybe a) -> Located a -> R (Located a)
fastTry f e = maybe (return e) (tryNewValue e) . f . unLoc $ e

reduceListOfSubelements ::
  (Data a, Eq e) =>
  -- | how to get a list of comparable elements
  (a -> [e]) ->
  -- | based on comparable info, change element a
  (e -> a -> a) ->
  Located a ->
  R (Located a)
reduceListOfSubelements getCmpElements f la = (foldr ((>=>) . try . f) return . getCmpElements . unLoc $ la) la

try :: Data a => (a -> a) -> Located a -> R (Located a)
try g (L l v) = tryNewValue (L l v) (g v)

tryNewValue :: Data a => Located a -> a -> R (Located a)
tryNewValue oldValue@(L loc _) newValue = do
    oldState <- get
    let newSource = transformBi (overwriteAtLoc loc newValue) (_parsed oldState)
    testAndUpdateStateFlex oldValue (L loc newValue) (oldState { _parsed = newSource })

overwriteAtLoc ::
  -- | loc:      location that should be updated
  SrcSpan ->
  -- | newValue: has to come before oldValue because we use it in a closure
  a ->
  -- | oldValue
  Located a ->
  Located a
overwriteAtLoc loc newValue oldValue@(L oldLoc _)
    | loc == oldLoc = L loc newValue
    | otherwise = oldValue

testAndUpdateStateFlex :: a -> a -> RState -> R a
testAndUpdateStateFlex a b newState = do
    sourceFile <- asks _sourceFile
    (liftIO . CE.try . TIO.writeFile (fromAbsFile sourceFile) . showState $ newState) >>= \case
        Left (_ :: CE.SomeException) -> return a
        Right _ -> do
          runTest defaultDuration >>= \case
              Uninteresting -> return a
              Interesting -> do
                -- TODO: add information to change operation for better debugging messages
  
                  modify $
                    \s -> newState {
                      _isAlive = if _isAlive s
                                 then traceShow ("+" :: String) $ True
                                 else let e = showState newState /= showState s
                                      in if e then traceShow ("+" :: String) e else e
                      }
  
                  return b


-- TODO: before running test: check if parseable, renaming and typchecking succeed
-- | run the interestingness test on a timeout of 30 seconds
runTest :: Word -> R Interesting
runTest duration = do
    test <- asks _test
    let dirName = parent test
    let testName = filename test
    -- TODO: make timout duration configurable
    liftIO $ timeout (fromIntegral duration) (readCreateProcessWithExitCode ((shell $ "./" ++ fromRelFile testName) {cwd = Just $ fromAbsDir dirName}) "") >>= \case
         Nothing -> do
           errorPrint "runTest: timed out"
           return Uninteresting
         Just (exitCode, _, _) ->
             case exitCode of
                 ExitFailure _ -> return Uninteresting
                 ExitSuccess -> return Interesting

-- | run ghc with -Wunused-binds -ddump-json and delete decls that are mentioned there
getGhcOutput :: Tool -> GhcMode -> Path Abs File -> IO (Maybe [(Either (M.ParseErrorBundle T.Text Void) T.Text, SL.RealSrcSpan)])
getGhcOutput tool ghcMode sourcePath = do
    pragmas <- getPragmas sourcePath

    let dirName  = parent sourcePath
        fileName = filename sourcePath
        command = case tool of
            Ghc -> "ghc " ++ ghcModeString ++ " -ddump-json " ++ unwords (("-X" ++) . T.unpack . showExtension <$> pragmas) ++ " " ++ fromRelFile fileName
            Cabal -> "nix-shell --run 'cabal new-build'"

    (_, stdout, _) <- 
        fromMaybe (error "getGhcOutput timeout!") 
        <$> timeout (fromIntegral defaultDuration) (readCreateProcessWithExitCode ((shell command) {cwd = Just $ fromAbsDir dirName}) "")

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
-- currently so high, because in big files we have up to 8000 errors
defaultDuration :: Word
defaultDuration = 30 * 1000 * 1000

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

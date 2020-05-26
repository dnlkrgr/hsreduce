module Util.Util (safeHead, gshow, recListDirectory, banner, oshow, getGhcOutput, lshow, fastTryR, fastTry, try, reduceListOfSubelements, trace'', isQual) where

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
import Data.Either
import System.Directory
import System.Posix.Files

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
recListDirectory dir =
  listDirectory dir >>=
    fmap concat . mapM
      (\f ->
        let f' = dir <> "/" <> f
        in (isDirectory <$> getFileStatus f') >>=
           \case
             False -> return [f']
             True  -> recListDirectory f')

trace'' :: String -> (a -> String) -> a -> a
trace'' s f a = traceShow (s <> ": " <> f a) a

fastTryR :: Data a => (Located a -> Maybe (R (Located a))) -> Located a -> R (Located a)
fastTryR f e = fromMaybe (return e) . f $ e

fastTry :: Data a => (a -> Maybe a) -> Located a -> R (Located a)
fastTry f e = maybe (return e) (tryNewValue e) . f . unLoc $ e

try :: Data a => (a -> a) -> Located a -> R (Located a)
try g (L l v) = tryNewValue (L l v) (g v)

reduceListOfSubelements ::
  (Data a, Eq e) =>
  -- | how to get a list of comparable elements
  (a -> [e]) ->
  -- | based on comparable info, change element a
  (e -> a -> a) ->
  Located a ->
  R (Located a)
reduceListOfSubelements getCmpElements f la =
  (foldr ((>=>) . try . f) return . getCmpElements . unLoc $ la) la

tryNewValue :: Data a => Located a -> a -> R (Located a)
tryNewValue oldValue@(L loc _) newValue = do
  oldState <- get
  let newSource = descendBi (overwriteAtLoc loc newValue) (_parsed oldState)
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
getGhcOutput :: Path Abs File -> Tool -> GhcMode -> IO (Maybe [(T.Text, SL.RealSrcSpan)])
getGhcOutput sourcePath tool ghcMode = do
  pragmas <- getPragmas sourcePath

  let dirName  = parent sourcePath
      fileName = filename sourcePath
      command = case tool of
        Ghc -> "ghc "
               ++ ghcModeString
               ++ " -ddump-json "
               ++ unwords (("-X" ++) . T.unpack . showExtension <$> pragmas)
               ++ " " ++ fromRelFile fileName
        Cabal -> "cabal new-build"
  (_, stdout, _) <- fromMaybe (error "getGhcOutput timeout!")
                 <$> timeout (fromIntegral $ 30 * defaultDuration)
                             (readCreateProcessWithExitCode
                             ((shell command) {cwd = Just $ fromAbsDir dirName}) "")

  return $ case stdout of
    "" -> Nothing
    _  ->   mapFunc
          . filterFunc
          <$> (sequence . filter isJust . map (decode . LBS.fromStrict . TE.encodeUtf8) . T.lines . T.pack $ stdout)

  where
    ghcModeString = case ghcMode of
      Binds       -> "-Wunused-binds"
      Imports     -> "-Wunused-imports"
      ParseIndent -> ""
      Other       -> ""

    filterFunc = case ghcMode of
      Other -> filter (T.isPrefixOf "Not in scope:" . doc)
      ParseIndent -> filter (T.isInfixOf "parse error (possibly incorrect indentation" . doc)
      _     -> filter ((isJust . UT.span)
                       <&&> (isJust . reason)
                       <&&> (T.isPrefixOf "Opt_WarnUnused" . fromJust . reason))

    mapFunc = case ghcMode of
      Other -> map (\o -> (getQualText $ doc o, span2SrcSpan . fromJust . UT.span $ o))
      ParseIndent -> map (\o -> ("", span2SrcSpan . fromJust . UT.span $ o))
      _     -> map ((,)
                    <$> (T.takeWhile (/= '’') . T.drop 1 . T.dropWhile (/= '‘') . doc)
                    <*> (span2SrcSpan . fromJust . UT.span))

isQual :: T.Text -> Bool
isQual =
  matched . (?=~ [re|([A-Za-z]+\.)+[A-Za-z0-9#_]+|])

-- noSuchModule :: T.Text
-- noSuchModule = "Not in scope: ‘Data.Binary.Put.runPutM’\nNo module named ‘Data.Binary.Put’ is imported."
-- 
-- simple :: T.Text
-- simple = "Not in scope:\n type constructor or class ‘Text.ProtocolBuffers.Extensions.GPB’\n Perhaps you meant ‘Text.ProtocolBuffers.Header.GPB’ (imported from Text.ProtocolBuffers.Header)\n No module named ‘Text.ProtocolBuffers.Extensions’ is imported."
-- 
-- noModuleNamed :: T.Text
-- noModuleNamed = "Not in scope:\n type constructor or class ‘Text.ProtocolBuffers.TextMessage.TextType’\n Perhaps you meant one of these:\n ‘Text.ProtocolBuffers.Header.TextType’ (imported from Text.ProtocolBuffers.Header),\n ‘Text.ProtocolBuffers.WireMessage.Get’ (imported from Text.ProtocolBuffers.WireMessage)\n No module named ‘Text.ProtocolBuffers.TextMessage’ is imported."
-- 
-- importedFrom :: T.Text
-- importedFrom = "Not in scope: type constructor or class ‘Data’\nPerhaps you meant ‘Text.ProtocolBuffers.Header.Data’ (imported from Text.ProtocolBuffers.Header)"

getQualText :: T.Text -> T.Text
getQualText = fromRight "" . M.parse notInScopeP "" . trace'' "getQualText" show

-- getModuleName :: T.Text -> T.Text
-- getModuleName = T.intercalate "." . fromMaybe [] . safeInit . T.words . T.map (\c -> if c == '.' then ' ' else c) . trace'' "getModuleName" show

-- this isn't exactly like the init from Prelude
-- safeInit :: [a] -> Maybe [a]
-- safeInit [] = Nothing
-- safeInit xs = Just $ init xs

notInScopeP :: Parser T.Text
notInScopeP = M.try noModuleNamedP <|> importedFromP

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

debug :: MonadIO m => (a -> m ()) -> a -> m ()
debug f s
  | isInProduction = return ()
  | otherwise = f s

errorPrint :: MonadIO m => String -> m ()
errorPrint = debug (liftIO . putStrLn . ("[error] " ++))

isInProduction :: Bool
isInProduction = False

defaultDuration :: Word
defaultDuration = 60 * 1000 * 1000

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)

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

module Util.Util where

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
import GHC hiding (GhcMode, ghcMode)
import DynFlags hiding (GhcMode, ghcMode)
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
import Parser.Parser
import Data.Generics.Uniplate.Data
import qualified Text.Megaparsec as M
import Text.Megaparsec.Debug
import qualified Text.Megaparsec.Char as MC
import Data.Either


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

testAndUpdateState :: RState -> R ()
testAndUpdateState = testAndUpdateStateFlex () ()

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

changeExports :: ([LIE GhcPs] -> [LIE GhcPs]) -> RState -> RState
changeExports f oldState =
  let L moduleLoc oldModule = _parsed oldState
      L exportsLoc oldExports = fromJust $ hsmodExports oldModule
      newExports = f (traceShow (concatMap ((++ " ") . oshow . unLoc) oldExports) oldExports)
   in oldState {_parsed = L moduleLoc oldModule {hsmodExports = Just (L exportsLoc newExports)}}

changeImports :: ([LImportDecl GhcPs] -> [LImportDecl GhcPs]) -> RState -> RState
changeImports f oldState =
  let L moduleLoc oldModule = _parsed oldState
      allImports = hsmodImports oldModule
      newImports = f allImports
   in oldState {_parsed = L moduleLoc oldModule {hsmodImports = newImports}}


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
      Other -> filter ((T.isInfixOf "Perhaps you meant" <&&> T.isInfixOf "ot in scope:") . doc)
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

testText :: T.Text
testText = "Not in scope:\n  type constructor or class ‘Text.ProtocolBuffers.TextMessage.TextMsg’\nPerhaps you meant one of these:\n  ‘Text.ProtocolBuffers.Header.TextMsg’ (imported from Text.ProtocolBuffers.Header),\n  ‘Text.ProtocolBuffers.Header.TextMsg’ (imported from Text.ProtocolBuffers.Header),\n  ‘Text.ProtocolBuffers.Header.TextMsg’ (imported from Text.ProtocolBuffers.Header)\nNo module named ‘Text.ProtocolBuffers.TextMessage’ is imported."


-- testText :: T.Text
-- testText = "Not in scope: \u2018Data.Set.Internal.fromDistinctAscList\u2019\nNo module named \u2018Data.Set.Internal\u2019 is imported."

getQualText = fromRight "" . M.parse perhapsYouMeant ""

perhapsYouMeant :: Parser T.Text
perhapsYouMeant = do
  M.some $ M.satisfy (/= '\n')
  MC.space
  M.some $ M.satisfy (/= '\n')
  MC.space
  M.some $ M.satisfy (/= '\n')
  MC.space
  MC.string "‘"
  T.pack <$> M.some (M.satisfy (/= '’'))

-- getQualText :: T.Text -> T.Text
-- getQualText =
--     ((not . null) ?: (f . head) $ (const ""))
--   . allMatches
--   . (*=~ [re|‘([A-Za-z]+\.)+[A-Za-z0-9#_']+’|])
--   . T.unlines
--   . drop 1
--   . T.lines
--   where f = T.init . T.tail . fromMaybe "" . matchedText

(?:) :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
(?:) p t f x
  | p x  = t x
  | True = f x

infixl 8 ?:

span2SrcSpan :: Span -> SL.RealSrcSpan
span2SrcSpan (Span f sl sc el ec) = SL.mkRealSrcSpan (SL.mkRealSrcLoc n sl sc) (SL.mkRealSrcLoc n el ec)
  where n = mkFastString $ T.unpack f

debug :: MonadIO m => (a -> m ()) -> a -> m ()
debug f s
  | isInProduction = return ()
  | otherwise = f s

debugPrint :: MonadIO m => String -> m ()
debugPrint = debug (liftIO . putStrLn . ("[debug] " ++))

errorPrint :: MonadIO m => String -> m ()
errorPrint = debug (liftIO . putStrLn . ("[error] " ++))

isInProduction :: Bool
isInProduction = False

defaultDuration :: Word
defaultDuration = 60 * 1000 * 1000

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)

infixr 8 <&&>


showGhc :: (Outputable a) => a -> String
showGhc = showPpr unsafeGlobalDynFlags

trace' :: Show a => a -> a
trace' a = traceShow a a

banner :: MonadIO m => String -> m ()
banner s = liftIO $ putStrLn $ "\n" ++ s' ++ s ++ s'
  where
    n = 80 - length s
    s' = replicate (div n 2) '='

oshow :: Outputable a => a -> String
oshow = showSDocUnsafe . ppr

lshow :: Outputable a => Located a -> String
lshow = showSDocUnsafe . ppr . unLoc

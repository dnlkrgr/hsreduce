module Util.Util (try, fastTry, banner, getGhcOutput, showGhc, isQual, trace', debugPrint, reduceListOfSubelements, tryNewValue, oshow, lshow, changeDecls, changeExports, changeImports, testAndUpdateState, testAndUpdateStateFlex, runTest) where

import qualified Control.Exception as CE
import qualified Data.Text.Encoding as TE
import "ghc" GHC hiding (GhcMode, ghcMode)
import "ghc" DynFlags hiding (GhcMode, ghcMode)
import Path
import Control.Applicative
import Text.RE.TDFA.Text
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import "ghc" SrcLoc as SL
import "ghc" Outputable
import "ghc" FastString
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
                         then True
                         else let e = showState newState /= showState s
                              in if e then traceShow ("." :: String) e else e
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
  liftIO $
    timeout (fromIntegral duration) (readCreateProcessWithExitCode ((shell $ "./" ++ fromRelFile testName) {cwd = Just $ fromAbsDir dirName}) "")
      >>= \case
        Nothing -> do
          errorPrint "runTest: timed out"
          return Uninteresting
        Just (exitCode, _, _) ->
          case exitCode of
            ExitFailure _ -> return Uninteresting
            ExitSuccess -> return Interesting

changeDecls :: ([LHsDecl GhcPs] -> [LHsDecl GhcPs]) -> RState -> RState
changeDecls f oldState =
  let L moduleLoc oldModule = _parsed oldState
      allDecls = hsmodDecls oldModule
      newDecls = f allDecls
   in oldState {_parsed = L moduleLoc oldModule {hsmodDecls = newDecls}}

changeExports :: ([LIE GhcPs] -> [LIE GhcPs]) -> RState -> RState
changeExports f oldState =
  let L moduleLoc oldModule = _parsed oldState
      L exportsLoc oldExports = fromJust $ hsmodExports oldModule
      newExports = f (traceShow (concatMap ((++ " ") . lshow) oldExports) oldExports)
   in oldState {_parsed = L moduleLoc oldModule {hsmodExports = Just (L exportsLoc newExports)}}

changeImports :: ([LImportDecl GhcPs] -> [LImportDecl GhcPs]) -> RState -> RState
changeImports f oldState =
  let L moduleLoc oldModule = _parsed oldState
      allImports = hsmodImports oldModule
      newImports = f allImports
   in oldState {_parsed = L moduleLoc oldModule {hsmodImports = newImports}}

-- | run ghc with -Wunused-binds -ddump-json and delete decls that are mentioned there
getGhcOutput :: Path Abs File -> GhcMode -> IO (Maybe [(T.Text, SL.RealSrcSpan)])
getGhcOutput sourcePath ghcMode = do
  pragmas <- getPragmas sourcePath

  let dirName  = parent sourcePath
      fileName = filename sourcePath
      command             = "ghc "
                            ++ ghcModeString
                            ++ " -ddump-json "
                            ++ unwords (("-X" ++) . T.unpack . showExtension <$> pragmas)
                            ++ " " ++ fromRelFile fileName
  (_, stdout, _) <- fromMaybe (error "getGhcOutput") 
                 <$> timeout (fromIntegral defaultDuration)
                             (readCreateProcessWithExitCode 
                             ((shell command) {cwd = Just $ fromAbsDir dirName}) "")

  return $ case stdout of 
    "" -> Nothing
    -- dropping first line because it doesn't fit into our JSON schema
    _  ->   mapFunc
          . filterFunc
          <$> mapM (decode . LBS.fromStrict . TE.encodeUtf8) (T.lines $ T.pack stdout) 
  where
    ghcModeString = case ghcMode of
      Binds   -> "-Wunused-binds"
      Imports -> "-Wunused-imports"
      Other   -> ""
      ParseIndent   -> ""

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


getQualText :: T.Text -> T.Text
getQualText =
    ((not . null) ?: (f . head) $ (const ""))
  . allMatches
  . (*=~ [re|‘([A-Za-z]+\.)+[A-Za-z0-9#_']+’|])
  . T.unlines
  . drop 1
  . T.lines
  where f = T.init . T.tail . fromMaybe "" . matchedText

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
defaultDuration = 30 * 1000 * 1000

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)


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

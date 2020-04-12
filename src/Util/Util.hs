module Util.Util where

import qualified Data.Text.Encoding as TE
import "ghc" GHC hiding (GhcMode, ghcMode)
import "ghc" DynFlags hiding (GhcMode, ghcMode)
import System.Directory
import System.FilePath.Posix
import System.Posix.Files
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

banner :: MonadIO m => String -> m ()
banner s = liftIO $ putStrLn $ "\n" ++ s' ++ s ++ s'
  where
    n = 80 - length s
    s' = replicate (div n 2) '='

try :: Data a => (a -> a) -> Located a -> R (Located a)
try g (L l v) = tryNewValue (L l v) (g v)

reduceListOfSubelements ::
  (Data a, Eq e) =>
  (a -> [e]) ->
  (e -> a -> a) ->
  Located a ->
  R (Located a)
reduceListOfSubelements getCmpElements f la =
  (foldr ((>=>) . try . f) return . getCmpElements . unLoc $ la) la

tryNewValue :: Data a => Located a -> a -> R (Located a)
tryNewValue oldValue@(L loc _) newValue = do
  oldState <- get
  let newSource = transformBi (overwriteAtLoc loc newValue) (_parsed oldState)
  testAndUpdateStateFlex oldValue (L loc newValue) (oldState { _parsed = newSource})

oshow :: Outputable a => a -> String
oshow = showSDocUnsafe . ppr

lshow :: Outputable a => Located a -> String
lshow = showSDocUnsafe . ppr . unLoc

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
testAndUpdateStateFlex a b s = do
  sourceFile <- asks _sourceFile
  liftIO $ TIO.writeFile sourceFile . showState $ s
  runTest
    >>= \case
      Uninteresting -> return a
      Interesting -> do
        -- TODO: add information to change operation for better debugging messages
        liftIO $ putChar '.'
        put s
        return b

-- TODO: before running test: check if parseable, renaming and typchecking succeed
-- | run the interestingness test on a timeout of 30 seconds
runTest :: R Interesting
runTest = do
  test <- asks _test
  let (dirName, testName) = splitFileName test
  -- TODO: make timout duration configurable
  liftIO $
    timeout duration (readCreateProcessWithExitCode ((shell $ "./" ++ testName) {cwd = Just dirName}) "")
      >>= \case
        Nothing -> do
          errorPrint "runTest: timed out"
          return Uninteresting
        Just (exitCode, _, _) ->
          case exitCode of
            ExitFailure _ -> return Uninteresting
            ExitSuccess -> return Interesting

changeDecls :: ([LHsDecl GhcPs] -> [LHsDecl GhcPs]) -> RState -> RState
changeDecls f oldOrmolu =
  let L moduleLoc oldModule = _parsed oldOrmolu
      allDecls = hsmodDecls oldModule
      newDecls = f allDecls
   in oldOrmolu {_parsed = L moduleLoc oldModule {hsmodDecls = newDecls}}

changeExports :: ([LIE GhcPs] -> [LIE GhcPs]) -> RState -> RState
changeExports f oldOrmolu =
  let L moduleLoc oldModule = _parsed oldOrmolu
      L exportsLoc oldExports = fromJust $ hsmodExports oldModule
      newExports = f (traceShow (concatMap ((++ " ") . lshow) oldExports) oldExports)
   in oldOrmolu {_parsed = L moduleLoc oldModule {hsmodExports = Just (L exportsLoc newExports)}}

changeImports :: ([LImportDecl GhcPs] -> [LImportDecl GhcPs]) -> RState -> RState
changeImports f oldOrmolu =
  let L moduleLoc oldModule = _parsed oldOrmolu
      allImports = hsmodImports oldModule
      newImports = f allImports
   in oldOrmolu {_parsed = L moduleLoc oldModule {hsmodImports = newImports}}

-- | run ghc with -Wunused-binds -ddump-json and delete decls that are mentioned there
getGhcOutput :: FilePath -> GhcMode -> IO (Maybe [(T.Text, SL.RealSrcSpan)])
getGhcOutput sourcePath ghcMode = do
  pragmas <- getPragmas sourcePath

  let (dirName, fileName) = splitFileName sourcePath
      command             = "ghc "
                            ++ ghcModeString
                            ++ " -ddump-json "
                            ++ unwords (("-X" ++) . T.unpack . showExtension <$> pragmas)
                            ++ " " ++ fileName
  (_, stdout, _) <- fromMaybe (error "getGhcOutput") 
                 <$> timeout duration
                             (readCreateProcessWithExitCode 
                             ((shell command) {cwd = Just dirName}) "")

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

    filterFunc = case ghcMode of
      Other -> filter (T.isInfixOf "ot in scope:" . doc)
      _     -> filter ((isJust . UT.span)
                       <&&> (isJust . reason)
                       <&&> (T.isPrefixOf "Opt_WarnUnused" . fromJust . reason))

    mapFunc = case ghcMode of
      Other -> map (\o -> (gotQual $ doc o, span2SrcSpan . fromJust . UT.span $ o))
      _     -> map ((,)
                    <$> (T.takeWhile (/= '’') . T.drop 1 . T.dropWhile (/= '‘') . doc)
                    <*> (span2SrcSpan . fromJust . UT.span))


gotQual :: T.Text -> T.Text
gotQual = fromMaybe "" . matchedText . (?=~ [re|([A-Za-z]+\.)+[A-Za-z0-9#_]+|])


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

duration :: Num a => a
duration = 20 * 1000 * 1000

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)


recListDirectory :: FilePath -> IO [FilePath]
recListDirectory dir =
  listDirectory dir >>=
    fmap concat . mapM
      (\f ->
        let f' = dir </> f
        in (isDirectory <$> getFileStatus f') >>=
           \case
             False -> return [f']
             True  -> recListDirectory f')


showGhc :: (Outputable a) => a -> String
showGhc = showPpr unsafeGlobalDynFlags

trace' :: Show a => a -> a
trace' a = traceShow a a

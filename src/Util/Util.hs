module Util.Util where

import Text.RE.TDFA.String
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified "ghc" SrcLoc as SL
import "ghc" FastString
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Aeson (decode)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Data
import Data.Generics
import Data.List
import Data.Maybe
import Debug.Trace
import Ormolu.Parser.Pragma as OPP (Pragma (PragmaLanguage))
import Ormolu.Parser.Result as OPR (ParseResult, prParsedSource)
import Ormolu.Printer (printModule)
import "ghc-lib-parser" Outputable
import "ghc-lib-parser" HsSyn
import "ghc-lib-parser" SrcLoc
import System.Exit
import System.FilePath.Posix
import System.Process
import System.Timeout
import Util.Types as UT

trace' a = traceShow a a

banner :: MonadIO m => String -> m ()
banner s = liftIO $ putStrLn $ "\n" ++ s' ++ s ++ s'
  where
    n = 80 - length s
    s' = replicate (div n 2) '='

try ::
  Typeable a =>
  (a -> a) ->
  Located a ->
  R (Located a)
try g (L l v) = tryNewValue (L l v) (g v)

reduceListOfSubelements ::
  (Typeable a, Eq e) =>
  (a -> [e]) ->
  (e -> a -> a) ->
  Located a ->
  R (Located a)
reduceListOfSubelements getCmpElements transform la =
  (foldr ((>=>) . try . transform) return . getCmpElements . unLoc $ la) la

tryNewValue :: Typeable a => Located a -> a -> R (Located a)
tryNewValue oldValue@(L loc _) newValue = do
  oldOrmolu <- gets _ormolu
  let oldModule = prParsedSource oldOrmolu
      newModule = everywhere' (mkT (overwriteAtLoc loc newValue)) oldModule
  testAndUpdateStateFlex oldValue (L loc newValue) (oldOrmolu {prParsedSource = newModule})

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

testAndUpdateState :: OPR.ParseResult -> R ()
testAndUpdateState = testAndUpdateStateFlex () ()

testAndUpdateStateFlex :: a -> a -> OPR.ParseResult -> R a
testAndUpdateStateFlex a b newOrmolu = do
  sourceFile <- asks _sourceFile
  liftIO $ TIO.writeFile sourceFile . printModule $ newOrmolu
  runTest
    >>= \case
      Uninteresting -> return a
      Interesting -> do
        -- TODO: add information to change operation for better debugging messages
        liftIO $ putChar '.'
        modify $ \s -> s {_ormolu = newOrmolu}
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

changeDecls :: ([LHsDecl GhcPs] -> [LHsDecl GhcPs]) -> OPR.ParseResult -> OPR.ParseResult
changeDecls f oldOrmolu =
  let L moduleLoc oldModule = prParsedSource oldOrmolu
      allDecls = hsmodDecls oldModule
      newDecls = f allDecls
   in oldOrmolu {prParsedSource = L moduleLoc oldModule {hsmodDecls = newDecls}}

changeExports :: ([LIE GhcPs] -> [LIE GhcPs]) -> OPR.ParseResult -> OPR.ParseResult
changeExports f oldOrmolu =
  let L moduleLoc oldModule = prParsedSource oldOrmolu
      L exportsLoc oldExports = fromJust $ hsmodExports oldModule
      newExports = f (traceShow (concatMap ((++ " ") . lshow) oldExports) oldExports)
   in oldOrmolu {prParsedSource = L moduleLoc oldModule {hsmodExports = Just (L exportsLoc newExports)}}

changeImports :: ([LImportDecl GhcPs] -> [LImportDecl GhcPs]) -> OPR.ParseResult -> OPR.ParseResult
changeImports f oldOrmolu =
  let L moduleLoc oldModule = prParsedSource oldOrmolu
      allImports = hsmodImports oldModule
      newImports = f allImports
   in oldOrmolu {prParsedSource = L moduleLoc oldModule {hsmodImports = newImports}}

-- | run ghc with -Wunused-binds -ddump-json and delete decls that are mentioned there
getGhcOutput :: FilePath -> GhcMode -> IO (Maybe [(String, SL.RealSrcSpan)])
getGhcOutput sourcePath ghcMode = do
  pragmas <- getPragmas . T.unpack <$> TIO.readFile sourcePath
  let (dirName, fileName) = splitFileName sourcePath
      command             = "ghc " 
                            ++ ghcModeString 
                            ++ " -ddump-json " 
                            ++ unwords (("-X" ++) <$> pragmas) 
                            ++ " " ++ fileName
  (_, stdout, _) <- fromMaybe (error "getGhcOutput") 
                 <$> timeout duration
                             (readCreateProcessWithExitCode 
                             ((shell command) {cwd = Just dirName}) "")
  return $ case stdout of 
    "" -> Nothing
    _ -> do
      -- dropping first line because it doesn't fit into our JSON schema
      mapFunc . filterFunc <$> mapM (decode . pack) (lines stdout)
  where
    ghcModeString = case ghcMode of
      Binds   -> "-Wunused-binds"
      Imports -> "-Wunused-imports"
      Other   -> ""
    filterFunc = case ghcMode of
      Other -> filter (isSubsequenceOf "ot in scope:" . doc)
      _     -> filter (const True)
    mapFunc = case ghcMode of
      Other -> map (\o -> (gotQual $ doc o, span2SrcSpan . fromJust . UT.span $ o))
      _     -> error "implement me"

gotQual = fromMaybe "" . matchedText . (?=~ [re|([A-Za-z]+\.)+[A-Za-z#_]+|])


span2SrcSpan :: Span -> SL.RealSrcSpan
span2SrcSpan (Span f sl sc el ec) = SL.mkRealSrcSpan (SL.mkRealSrcLoc n sl sc) (SL.mkRealSrcLoc n el ec)
  where n = mkFastString f

showPragma :: String -> String
showPragma s = "{-# LANGUAGE " ++ s ++ " #-}"

-- TODO: use regexes here
getPragmas :: String -> [String]
getPragmas source = 
  filter (`isSubsequenceOf` source) 
         ["CPP", "BangPatterns", "MagicHash", "StandaloneDeriving", "TypeFamilies", "PatternGuards"]

debug :: MonadIO m => (a -> m ()) -> a -> m ()
debug f s
  | isInProduction = return ()
  | otherwise = f s

debugPrint :: MonadIO m => String -> m ()
debugPrint = debug (liftIO . putStrLn . ("[debug] " ++))

errorPrint :: MonadIO m => String -> m ()
errorPrint = debug (liftIO . putStrLn . ("[error] " ++))

isLanguagePragma :: OPP.Pragma -> Bool
isLanguagePragma (PragmaLanguage _) = True
isLanguagePragma _ = False

getPragmaStrings :: OPP.Pragma -> Maybe [String]
getPragmaStrings (PragmaLanguage ss) = Just ss
getPragmaStrings _ = Nothing

isInProduction :: Bool
isInProduction = False

duration = 20 * 1000 * 1000

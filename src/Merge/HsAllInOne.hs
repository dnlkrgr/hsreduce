module Merge.HsAllInOne (plugin, merge, fastCleanUp) where

import System.Posix.Files
import System.Directory
import Debug.Trace
import SrcLoc
import Name
import RdrName hiding (isQual, mkUnqual, mkQual)
import Control.Monad.Extra
import Control.Monad.Random
import Data.Char
import Data.Hashable
import Data.List
import GHC hiding (GhcMode)
import Path
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.RE.TDFA.Text hiding (Match)
import Data.Generics.Uniplate.Data
import GhcPlugins hiding ((<>), isQual, mkUnqual, qualName, GhcMode)
import TcRnTypes
import qualified Distribution.Parsec.Field as DPF
import qualified Distribution.Parsec.Parser as DPP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Word8 as W8
import Data.Maybe
import Data.Either
import Util.Util
import Util.Types
import Parser.Parser (getPragmas)


-- | take all renamed modules and the import module and merge it into one file
merge :: ProjectType -> FilePath -> IO ()
merge projectType cabal = do
  sections <- fromRight [] . DPP.readFields <$> BS.readFile cabal
  let root    = trace'' "root dir" show . parent . fromJust . parseAbsFile $ cabal
      srcDirs = trace'' "srcDirs" show . fromMaybe [] . getDirs projectType ["hs-source-dirs"] root $ sections

  pragmas <- fmap nub . getAllPragmas =<< map fromAbsFile <$> getFiles projectType srcDirs sections
  print pragmas

  -- get file contents of merged files
  let renamedFilesLocation = fromAbsDir root <> ".hsallinone/"

  fileContents <- 
    mapM TIO.readFile 
    =<< map (renamedFilesLocation <>) 
    . filter (/= "Imports") 
    <$> listDirectory renamedFilesLocation

  imports      <- 
    fmap (T.unlines . nub . T.lines) 
    . TIO.readFile 
    . fromAbsFile 
    $ root </> (fromJust $ parseRelFile ".hsallinone/Imports")

  let allInOne = 
        T.unlines (map (T.pack . show) pragmas) : "module AllInOne () where" : imports : fileContents

  TIO.writeFile (fromAbsDir root <> "AllInOne.hs") $ foldr ((<>) . (<> "\n")) T.empty allInOne


-- | rename modules and create one imports module
plugin :: Plugin
plugin = defaultPlugin {
    pluginRecompile = impurePlugin
  , renamedResultAction = \_ t r -> do
      let tempDirName = ".hsallinone"
          importsPath = ".hsallinone/Imports"

      liftIO $ flip unless (createDirectory tempDirName) =<< doesDirectoryExist tempDirName

      importPathExists <- liftIO $ fileExist importsPath

      allImports <- 
        if importPathExists
        then 
          (map (mkModuleName . T.unpack . last . T.words)) 
          . T.lines 
          <$> (liftIO $ TIO.readFile importsPath)
        else return []

      ours <- liftIO . fmap (map mkModuleName) . listDirectory $ tempDirName
      let imports  = T.unlines . map (T.pack . oshow) . qualImports . removeImports ours . tcg_rn_imports $ t

      let modName     = moduleName . tcg_mod $ t
          renamedMod  = T.pack . oshow . renameModule ours allImports $ r

      liftIO $ TIO.appendFile importsPath imports
      liftIO $ TIO.writeFile (tempDirName <> "/" <> moduleNameString modName) renamedMod
      return (t, r)
}


-- ***************************************************************************
-- CLEANING UP
-- ***************************************************************************
arst :: IO ()
arst = do
  f <- parseAbsFile "/home/daniel/workspace/hsreduce-test-cases/head.hackage/packages/pandoc-2.9.2.1/AllInOne.hs"
  fastCleanUp Cabal f

callCabal mode = do
  f <- parseAbsFile "/home/daniel/workspace/hsreduce-test-cases/head.hackage/packages/pandoc-2.9.2.1/AllInOne.hs"
  srcSpans <- getErrorOutput Cabal mode [] f
  print srcSpans
  return srcSpans


fastCleanUp :: Tool -> Path Abs File -> IO ()
fastCleanUp tool sourcePath = do
  cleanUp tool Indent sourcePath
  cleanUp tool MissingImport sourcePath -- add imports
  cleanUp tool HiddenImport sourcePath  -- clean up imports
  cleanUp tool NotInScope sourcePath  -- clean up uses of hidden modules
  -- cleanUp tool PerhapsYouMeant sourcePath

allowedToLoop = [Indent]

cleanUp :: Tool -> GhcMode -> Path Abs File -> IO ()
cleanUp tool mode sourcePath = do
  fileContent <- TIO.readFile (fromAbsFile sourcePath)
  imports <- map (last . T.words) . T.lines <$> TIO.readFile ((fromAbsDir $ parent sourcePath) <> ".hsallinone/Imports")

  getErrorOutput tool mode imports sourcePath >>= \case
    Nothing -> return ()
    Just [] -> return ()
    Just mySpans -> do
      banner (show mode)

      let 
          rightSpans = 
            (if mode == Indent then id else filter ((/="") . fst))
            . map (\(Right t, s) -> (t, s)) 
            . filter (isRight . fst) 
            $ mySpans

          newFileContent = case mode of

            Indent -> 
              foldr insertIndent fileContent 
              . map (fmap span2Locs) 
              $ rightSpans

            MissingImport -> 
              let contentLines = T.lines fileContent
                  prefix = T.unlines $ takeWhile (not . ("import qualified" `T.isPrefixOf`)) contentLines
                  suffix = T.unlines $ dropWhile (not . ("import qualified" `T.isPrefixOf`)) contentLines
              in
                prefix 
                <> (T.unlines 
                   . map ("import qualified " <>) 
                   . map (trace'' "cleanUp" show) 
                   . nub 
                   . map fst 
                   $ rightSpans) 
                <> suffix

            HiddenImport ->
              T.unlines 
              . map (\(i, l) -> 
                      let myLines = (map (\(m, s) -> (m, srcLocLine . fst . span2Locs $ s)) rightSpans) 
                      in 
                        if  i `elem` map snd myLines
                            && "import qualified" `T.isPrefixOf` l 
                        then 
                          let wl      = T.words l 
                              modName = fst . head $ filter ((==i) . snd) myLines
                          in traceShow ("Changing " 
                                        <> last wl 
                                        <> " to " 
                                        <> modName 
                                        <> " at line " 
                                        <> (T.pack $ show i)) 
                             . T.unwords 
                             $ init wl <> [modName] 
                        else l) 
              . zip [1..]
              . T.lines 
              $ fileContent
              
            PerhapsYouMeant -> 
              foldr replaceWithGhcOutput fileContent 
              . map (fmap span2Locs) 
              $ rightSpans

            NotInScope -> 
              foldr replaceWithGhcOutput fileContent 
              . map (fmap span2Locs) 
              $ rightSpans

      TIO.writeFile (fromAbsFile sourcePath) newFileContent
      when (mode `elem` allowedToLoop) $ cleanUp tool mode sourcePath

replaceWithGhcOutput :: (T.Text, (RealSrcLoc, RealSrcLoc)) -> T.Text -> T.Text
replaceWithGhcOutput (newName, (startLoc, endLoc)) fileContent
  = T.unlines $ prevLines ++ [traceShow ("changing at line " <> show (currentIndex + 1) <> ": " <> show oldName <> " -> " <> show realNewName) newLineContent] ++ succLines
  where
        contentLines   = T.lines fileContent
        lineStart      = srcLocLine startLoc
        colStart       = srcLocCol  startLoc
        colEnd         = srcLocCol  endLoc
        currentIndex   = lineStart-1
        prevLines      = take currentIndex contentLines
        succLines      = drop lineStart contentLines
        currentLine    = contentLines !! currentIndex
        -- currentLine    = traceShow ("length contentLines: " <> (show $ length contentLines)) $ traceShow ("currentIndex: " <> show currentIndex) $ contentLines !! currentIndex
        oldName        = T.take (colEnd - colStart) $ T.drop (colStart - 1) currentLine
        prefix         = T.take (colStart-1) currentLine
        suffix         = T.drop (colEnd-1) currentLine
        isInfix        = (==2) . T.length . T.filter (=='`') . T.drop (colStart-1) . T.take (colEnd-1) $ currentLine
        realNewName    = 
          if "Internal" `elem` T.words oldName
          then removeInternal id oldName
          else newName
        newLineContent =
          if isInfix
          then prefix <> "`" <> realNewName <> "`" <> suffix
          else prefix <> realNewName <> suffix

insertIndent :: (T.Text, (RealSrcLoc, RealSrcLoc)) -> T.Text -> T.Text
insertIndent (_, (startLoc, _)) fileContent =
  T.unlines $ prevLines ++ [traceShow ("inserting indent at line " <> show (currentIndex + 1)) $ newLineContent] ++ succLines
  where
        contentLines   = T.lines fileContent
        lineStart      = srcLocLine startLoc
        currentIndex   = lineStart-1
        prevLines      = take currentIndex contentLines
        succLines      = drop lineStart contentLines
        currentLine    = contentLines !! currentIndex
        newLineContent = "  " <> currentLine

span2Locs :: RealSrcSpan -> (RealSrcLoc, RealSrcLoc)
span2Locs s = (realSrcSpanStart s, realSrcSpanEnd s)


-- ***************************************************************************
-- MERGING MODULES
-- ***************************************************************************
renameModule :: [ModuleName]
             -> [ModuleName]
             -> HsGroup GhcRn
             -> HsGroup GhcRn
renameModule ours imports r = do
      descendBi unqualSig
    . descendBi (unqualOurNames ours)
    . descendBi unqualBinds
    . descendBi (renameAbiguousField ours)
    . descendBi (renameField ours)
    . descendBi (renameName ours imports)
    $ r

renameName :: [ModuleName] -> [ModuleName] -> Name -> Name
renameName ours imports n
  -- don't rename things from the Main module to avoid making the test case print uninteresting output
  -- example: data cons renamed from T2 to T2_Main
  --   doesn't match in interestingness test matching on `fromList [T2, T2]`
  | showSDocUnsafe (pprNameUnqualified n) == "main" = n

  -- built-in things
  | isSystemName    n    = n
  | isWiredInName   n    = n
  | isBuiltInSyntax n    = n
  | isDataConName   n
  , isOperator $ tail os = n

  -- not our operator, leave unmodified
  | isOperator . oshow $ getRdrName n
  , Just mn <- getModuleName n
  , mn `notElem` ours = n

  -- our operator
  | Just mn <- getModuleName n
  , isOperator . oshow $ getRdrName n
  , mn `elem` ours
  = tidyNameOcc n . mkOccName ns . renameOperator $ (os ++ filter (/= '.') (moduleNameString mn))

  -- our function / variable
  | Just mn <- getModuleName n
  , mn `elem` ours
  = tidyNameOcc n $ mangle mn on

  -- something external
  | Just mn <- getModuleName n
  = tidyNameOcc n $ qualName imports mn on

  | otherwise = n
  where
    on = occName n
    ns = occNameSpace on
    os = occNameString on


renameAbiguousField :: [ModuleName ] 
                    -> AmbiguousFieldOcc GhcRn 
                    -> AmbiguousFieldOcc GhcRn
renameAbiguousField ours (Unambiguous n (L l rn))
  | Just mn <- moduleName <$> nameModule_maybe n
  , mn `elem` ours
  = Unambiguous n $ L l $ Unqual $ mangle mn on

  | Just mn <- moduleName <$> nameModule_maybe n
  = Unambiguous n $ L l $ Qual mn on

  | otherwise = Unambiguous n $ L l $ Unqual $ traceShow (oshow $ nameModule_maybe n) $ traceShow (nameStableString n) $ undefined
  where on = rdrNameOcc rn

renameAbiguousField ours (Ambiguous x (L l (Qual mn on)))
  | mn `elem` ours
  = Ambiguous x $ L l $ Unqual $ mangle mn on
  -- don't know what to do otherwise yet, let it crash for now
renameAbiguousField _ a = a


renameField :: [ModuleName] -> FieldOcc GhcRn -> FieldOcc GhcRn
renameField ours (FieldOcc n (L l rn))
  | Just mn <- moduleName <$> nameModule_maybe n
  , mn `elem` ours
  = FieldOcc n (L l $ Unqual $ mangle mn on)
  -- = traceShow ("renameField") $ traceShow (nameStableString n) $ FieldOcc n (L l $ Unqual $ mangle mn on)


  | Just mn <- moduleName <$> nameModule_maybe n
  = FieldOcc n (L l $ Qual mn on)
  -- = traceShow ("renameField") $ traceShow (nameStableString n) $ FieldOcc n (L l $ Qual mn on)
  -- | otherwise = traceShow "renameField" $ traceShow (nameStableString n) $ traceShow (oshow $ nameModule_maybe n) FieldOcc n (L l $ Unqual $ mangle we on)
  where on = rdrNameOcc rn
renameField _ a = a


mangle :: ModuleName -> OccName -> OccName
mangle mn on = mkOccName ns $ os ++ "_" ++ filter (/= '.') (moduleNameString mn)
  where
    os = occNameString on
    ns = occNameSpace on

getModuleName :: Name -> Maybe ModuleName
getModuleName = fmap moduleName . nameModule_maybe


-- ***************************************************************************
-- MERGING MODULES UTILITIES
-- ***************************************************************************
unqualOurNames :: [ModuleName] -> Name -> Name
unqualOurNames ours n
  | Just m <- nameModule_maybe n
  , let mn = moduleName m  in mn `elem` ours
  = unqualName n
  | otherwise = n

unqualSig :: Sig GhcRn -> Sig GhcRn 
unqualSig (TypeSig x names t) = TypeSig x (map (fmap unqualName) names) t
unqualSig (ClassOpSig x b names t) = ClassOpSig x b (map (fmap unqualName) names) t
unqualSig s = s

unqualBinds :: HsBindLR GhcRn GhcRn -> HsBindLR GhcRn GhcRn
unqualBinds fb@(FunBind _ (L l n) fm  _ _)
  | isQual (T.pack $ oshow n) = newFB
  | otherwise = fb
  where newFM = descendBi unqualMatchCtxt fm
        newFB = fb { fun_id = L l $ unqualName n, fun_matches = newFM }
unqualBinds hb = hb

unqualMatchCtxt :: HsMatchContext Name -> HsMatchContext Name
unqualMatchCtxt = fmap unqualName

unqualName :: Name -> Name
unqualName n = tidyNameOcc n $ mkOccName ns $ T.unpack $ unqualText (T.pack $ oshow n)
   where
     on = occName n
     ns = occNameSpace on

unqualText :: T.Text -> T.Text
unqualText =
  (*=~/ [ed|(([A-Z][A-Za-z0-9_]*)\.)+${e}([A-Za-z][A-Za-z0-9_']*)///${e}|])

--  | make all imports qualified, hiding nothing, no aliasing and no safe imports
qualImports :: [LImportDecl GhcRn] -> [LImportDecl GhcRn]
qualImports = map (\(L l i) ->
                       L l
                       $ i { ideclQualified = True
                           , ideclHiding    = Nothing
                           , ideclAs        = Nothing
                           , ideclSafe      = False})

-- | remove imports that come from "our" modules
removeImports :: [ModuleName] -> [LImportDecl GhcRn] -> [LImportDecl GhcRn]
removeImports ours = filter go
  where
    go (L _ i) = let modName = unLoc . ideclName $ i
                 in modName `notElem` ours && ("Prelude" /= (moduleNameString modName))

-- | qualify an occ name
qualName :: [ModuleName] -> ModuleName -> OccName -> OccName
qualName imports mn on
  = mkOccName ns $ modulenameSansInternal ++ "." ++ oshow on
  where ns = occNameSpace on
        modulenameSansInternal = moduleNameString mn 
          -- trace'' "module name after" id 
          -- . moduleNameString
          -- . handleHiddenModules imports 
          -- . traceShow ("module name before: " <> oshow mn)
          -- $ mn 

handleHiddenModules :: [ModuleName] -> ModuleName -> ModuleName
handleHiddenModules imports mn
  | all (not . (`isPrefixOf` moduleNameString mn) . moduleNameString) imports = mn
  | mn `elem` imports = mn
  | length components >= 2 = let newMN = mkModuleName . intercalate "." . map T.unpack . init $ components
           in handleHiddenModules imports newMN
  | otherwise = mn
  where components = modname2components . T.pack $ moduleNameString mn

brst input = do
  allImports <- (map (mkModuleName . T.unpack . last . T.words)) . T.lines <$> (TIO.readFile "/home/daniel/workspace/hsreduce-test-cases/head.hackage/packages/pandoc-2.9.2.1/.hsallinone/Imports")
  print $ filter (`isPrefixOf` input) $ map moduleNameString allImports
  putStrLn . oshow $ handleHiddenModules allImports $ mkModuleName input
  return ()

-- | too naive check if something is an operator
-- TODO: use syntax from Haskell2010 report
isOperator :: String -> Bool
isOperator = not . any isAlphaNum

renameOperator :: String -> String
renameOperator = evalRand randomOpString . mkStdGen . hash

-- | create a random operator string
randomOpString :: MonadRandom m => m String
randomOpString = do
  s1 <-
    replicateM
      4
      ( do
          i <- getRandomR (0, length operatorSymbols - 1)
          return $ operatorSymbols !! i
      )
  b <- getRandom
  s2 <-
    if b
      then randomOpString
      else return ""
  return $ "<" <> s1 <> s2 <> ">"
  where
    operatorSymbols = "!#$%&*+<>?@^~"


-- ***************************************************************************
-- CABAL FILE UTILITIES
-- ***************************************************************************


getFiles :: ProjectType -> [Path Abs Dir] -> [DPF.Field a] -> IO [Path Abs File]
getFiles projectType srcDirs =
    filterM (doesFileExist . fromAbsFile)
  . fromJust
  . fmap (concatMap (\n -> map (\d -> d </> n) srcDirs) . concat)
  . traverse (file2Paths modName2FileName ["exposed-modules", "other-modules", "main-is"])
  . keepFields [show projectType]

file2Paths :: (FilePath -> Maybe (Path Rel a))
                -> [BS.ByteString]
                -> DPF.Field b
                -> Maybe [Path Rel a]
file2Paths g names (DPF.Section _ _ f) =
  concat <$> traverse (file2Paths g names) f
file2Paths g names (DPF.Field n f)
  | BS.map W8.toLower (DPF.getName n) `elem` names
  = sequence . filter isJust . map g $ map (B8.unpack . fieldLineBS) f
  | otherwise = return []

getDirs :: ProjectType 
        -> [BS.ByteString]
        -> Path Abs Dir
        -> [DPF.Field a]
        -> Maybe [Path Abs Dir]
getDirs projectType dirNames rootPath sections = do
  relPaths <- 
    fmap concat 
    . traverse (file2Paths parseRelDir dirNames) 
    . keepFields [show projectType] 
    $ sections

  relPathsFixed <-
    sequence
    . filter isJust
    . concatMap (map parseRelDir
                 . words
                 . map (\c -> if c == ',' then ' ' else c)
                 . fromRelDir)
    $ relPaths

  fmap (map (rootPath </>)) .
    traverse (parseRelDir . filter (/= ',') . fromRelDir) $ relPathsFixed

modName2FileName :: String -> Maybe (Path Rel File)
modName2FileName f
   | f == "Main.hs" = parseRelFile f
   | ".hs" `isSubsequenceOf` f =
     let fileName = take (length f - 3) f
     in parseRelFile $ map (\case '.' -> '/'; c -> c) fileName ++ ".hs"
   | otherwise = parseRelFile $ (map (\case '.' -> '/'; c -> c) $ filter ((/= ' ') <&&> (/= ',')) f) ++ ".hs"

keepFields :: [String] -> [DPF.Field a] -> [DPF.Field a]
keepFields names =
  filter ((`elem` map B8.pack names) . BS.map W8.toLower . DPF.getName . DPF.fieldName)

fieldLineBS :: DPF.FieldLine a -> B8.ByteString
fieldLineBS (DPF.FieldLine _ bs) = bs

getAllPragmas :: [FilePath] -> IO [Pragma]
getAllPragmas interestingFiles = do
  fmap concat . mapM getPragmas . map (fromJust . parseAbsFile) $ interestingFiles

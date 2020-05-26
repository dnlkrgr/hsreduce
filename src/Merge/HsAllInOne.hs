module Merge.HsAllInOne (plugin, merge, fastCleanUp) where

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
import GHC
import Path
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.RE.TDFA.Text hiding (Match)
import Data.Generics.Uniplate.Data
import GhcPlugins hiding ((<>), isQual, mkUnqual, qualName)
import TcRnTypes
import qualified Distribution.Parsec.Field as DPF
import qualified Distribution.Parsec.Parser as DPP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Word8 as W8
import Data.Maybe
import Data.Either
import Util.Util
import Util.Types (Tool(..), Pragma, GhcMode(Other))
import Parser.Parser (getPragmas)


-- | take all renamed modules and the import module and merge it into one file
merge :: ProjectType -> FilePath -> IO ()
merge projectType cabal = do
  sections <- fromRight [] . DPP.readFields <$> BS.readFile cabal
  let root = parent . fromJust . parseAbsFile $ cabal
      srcDirs = fromMaybe [] . getDirs ["hs-source-dirs"] root $ sections

  pragmas <- fmap nub . getAllPragmas =<< map fromAbsFile <$> getFiles projectType srcDirs sections
  print pragmas

  -- get file contents of merged files
  let renamedFilesLocation = fromAbsDir root <> ".hsallinone/"
  fileContents <- mapM TIO.readFile =<< map (renamedFilesLocation <>) . filter (/= "Imports") <$> listDirectory renamedFilesLocation
  imports      <- fmap (T.unlines . nub . T.lines) . TIO.readFile . fromAbsFile $ root </> (fromJust $ parseRelFile ".hsallinone/Imports")

  let newFileContents = 
        T.unlines (map (T.pack . show) pragmas) : "module AllInOne () where" : imports : fileContents
  TIO.writeFile (fromAbsDir root <> "AllInOne.hs") $ foldr ((<>) . (<> "\n")) T.empty newFileContents

-- | rename modules and create one imports module
plugin :: Plugin
plugin = defaultPlugin {
    pluginRecompile = impurePlugin
  , renamedResultAction = \_ t r -> do
      let tempDirName = ".hsallinone"
      b <- liftIO $ doesDirectoryExist tempDirName
      unless b . liftIO $ createDirectory tempDirName

      ours <- liftIO . fmap (map mkModuleName) . listDirectory $ tempDirName
      -- let imports     = map unLoc . tcg_rn_imports $ t
      let imports  = T.unlines . map (T.pack . oshow) . qualImports . removeImports ours . tcg_rn_imports $ t

      -- let newOurs = 
      --       map unLoc 
      --       . concatMap (\i -> 
      --                     case ideclAs i of
      --                       Nothing -> [ideclName i]
      --                       Just iMn -> if ideclQualified i 
      --                         then [iMn]
      --                         else [ideclName i, iMn]) 
      --       . filter ((`elem` ours) . unLoc . ideclName) 
      --       $ imports

      -- liftIO $ mapM (putStrLn . oshow) newOurs

      let modName     = moduleName . tcg_mod $ t
          renamedMod  = T.pack . oshow . renameModule modName ours $ r

      liftIO . TIO.appendFile (tempDirName <> "/" <> "Imports") $ imports
      liftIO . TIO.writeFile  (tempDirName <> "/" <> moduleNameString modName)   $ renamedMod
      return (t, r)
}

-- ***************************************************************************
-- CLEANING UP
-- ***************************************************************************
arst :: IO ()
arst = do
  f <- parseAbsFile "/home/daniel/workspace/hsreduce-test-cases/protocol-buffers/descriptor/AllInOne.hs"
  fastCleanUp Cabal f

callCabal :: IO ()
callCabal = do
  f <- parseAbsFile "/home/daniel/workspace/hsreduce-test-cases/protocol-buffers/descriptor/AllInOne.hs"
  srcSpans <- getGhcOutput f Cabal Other
  print srcSpans
  return ()


fastCleanUp :: Tool -> Path Abs File -> IO ()
fastCleanUp tool sourcePath = do
  fileContent <- TIO.readFile (fromAbsFile sourcePath)
  cleanUp tool sourcePath fileContent

cleanUp :: Tool -> Path Abs File -> T.Text -> IO ()
cleanUp tool sourcePath fileContent =
  getGhcOutput sourcePath tool Other >>= \case
    Nothing -> return ()
    Just [] -> return ()
    Just mySpans -> do
      banner "new clean up iteration" 
      -- let newFileContent = foldr replaceWithGhcOutput fileContent . map (fmap span2Locs) $ mySpans
      let newFileContent = (T.unlines . map ("import qualified " <>) . map (trace'' "cleanUp" show) . nub . filter (/= "") . map fst $ mySpans) <> fileContent
      TIO.writeFile (fromAbsFile sourcePath) newFileContent
      -- cleanUp tool sourcePath newFileContent

-- mkIndent :: (T.Text, (RealSrcLoc, RealSrcLoc)) -> T.Text -> T.Text
-- mkIndent (_, (startLoc, _)) fileContent =
--   T.unlines $ prevLines ++ [newLineContent] ++ succLines
--   where
--         contentLines   = T.lines fileContent
--         lineStart      = srcLocLine startLoc
--         currentIndex   = lineStart-1
--         prevLines      = take currentIndex contentLines
--         succLines      = drop lineStart contentLines
--         lineContent    = contentLines !! currentIndex
--         newLineContent = "  " <> lineContent

-- replaceWithGhcOutput :: (T.Text, (RealSrcLoc, RealSrcLoc)) -> T.Text -> T.Text
-- replaceWithGhcOutput (newName, (startLoc, endLoc)) fileContent
--   = trace'' "replaceWithGhcOutput" (const (show $ "replacing `" <> oldName <> "` with `" <> newName <> "`")) $ T.unlines $ prevLines ++ [newLineContent] ++ succLines
--   where
--         contentLines   = T.lines fileContent
--         lineStart      = srcLocLine startLoc
--         colStart       = srcLocCol  startLoc
--         colEnd         = srcLocCol  endLoc
--         currentIndex   = lineStart-1
--         prevLines      = take currentIndex contentLines
--         succLines      = drop lineStart contentLines
--         lineContent    = contentLines !! currentIndex
--         -- lineContent    = traceShow ("length contentLines: " <> (show $ length contentLines)) $ traceShow ("currentIndex: " <> show currentIndex) $ contentLines !! currentIndex
--         prefix         = T.take (colStart-1) lineContent
--         suffix         = T.drop (colEnd-1) lineContent
--         oldName        = T.take (colEnd - colStart) $ T.drop (colStart-1) lineContent
--         isInfix        = 
--           (==2) 
--           . T.length 
--           . T.filter (=='`') 
--           . T.drop (colStart-1) 
--           . T.take (colEnd-1) 
--           $ lineContent
--         newLineContent =
--           if isInfix
--           then prefix <> "`" <> newName <> "`" <> suffix
--           else prefix <> newName <> suffix

-- span2Locs :: RealSrcSpan -> (RealSrcLoc, RealSrcLoc)
-- span2Locs s = (realSrcSpanStart s, realSrcSpanEnd s)


-- ***************************************************************************
-- MERGING MODULES
-- ***************************************************************************
renameModule :: ModuleName
             -> [ModuleName]
             -> HsGroup GhcRn
             -> HsGroup GhcRn
renameModule modName ours r = do
    -- descendBi (unqualOurRdrNames ours)
    -- . descendBi (renameRdrName ours env)
      descendBi unqualSig
    . descendBi (unqualOurNames ours)
    . descendBi unqualBinds
    . descendBi (renameAbiguousField ours modName)
    . descendBi (renameField ours)
    . descendBi (renameName ours)
    -- . descendBi arstbrst
    $ r

-- arstbrst :: Name -> Name
-- arstbrst n
--   = traceShow ("" :: String)  $ traceShow (showSDocUnsafe $ pprNameSpace ns) $ traceShow os $ traceShow (oshow . nameModule_maybe $ n) $ traceShow (nameStableString n) $ n
--   where on = occName n
--         ns = occNameSpace on
--         os = occNameString on


--unqualOurRdrNames :: [ModuleName] -> RdrName -> RdrName
--unqualOurRdrNames ours rn@(Qual mn on)
--  | traceShow ("mn: " ++ oshow mn) mn `elem` map (trace'' "ours" oshow) ours = Unqual $ traceShow "qual->unqual: " $ traceShow (oshow on) $ traceShow (gshow on) $ mkOccName ns $ T.unpack $ mkUnqual (T.pack $ oshow on)
--  | otherwise = rn
--  where
--    ns = occNameSpace on
--unqualOurRdrNames _ n = n

renameName :: [ModuleName] -> Name -> Name
renameName ours n
  -- don't rename things from the Main module to avoid making the test case print uninteresting output
  -- example: data cons renamed from T2 to T2_Main
  --   doesn't match in interestingness test matching on `fromList [T2, T2]`
  | showSDocUnsafe (pprNameUnqualified n) == "main" = n

  -- built-in things
  | isSystemName    n    = traceShow ("renameName - SYSTEM NAME: " ++ nameStableString n) n
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
  = tidyNameOcc n $ qualName ours mn on

  | otherwise = n
  where
    on = occName n
    ns = occNameSpace on
    os = occNameString on


-- -- TODO: also include current module name and mangle names with that if it's not an external name
-- renameRdrName :: [ModuleName] -> RdrName -> RdrName
-- renameRdrName ours rn@(Qual mn on)
--   | mn `elem` ours 
--   = Unqual $ mangle mn on
-- 
--   | otherwise = rn
-- renameRdrName _ rn = rn

renameAbiguousField :: [ModuleName ] 
                    -> ModuleName 
                    -> AmbiguousFieldOcc GhcRn 
                    -> AmbiguousFieldOcc GhcRn
renameAbiguousField ours we (Unambiguous n (L l rn))
  | Just mn <- moduleName <$> nameModule_maybe n
  , mn `elem` ours
  = Unambiguous n $ L l $ Unqual $ mangle mn on

  | Just mn <- moduleName <$> nameModule_maybe n
  = Unambiguous n $ L l $ Qual mn on

  | otherwise = traceShow "renameAbiguousField" $ traceShow "Unambiguous" $ Unambiguous n $ L l $ Unqual $ traceShow (oshow $ nameModule_maybe n) $ traceShow (nameStableString n) $ undefined
  where on = rdrNameOcc rn

renameAbiguousField ours we (Ambiguous x (L l (Qual mn on)))
  | mn `elem` ours
  = Ambiguous x $ L l $ Unqual $ mangle mn on
  -- don't know what to do otherwise yet, let it crash for now
renameAbiguousField _ _ a = a

-- renameAbiguousField we (Unambiguous n (L l (Unqual on)))
--   | Just mn <- moduleName <$> nameModule_maybe n
--   , "$main$" `isPrefixOf `nameStableString n 
--   = Unambiguous n $ L l $ Unqual $ mangle mn on
-- 
--   | otherwise = Unambiguous n $ L l $ Unqual $ mangle we on


renameField :: [ModuleName] -> FieldOcc GhcRn -> FieldOcc GhcRn
renameField ours f@(FieldOcc n (L l rn))
  | Just mn <- moduleName <$> nameModule_maybe n
  , mn `elem` ours
  = FieldOcc n (L l $ Unqual $ mangle mn on)

  | Just mn <- moduleName <$> nameModule_maybe n
  = FieldOcc n (L l $ Qual mn on)
  -- | otherwise = traceShow "renameField" $ traceShow (nameStableString n) $ traceShow (oshow $ nameModule_maybe n) FieldOcc n (L l $ Unqual $ mangle we on)
  where on = rdrNameOcc rn
renameField _ a = a

-- renameField we f@(FieldOcc n (L l (Qual mn on)))
--   | mn `elem` ours
--   = FieldOcc n (L l $ Unqual $ mangle mn on)

-- renameField we (FieldOcc n (L l (Unqual on)))
--   | Just mn <- moduleName <$> nameModule_maybe n
--   , "$main$" `isPrefixOf `nameStableString n 
--   = FieldOcc n (L l $ Unqual $ mangle mn on)
--       
--   | otherwise = FieldOcc n (L l $ Unqual $ mangle we on)
-- renameField _ c = c


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
qualName _ mn on
  = mkOccName ns $ moduleNameString mn ++ "." ++ oshow on
  where ns = occNameSpace on

  -- | mn `elem` ours
  -- = tidyNameOcc n $ mkOccName ns $ moduleNameString mn ++ "." ++ oshow on
 
  -- | Just i <- fst <$> bestMatchingImport
  -- = tidyNameOcc n $ mkOccName ns $ moduleNameString (snd $ (zip [0..] ours) !! i) ++ "." ++ oshow on
 
  -- where moduleComponents = getNameComponents mn
  --       matchingImports =
  --           reverse
  --         . sortOn snd
  --         . map (fmap (matchingComponents moduleComponents) . fmap getNameComponents)
  --         . zip [0..]
  --         $ ours
  --       bestMatchingImport = if length matchingImports >=1 && fst (head matchingImports) > 2 && length moduleComponents - snd (head matchingImports) == 1
  --         then Just $ head matchingImports
  --         else Nothing

-- matchingComponents :: Eq a => [a] -> [a] -> Int
-- matchingComponents l = length . filter (`elem` l)
-- 
-- getNameComponents :: ModuleName -> [String]
-- getNameComponents = words . map (\c -> if c == '.' then ' ' else c) . moduleNameString


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

data ProjectType = Executable | Library

getFiles :: ProjectType -> [Path Abs Dir] -> [DPF.Field a] -> IO [Path Abs File]
getFiles projectType srcDirs =
    filterM (doesFileExist . fromAbsFile)
  . fromJust
  . fmap (concatMap (\n -> map (\d -> d </> n) srcDirs) . concat)
  . traverse (file2Paths modName2FileName ["exposed-modules", "other-modules", "main-is"])
  . keepFields [case projectType of
                  Executable -> "executable"
                  Library    -> "library"]

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

getDirs :: [BS.ByteString]
        -> Path Abs Dir
        -> [DPF.Field a]
        -> Maybe [Path Abs Dir]
getDirs dirNames rootPath sections = do
  relPaths <- concat <$> traverse (file2Paths parseRelDir dirNames) sections

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
   | otherwise = parseRelFile $ map (\case '.' -> '/'; c -> c) f ++ ".hs"

keepFields :: [String] -> [DPF.Field a] -> [DPF.Field a]
keepFields names =
  filter ((`elem` map B8.pack names) . BS.map W8.toLower . DPF.getName . DPF.fieldName)

fieldLineBS :: DPF.FieldLine a -> B8.ByteString
fieldLineBS (DPF.FieldLine _ bs) = bs

getAllPragmas :: [FilePath] -> IO [Pragma]
getAllPragmas interestingFiles = do
  fmap concat . mapM getPragmas . map (fromJust . parseAbsFile) $ interestingFiles

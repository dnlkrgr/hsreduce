module Merge.HsAllInOne where

import Data.Maybe
import System.Directory
import Data.Either
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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Distribution.Parsec.Parser as DPP (readFields)
import qualified Distribution.Parsec.Field as DPF
import Text.RE.TDFA.Text
import Util.Util
import Util.Types
import Parser.Parser
import Data.Generics.Uniplate.Data
import qualified Data.Word8 as W8
import GhcPlugins hiding ((<>), isQual, mkUnqual)
import TcRnTypes
import System.Posix.Files

-- | take all renamed modules and the import module and merge it into one file
mergeAllInFolder = do
  files        <- listDirectory ".hsallinone"
  imports      <- TIO.readFile . (".hsallinone/" <>) . head . filter (== "Imports") $ files
  fileContents <- mapM TIO.readFile . map (".hsallinone/" <>) . filter (/= "Imports") $ files
  TIO.writeFile "AllInOne.hs" $ foldr ((<>) . (<> "\n")) T.empty (imports:fileContents)

-- | rename modules and create one imports module
plugin :: Plugin
plugin = defaultPlugin {
      pluginRecompile = impurePlugin
    , renamedResultAction = \_ t r -> do
        let tempDirName = ".hsallinone"
        b <- liftIO $ doesDirectoryExist tempDirName
        unless b . liftIO $ createDirectory tempDirName

        ours <- liftIO . fmap (map mkModuleName) . listDirectory $ tempDirName
        let newImports  = T.unlines . map (T.pack . oshow) . mkImportsQual . removeImports ours . tcg_rn_imports $ t
        let importNames = map (unLoc . ideclName . unLoc) . mkImportsQual . removeImports ours . tcg_rn_imports $ t
            modName     = moduleName . tcg_mod $ t
            renamedMod  = T.pack . oshow . renameModule modName ours importNames $ r

        liftIO . TIO.appendFile (tempDirName <> "/" <> "Imports") $ newImports
        liftIO . TIO.writeFile  (tempDirName <> "/" <> moduleNameString modName)   $ renamedMod
        return (t, r)
}

--   banner "Cleaning up"
-- 
--   getGhcOutput sourcePath ParseIndent >>= \case
--     Nothing -> do
--       cleanUp sourcePath fileContent
--     Just [] -> do
--       cleanUp sourcePath fileContent
--     Just mySpans -> do
--       let newFileContent = foldr mkIndent fileContent . map (fmap span2Locs) $ mySpans
--       TIO.writeFile (fromAbsFile sourcePath) newFileContent
-- 
--       cleanUp sourcePath newFileContent
-- 
--   -- mkNewCabal filePath

-- ***************************************************************************
-- CLEANING UP
-- ***************************************************************************
arst = do
  f <- parseAbsFile "/home/daniel/workspace/hsreduce-test-cases/protocol-buffers/descriptor/AllInOne.hs"
  fastCleanUp Cabal f

callCabal = do
  f <- parseAbsFile "/home/daniel/workspace/hsreduce-test-cases/protocol-buffers/descriptor/AllInOne.hs"
  srcSpans <- getGhcOutput f Cabal Other
  print srcSpans
  return ()

fastCleanUp :: Tool -> Path Abs File -> IO ()
fastCleanUp tool sourcePath = do
  print (fromAbsFile sourcePath)
  fileContent <- TIO.readFile (fromAbsFile sourcePath)
  print $ T.length fileContent
  cleanUp tool sourcePath fileContent

cleanUp :: Tool -> Path Abs File -> T.Text -> IO ()
cleanUp tool sourcePath fileContent =
  getGhcOutput sourcePath tool Other >>= \case
    Nothing -> return ()
    Just [] -> return ()
    Just mySpans -> do
      let newFileContent = foldr replaceWithGhcOutput fileContent . (\l -> traceShow ("length of locs: " <> (show $ length l)) l) . map (fmap span2Locs) $ mySpans
      TIO.writeFile (fromAbsFile sourcePath) newFileContent
      -- cleanUp tool sourcePath newFileContent

mkIndent :: (T.Text, (RealSrcLoc, RealSrcLoc)) -> T.Text -> T.Text
mkIndent (_, (startLoc, _)) fileContent =
  T.unlines $ prevLines ++ [newLineContent] ++ succLines
  where
        contentLines   = T.lines fileContent
        lineStart      = srcLocLine startLoc
        currentIndex   = lineStart-1
        prevLines      = take currentIndex contentLines
        succLines      = drop lineStart contentLines
        lineContent    = contentLines !! currentIndex
        newLineContent = "  " <> lineContent

replaceWithGhcOutput :: (T.Text, (RealSrcLoc, RealSrcLoc)) -> T.Text -> T.Text
replaceWithGhcOutput (newName, (startLoc, endLoc)) fileContent
  = T.unlines $ prevLines ++ [newLineContent] ++ succLines
  where
        contentLines   = T.lines fileContent
        lineStart      = srcLocLine startLoc
        colStart       = srcLocCol  startLoc
        colEnd         = srcLocCol  endLoc
        currentIndex   = lineStart-1
        prevLines      = take currentIndex contentLines
        succLines      = drop lineStart contentLines
        lineContent    = contentLines !! currentIndex
        -- lineContent    = traceShow ("length contentLines: " <> (show $ length contentLines)) $ traceShow ("currentIndex: " <> show currentIndex) $ contentLines !! currentIndex
        prefix         = T.take (colStart-1) lineContent
        suffix         = T.drop (colEnd-1) lineContent
        isInfix        = (==2) . T.length . T.filter (=='`') . T.drop (colStart-1) . T.take (colEnd-1) $ lineContent
        newLineContent =
          if isInfix
          then prefix <> "`" <> newName <> "`" <> suffix
          else prefix <> newName <> suffix

span2Locs :: RealSrcSpan -> (RealSrcLoc, RealSrcLoc)
span2Locs s = (realSrcSpanStart s, realSrcSpanEnd s)


-- ***************************************************************************
-- MERGING MODULES
-- ***************************************************************************
renameModule :: ModuleName
             -> [ModuleName]
             -> [ModuleName]
             -> HsGroup GhcRn
             -> HsGroup GhcRn
renameModule modName ours importNames r = do
      transformBi (unqualOurNames ours)
    . transformBi unqualBinds
    . transformBi (qualAmbiguousField modName)
    . transformBi (qualField modName)
    . transformBi (name2UniqueName importNames)
    $ r

name2UniqueName :: [ModuleName] -> Name -> Name
name2UniqueName importNames n
  -- don't rename things from the Main module to avoid making the test case print uninteresting output
  -- example: data cons renamed from T2 to T2_Main
  --   doesn't match in interestingness test matching on `fromList [T2, T2]`
  | "$main$Main$" `isPrefixOf` nameStableString n = n

  -- built-in things
  | isSystemName    n                = traceShow ("name2UniqueName - SYSTEM NAME: " ++ nameStableString n) n
  | isWiredInName   n                = n
  | isBuiltInSyntax n                = n
  | isDataConName   n
  , isOperator $ tail os = n

  -- not our operator, do not touch
  | isOperator . oshow $ getRdrName n,
    not $ "$main$" `isPrefixOf` nameStableString n = n

  -- our operator
  | Just m <- nameModule_maybe n
  , isOperator . oshow $ getRdrName n
  , "$main$" `isPrefixOf` nameStableString n =
                                               tidyNameOcc n
                                             . mkOccName ns
                                             . renameOperator
                                             $ (os ++ filter (/= '.') (moduleNameString (moduleName m)))

  -- our function / variable
  | Just m <- nameModule_maybe n,
    "$main$" `isPrefixOf` nameStableString n
  = tidyNameOcc n
  . mkOccName ns
  $ os
  ++ "_"
  ++ filter (/= '.') (moduleNameString (moduleName m))

  -- something external
  | all (not . (`isPrefixOf` nameStableString n)) blacklist, Just m <- nameModule_maybe n
  = mkNameQual importNames (moduleName m) on ns n

  | otherwise = n
  where
    on = occName n
    os = occNameString on
    ns = occNameSpace on
    blacklist = ["$main$", "$ghc-prim$", "$base$"]

-- ***************************************************************************
-- MERGING MODULES UTILITIES
-- ***************************************************************************
unqualOurNames :: [ModuleName] -> Name -> Name
unqualOurNames ours n
  | Just m <- nameModule_maybe n
  , moduleName m `elem` ours
  = unqualName n

  | otherwise = n

unqualBinds :: HsBindLR GhcRn GhcRn -> HsBindLR GhcRn GhcRn
unqualBinds fb@(FunBind _ (L l n) fm  _ _)
  | isQual (T.pack $ oshow n) = newFB
  | otherwise = fb
  where newFM = transformBi unqualMatchCtxt fm
        newFB = fb { fun_id = L l $ unqualName n, fun_matches = newFM }
unqualBinds hb = hb

unqualMatchCtxt :: HsMatchContext Name -> HsMatchContext Name
unqualMatchCtxt = fmap unqualName

unqualName :: Name -> Name
unqualName n = tidyNameOcc n $ mkOccName ns $ T.unpack $ mkUnqual (T.pack $ oshow n)
   where
     on = occName n
     ns = occNameSpace on

qualAmbiguousField :: ModuleName -> AmbiguousFieldOcc GhcRn -> AmbiguousFieldOcc GhcRn
qualAmbiguousField modName a@(Unambiguous n (L l (Unqual on)))
  | Just mn <- moduleName <$> nameModule_maybe n
  , "$main$" `isPrefixOf `nameStableString n =
  Unambiguous n $ L l $ Unqual $ mkOccName ns $ os ++ "_" ++ filter (/= '.') (moduleNameString mn)
  | otherwise = Unambiguous n $ L l $ Unqual $ mkOccName ns $ os ++ "_" ++ filter (/= '.') (moduleNameString modName)
  where
    os = occNameString on
    ns = occNameSpace on
qualAmbiguousField _ a = a

qualField :: ModuleName -> FieldOcc GhcRn -> FieldOcc GhcRn
qualField modName c@(FieldOcc n (L l (Unqual on)))
  | Just mn <- moduleName <$> nameModule_maybe n
  , "$main$" `isPrefixOf `nameStableString n =
      FieldOcc n (L l $ Unqual (mkOccName ns $ os ++ "_" ++ filter (/= '.') (moduleNameString mn)))
  | otherwise = FieldOcc n (L l $ Unqual (mkOccName ns $ os ++ "_" ++ filter (/= '.') (moduleNameString modName)))
  where ns = occNameSpace on
        os = occNameString on
qualField _ c = c

mkUnqual :: T.Text -> T.Text
mkUnqual =
  (*=~/ [ed|(([A-Z][A-Za-z0-9_]*)\.)+${e}([A-Za-z][A-Za-z0-9_']*)///${e}|])

-- | make all imports qualified, hiding nothing, no aliasing and no safe imports
mkImportsQual :: [LImportDecl GhcRn] -> [LImportDecl GhcRn]
mkImportsQual = map (\(L l i) ->
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
mkNameQual :: [ModuleName] -> ModuleName -> OccName -> NameSpace -> Name -> Name
mkNameQual importNames mn on ns n
  | mn `elem` importNames           = tidyNameOcc n $ mkOccName ns $ moduleNameString mn ++ "." ++ oshow on
  | (==1) . length $ fittingImports = tidyNameOcc n $ mkOccName ns $ moduleNameString (head fittingImports) ++ "." ++ oshow on
  | otherwise                       = tidyNameOcc n $ mkOccName ns $ moduleNameString mn ++ "." ++ oshow on
  where fittingImports = filter ((== getLeafModule (moduleNameString mn)) . getLeafModule . moduleNameString) $ importNames

getLeafModule = last . words . map (\c -> if c == '.' then ' ' else c)

-- | too naive check if something is an operator
-- TODO: use syntax from Haskell2010 report
isOperator :: String -> Bool
isOperator = not . any isAlphaNum

-- | rename an operator to avoid ambiguities
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
  return $ s1 ++ s2
  where
    operatorSymbols = "!#$%&*+<>?@^~"


-- ***************************************************************************
-- CABAL FILE UTILITIES
-- ***************************************************************************
-- getFiles :: [Path Abs Dir] -> [DPF.Field a] -> IO [Path Abs File]
-- getFiles srcDirs =
--     filterM (doesFileExist . fromAbsFile)
--   . fromJust
--   . fmap (concatMap (\n -> map (\d -> d </> n) srcDirs) . concat)
--   . traverse (file2Paths modName2FileName ["exposed-modules", "other-modules"])
--   . keepFields ["library"]
-- 
-- 
-- file2Paths :: (FilePath -> Maybe (Path Rel a))
--                 -> [BS.ByteString]
--                 -> DPF.Field b
--                 -> Maybe [Path Rel a]
-- file2Paths g names (DPF.Section _ _ f) =
--   concat <$> traverse (file2Paths g names) f
-- file2Paths g names (DPF.Field n f)
--   | BS.map W8.toLower (DPF.getName n) `elem` names
--   = sequence . filter isJust . map g $ map (B8.unpack . fieldLineBS) f
--   | otherwise = return []
-- 
-- getDirs :: [BS.ByteString]
--         -> Path Abs Dir
--         -> [DPF.Field a]
--         -> Maybe [Path Abs Dir]
-- getDirs dirNames rootPath sections = do
--   relPaths <- concat <$> traverse (file2Paths parseRelDir dirNames) sections
-- 
--   relPathsFixed <-
--     sequence
--     . filter isJust
--     . concatMap (map parseRelDir
--                  . words
--                  . map (\c -> if c == ',' then ' ' else c)
--                  . fromRelDir)
--     $ relPaths
-- 
-- 
--   fmap (map (rootPath </>)) .
--     traverse (parseRelDir . filter (/= ',') . fromRelDir) $ relPathsFixed
-- 
-- modName2FileName :: String -> Maybe (Path Rel File)
-- modName2FileName f
--    | f == "Main.hs" = parseRelFile f
--    | ".hs" `isSubsequenceOf` f =
--      let fileName = take (length f - 3) f
--      in parseRelFile $ map (\case '.' -> '/'; c -> c) fileName ++ ".hs"
--    | otherwise = parseRelFile $ map (\case '.' -> '/'; c -> c) f ++ ".hs"
-- 
-- keepFields :: [String] -> [DPF.Field a] -> [DPF.Field a]
-- keepFields names =
--   filter ((`elem` map B8.pack names) . BS.map W8.toLower . DPF.getName . DPF.fieldName)
-- 
-- 
-- 
-- -- get data from cabal file
-- hsAllInOne :: Bool -> Path Abs File -> IO ()
-- hsAllInOne isExecutable filePath = do
--   banner "Running hsAllInOne"
-- 
--   let rootPath = parent filePath
--   sourcePath  <- (rootPath </>) <$> parseRelFile "AllInOne.hs"
--   sections    <- fromRight [] . DPP.readFields <$> BS.readFile (fromAbsFile filePath)
--   let includeDirs = fromMaybe [] $ getDirs ["include-dirs"] rootPath sections
-- 
-- -- search for cabal file
-- getSrcDirs     = fromMaybe [] $ getDirs ["hs-source-dirs"] rootPath sections

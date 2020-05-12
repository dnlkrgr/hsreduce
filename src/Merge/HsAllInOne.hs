module Merge.HsAllInOne (plugin) where

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
import Util.Util
import Text.RE.TDFA.Text
import Parser.Parser
import Data.Generics.Uniplate.Data
import qualified Data.Word8 as W8
import GhcPlugins (Plugin(..), defaultPlugin, renamedResultAction)

hsAllInOne = error "hsAllInOne: currently not working"

plugin :: Plugin
plugin = defaultPlugin {
    renamedResultAction = \_ t r -> do
        liftIO $ writeFile "Arst.hs" (oshow $ renameModule r)
        return (t, r)
  }


-- fastCleanUp :: Path Abs File -> IO ()
-- fastCleanUp sourcePath = TIO.readFile (fromAbsFile sourcePath) >>= cleanUp sourcePath

-- cleanUp :: Path Abs File -> T.Text -> IO ()
-- cleanUp sourcePath fileContent =
--   getGhcOutput sourcePath Other >>= \case
--     Nothing -> return ()
--     Just [] -> return ()
--     Just mySpans -> do
--       let newFileContent = foldr replaceWithGhcOutput fileContent . map (fmap span2Locs) $ mySpans
--       TIO.writeFile (fromAbsFile sourcePath) newFileContent
--       cleanUp sourcePath newFileContent

renameModule :: HsGroup GhcRn
             -> HsGroup GhcRn
renameModule r = do
      transformBi unqualBinds
    . transformBi qualAmbiguousField
    . transformBi qualField
    . transformBi name2UniqueName
    $ r

name2UniqueName :: Name -> Name
name2UniqueName n
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
  | isOperator . showGhc $ getRdrName n,
    not $ "$main$" `isPrefixOf` nameStableString n = n

  -- our operator
  | Just m <- nameModule_maybe n
  , isOperator . showGhc $ getRdrName n
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
  = mkNameQual (moduleName m) on ns n

  | otherwise = n
  where
    on = occName n
    os = occNameString on
    ns = occNameSpace on
    blacklist = ["$main$", "$ghc-prim$", "$base$"]

-- ***************************************************************************
-- MERGING MODULES UTILITIES
-- ***************************************************************************

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
        prefix         = T.take (colStart-1) lineContent
        suffix         = T.drop (colEnd-1) lineContent
        isInfix        = (==2) . T.length . T.filter (=='`') . T.drop (colStart-1) . T.take (colEnd-1) $ lineContent
        newLineContent =
          if isInfix
          then prefix <> "`" <> newName <> "`" <> suffix
          else prefix <> newName <> suffix

span2Locs :: RealSrcSpan -> (RealSrcLoc, RealSrcLoc)
span2Locs s = (realSrcSpanStart s, realSrcSpanEnd s)

unqualOurNames :: [ModuleName] -> Name -> Name
unqualOurNames ours n
  | Just m <- nameModule_maybe n
  , moduleName m `elem` ours
  = unqualName n

  | otherwise = n

unqualBinds :: HsBindLR GhcRn GhcRn -> HsBindLR GhcRn GhcRn
unqualBinds fb@(FunBind _ (L l n) fm  _ _)
  | isQual (T.pack $ showGhc n) = newFB
  | otherwise = fb
  where newFM = transformBi unqualMatchCtxt fm
        newFB = fb { fun_id = L l $ unqualName n, fun_matches = newFM }
unqualBinds hb = hb

unqualMatchCtxt :: HsMatchContext Name -> HsMatchContext Name
unqualMatchCtxt = fmap unqualName

unqualName :: Name -> Name
unqualName n = tidyNameOcc n $ mkOccName ns $ T.unpack $ mkUnqual (T.pack $ showGhc n)
   where
     on = occName n
     ns = occNameSpace on

qualAmbiguousField :: AmbiguousFieldOcc GhcRn -> AmbiguousFieldOcc GhcRn
qualAmbiguousField (Unambiguous n (L l (Unqual on)))
  | Just mn <- moduleName <$> nameModule_maybe n
  , "$main$" `isPrefixOf `nameStableString n =
  Unambiguous n $ L l $ Unqual $ mkOccName ns $ os ++ "_" ++ filter (/= '.') (moduleNameString mn)
  where
    os = occNameString on
    ns = occNameSpace on
qualAmbiguousField a = a -- in other cases there is nothing we can do

qualField :: FieldOcc GhcRn -> FieldOcc GhcRn
qualField c@(FieldOcc n (L l (Unqual on)))
  | Just mn <- moduleName <$> nameModule_maybe n
  , "$main$" `isPrefixOf `nameStableString n =
      FieldOcc n (L l $ Unqual (mkOccName ns $ os ++ "_" ++ filter (/= '.') (moduleNameString mn)))
  | otherwise = traceShow ("HsAllInOne:qualField :-(" :: String) c
  where ns = occNameSpace on
        os = occNameString on
qualField c = c


mkUnqual :: T.Text -> T.Text
mkUnqual =
  (*=~/ [ed|(([A-Z][A-Za-z0-9_]*)\.)+${e}([A-Za-z][A-Za-z0-9_']*)///${e}|])

mkImportsQual :: [LImportDecl GhcRn] -> [LImportDecl GhcRn]
mkImportsQual = map (\(L l i) ->
                       L l
                       $ i { ideclQualified = True
                           , ideclHiding    = Nothing
                           , ideclAs        = Nothing
                           , ideclSafe      = False})

removeImports :: [ModuleName] -> [LImportDecl GhcRn] -> [LImportDecl GhcRn]
removeImports ours = filter go
  where
    go (L _ i) = let modName = unLoc . ideclName $ i
                 in modName `notElem` ours && ("Prelude" /= (moduleNameString modName))


mkNameQual :: ModuleName -> OccName -> NameSpace -> Name -> Name
mkNameQual mn on ns n = tidyNameOcc n $ mkOccName ns $ moduleNameString mn ++ "." ++ showGhc on

isOperator :: String -> Bool
isOperator = not . any isAlphaNum

renameOperator :: String -> String
renameOperator = evalRand randomOpString . mkStdGen . hash

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

fieldLineBS :: DPF.FieldLine a -> B8.ByteString
fieldLineBS (DPF.FieldLine _ bs) = bs

cabal2Main :: [Path Abs Dir] -> [DPF.Field a] -> IO (Path Abs File)
cabal2Main srcDirs =
  fmap head
  . filterM (doesFileExist . fromAbsFile)
  . fromJust
  . fmap (concatMap (\n -> map (\d -> d </> n) srcDirs) . concat)
  . traverse (file2Paths modName2FileName ["main-is"])
  . keepFields ["executable", "library"]

getFiles :: [Path Abs Dir] -> [DPF.Field a] -> IO [Path Abs File]
getFiles srcDirs =
    filterM (doesFileExist . fromAbsFile)
  . fromJust
  . fmap (concatMap (\n -> map (\d -> d </> n) srcDirs) . concat)
  . traverse (file2Paths modName2FileName ["exposed-modules", "other-modules"])
  . keepFields ["library"]


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

mkNewCabal :: Path Abs File -> IO ()
mkNewCabal f =
  (*=~/ [ed|main-is: *([A-Z][A-Za-z]+.hs)///main-is: AllInOne.hs|])
  <$> TIO.readFile f'
  >>= TIO.writeFile f'
  where f' = fromAbsFile f

keepFields :: [String] -> [DPF.Field a] -> [DPF.Field a]
keepFields names =
  filter ((`elem` map B8.pack names) . BS.map W8.toLower . DPF.getName . DPF.fieldName)



-- -- get data from cabal file
-- hsAllInOne :: Bool -> Path Abs File -> IO ()
-- hsAllInOne isExecutable filePath = do
--   banner "Running hsAllInOne"
-- 
--   let rootPath = parent filePath
--   sourcePath  <- (rootPath </>) <$> parseRelFile "AllInOne.hs"
--   sections    <- fromRight [] . DPP.readFields <$> BS.readFile (fromAbsFile filePath)
--   let includeDirs = fromMaybe [] $ getDirs ["include-dirs"] rootPath sections
--       srcDirs     = fromMaybe [] $ getDirs ["hs-source-dirs"] rootPath sections
-- 
-- 
--   files <- if isExecutable
--     then do
--       mainFile    <- cabal2Main srcDirs sections
-- 
-- 
--       mod2DepNames includeDirs srcDirs mainFile
--     else do
--       getFiles srcDirs sections
-- 
--   -- TODO: concatenate all modules in exposed-modules and other-modules to the dirs in hs-source-dirs
-- 
--   fileContent <-
--     T.unlines
--     . T.lines
--     <$> mergeModules includeDirs srcDirs files
--   TIO.writeFile (fromAbsFile sourcePath) fileContent
-- 
-- 
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



--mergeModules :: [Path Abs Dir] -> [Path Abs Dir] -> [Path Abs File] -> IO T.Text
--mergeModules includeDirs srcDirs filenames = do
--  modules <- mapM (undefined includeDirs srcDirs) filenames
--
--  banner "Finished merging"
--
--  let ours =
--          concatMap
--          ( \(_, ast, _, _) ->
--              maybe [mkModuleName "Main"] ((: []) . unLoc)
--                . hsmodName
--                . unLoc
--                $ ast
--          )
--          modules
--
--      prags = T.unlines
--              . nub
--              . concatMap (\(p, _, _, _) -> map (T.pack . show) $ p)
--              $ modules
--
--      modName = "module Main (main) where"
--
--      imps  = T.unlines
--              . nub
--              . map (\(_, _, i, _) -> T.unlines
--                      . map (T.pack . showGhc)
--                      . mkImportsQual
--                      . removeImports ours $ i)
--              $ modules
--
--      decls = T.pack
--              . showGhc
--              . transformBi (unqualOurNames ours)
--              . foldr (appendGroups . (\(_, _, _, d) -> d)) emptyRnGroup
--              $ modules
--
--
--  return $ T.unlines [prags, modName, imps , decls]


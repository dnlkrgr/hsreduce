module Merge.HsAllInOne (hsAllInOne) where


import Data.Either
import Debug.Trace
import "ghc" SrcLoc
import "ghc" Name
import "ghc" RdrName hiding (isQual, mkUnqual, mkQual)
import Control.Monad.Extra
import Control.Monad.Random
import Data.Char
import Data.Hashable
import Data.List
import GHC
import System.Directory
import System.FilePath.Posix
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Distribution.Parsec.Parser as DPP (readFields)
import qualified Distribution.Parsec.Field as DPF
import Util.Util
import Text.RE.TDFA.Text
import Util.Types
import Parser.Parser
import Data.Generics.Uniplate.Data


-- TODO:
-- [ ] remove all the ugly hacks / make decisions
  -- * qualify imports?
  -- * qualify names?
  -- * rename data constr?
    -- * why is it printed with "`"?
-- [ ] does this work for alternative Preludes?
-- [ ] get the file paths for the Haskell files from the main Haskell file
-- [ ] make `isOperator` a stronger predicate


-- get data from cabal file
hsAllInOne :: FilePath -> IO ()
hsAllInOne filePath = do
  banner "Running hsAllInOne"

  let rootPath = fst $ splitFileName filePath
      sourcePath = rootPath <> "AllInOne.hs"
  sections <- fromRight [] . DPP.readFields <$> BS.readFile filePath
  let includeDirs = getIncludeDirs rootPath sections
      srcDirs = getSrcDirs rootPath sections
  files       <- trace' <$> cabal2FilePaths srcDirs sections
  fileContent <- mergeModules includeDirs srcDirs files
  TIO.writeFile sourcePath fileContent

  banner "Cleaning up"

  go sourcePath fileContent

  mkNewCabal filePath

  where go sourcePath fileContent =
          (trace' <$> getGhcOutput sourcePath Other) >>= \case
            Nothing -> return ()
            Just [] -> return ()
            Just mySpans -> do
              let newFileContent = foldr mkQual fileContent . map (fmap span2Locs) $ mySpans
              TIO.writeFile sourcePath newFileContent
              go sourcePath newFileContent


mergeModules :: [FilePath] -> [FilePath] -> [FilePath] -> IO T.Text
mergeModules includeDirs srcDirs filenames = do
  modules <- mapM (renameModule includeDirs srcDirs) filenames

  banner "Finished merging"

  let ours =
          concatMap
          ( \(_, ast, _, _) ->
              maybe [mkModuleName "Main"] ((: []) . unLoc)
                . hsmodName
                . unLoc
                $ ast
          )
          modules

      imps  = T.unlines
              . nub
              . map (\(_, _, i, _) -> T.unlines
                      . map (T.pack . showGhc)
                      . mkImportsQual
                      . removeImports ours $ i)
              $ modules

      decls = T.pack
              . showGhc
              . transformBi (unqualOurNames ours)
              . foldr (appendGroups . (\(_, _, _, d) -> d)) emptyRnGroup
              $ modules

      prags = T.unlines
              . nub
              . concatMap (\(p, _, _, _) -> map (T.pack . show) $ p)
              $ modules

  return $ T.unlines [prags, imps , decls]


renameModule :: [FilePath]
             -> [FilePath]
             -> FilePath
             -> IO ([Pragma], ParsedSource, [LImportDecl GhcRn], HsGroup GhcRn)
renameModule includeDirs srcDirs fileName = do
  banner $ "Renaming: " ++ fileName

  RState prags ast (Just r) _  <- parse includeDirs srcDirs fileName

  banner "Finished parsing"

  return
    . (\(d, i, _, _) -> (prags, ast, tail i, d))
    . transformBi unqualBinds
    . transformBi name2UniqueName
    $ r

-- TODO: remove `isDataConName n` check, this is just now to make the code work,
-- this probably doesn't work for the general case
name2UniqueName :: Name -> Name
name2UniqueName n
  | ":<|" `isSubsequenceOf` nameStableString n = traceShow ("isOperator? " ++ show (isOperator $ showGhc $ getRdrName n)) $ traceShow ("isDataCon? " ++ show (isDataConName n)) $ traceShow ("OH MY GOD: " ++ nameStableString n) n
  | "main" == showGhc (getRdrName n) = n
  | isSystemName    n                = traceShow ("name2UniqueName - SYSTEM NAME: " ++ nameStableString n) n
  | isWiredInName   n                = n
  | isBuiltInSyntax n                = n
  | isDataConName   n                = n
  | isOperator . showGhc $ getRdrName n,
    not $ "$main" `isPrefixOf` nameStableString n = n -- not our operator, do not touch

  | isOperator . showGhc $ getRdrName n,                          -- our operator
    "$main" `isPrefixOf` nameStableString n =
                                              tidyNameOcc n
                                            . mkOccName ns
                                            . renameOperator
                                            $ os
  | Just m <- nameModule_maybe n,                                 -- our function / variable
    "$main" `isPrefixOf` nameStableString n = tidyNameOcc n
                                            . mkOccName ns
                                            $ os
                                            ++ "_"
                                            ++ filter (/= '.')
                                                      (moduleNameString (moduleName m))
  -- something external
  | all (not . (`isPrefixOf` nameStableString n)) blacklist, Just m <- nameModule_maybe n
  = mkNameQual (moduleName m) on ns n

  | otherwise = n
  where
    on = occName n
    os = occNameString on
    ns = occNameSpace on
    blacklist = ["$main$", "$ghc-prim$", "$base$"]

mkQual :: (T.Text, (RealSrcLoc, RealSrcLoc)) -> T.Text -> T.Text
mkQual (newName, (startLoc, endLoc)) fileContent =
  T.unlines $ prevLines ++ [newLineContent] ++ succLines
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
          then prefix `T.append` "`" `T.append`  newName `T.append` "`" `T.append` suffix
          else prefix `T.append` newName `T.append` suffix 

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
  | gotQual (T.pack $ showGhc n) /= "" = newFB
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
    go (L _ i) = (unLoc . ideclName $ i) `notElem` ours



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
  return $ '<' : s1 ++ s2 ++ ">"
  where
    operatorSymbols = "<@#%^&*/>"


fieldLineBS :: DPF.FieldLine a -> B8.ByteString
fieldLineBS (DPF.FieldLine _ bs) = bs

cabal2FilePaths :: [FilePath] -> [DPF.Field a] -> IO [FilePath]
cabal2FilePaths srcDirs =
  filterM doesFileExist
  . nub
  . concatMap ((\n -> map (\d -> d </> n) srcDirs) . modName2FileName)
  . concatMap (field2FilePaths ["main-is", "other-modules", "exposed-modules"])
  . keepFields ["executable", "library"]


field2FilePaths :: [BS.ByteString] -> DPF.Field a -> [FilePath]
field2FilePaths names (DPF.Section _ _ f) = concatMap (field2FilePaths names) f
field2FilePaths names (DPF.Field n f)
  | DPF.getName n `elem` names = map (B8.unpack . fieldLineBS) f
  | otherwise = []

modName2FileName :: String -> String
modName2FileName f
  | f == "Main.hs" = f
  | otherwise = map (\case '.' -> '/'; c -> c) f ++ ".hs"

getIncludeDirs :: FilePath -> [DPF.Field a] -> [FilePath]
getIncludeDirs rootPath =
  nub
  . map (rootPath <>)
  . concatMap (field2FilePaths ["include-dirs"])

getSrcDirs :: FilePath -> [DPF.Field a] -> [FilePath]
getSrcDirs rootPath =
  nub
  . map (rootPath <>)
  . concatMap (field2FilePaths ["hs-source-dirs"])

mkNewCabal :: FilePath -> IO ()
mkNewCabal f = (*=~/ [ed|main-is: *([A-Z][A-Za-z]+.hs)///main-is: AllInOne.hs|])
             <$> TIO.readFile f
             >>= TIO.writeFile f

keepFields :: [String] -> [DPF.Field a] -> [DPF.Field a]
keepFields names = filter ((`elem` map B8.pack names) . DPF.getName . DPF.fieldName)

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
  files       <- trace' <$> cabal2FilePaths filePath
  includeDirs <- trace' <$> cabal2IncludeDirs filePath

  fileContent <- mergeModules files includeDirs
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


mergeModules :: [FilePath] -> [FilePath] -> IO T.Text
mergeModules filenames includeDirs = do
  modules <- mapM (renameModule includeDirs) filenames

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
              . foldr (appendGroups . (\(_, _, _, d) -> d)) emptyRnGroup
              $ modules

      prags = T.unlines
              . map (\(p, _, _, _) -> T.unlines . map showPragma $ p)
              $ modules

  return $ T.unlines [prags, imps , decls]


renameModule :: [FilePath]
             -> FilePath
             -> IO ([Pragma], ParsedSource, [LImportDecl GhcRn], HsGroup GhcRn)
renameModule includeDirs fileName = do
  banner $ "Renaming: " ++ fileName

  RState prags ast (Just r) _  <- parse fileName includeDirs

  banner "Finished parsing"

  return
    . (\(d, i, _, _) -> (prags, ast, tail i, d))
    . transformBi unqualBinds
    . transformBi name2UniqueName
    $ r

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
mkImportsQual = map (\(L l i) -> L l $ i {ideclQualified = True, ideclHiding = Nothing, ideclAs = Nothing})

removeImports :: [ModuleName] -> [LImportDecl GhcRn] -> [LImportDecl GhcRn]
removeImports ours = filter go
  where
    go (L _ i) = (unLoc . ideclName $ i) `notElem` ours


-- TODO: remove `isDataConName n` check, this is just now to make the code work,
-- this probably doesn't work for the general case
name2UniqueName :: Name -> Name
name2UniqueName n
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

-- TODO: alle src dirs an alle paths consen und dann nach Existenz filtern
cabal2FilePaths :: FilePath -> IO [FilePath]
cabal2FilePaths f = do
  sections <- keepFields ["executable", "library"] . fromRight [] . DPP.readFields <$> BS.readFile f

  let rootPath  = trace' . fst $ splitFileName f

      srcDirs   = map fieldLine2String
                . concatMap field2FieldLines
                . concatMap (keepFields ["hs-source-dirs"] . getFields)
                $ sections

      filePaths = nub
                . concatMap ((\n -> map (\d -> rootPath <> d </> n) srcDirs) . modName2FileName . fieldLine2String)
                . concatMap field2FieldLines
                . concatMap (keepFields ["main-is", "other-modules"] . getFields)
                $ sections

  filterM doesFileExist filePaths

fieldLine2String :: DPF.FieldLine a -> String
fieldLine2String = B8.unpack . fieldLineBS

modName2FileName :: String -> String
modName2FileName f
  | f == "Main.hs" = f
  | otherwise = map (\case '.' -> '/'; c -> c) f ++ ".hs"

cabal2IncludeDirs :: FilePath -> IO [FilePath]
cabal2IncludeDirs f =
  nub
  . map ((rootPath <>) . fieldLine2String)
  . concatMap field2FieldLines
  . concatMap (keepFields ["include-dirs"] . getFields)
  . keepFields ["executable", "library"] . fromRight []
  . DPP.readFields
  <$> BS.readFile f

  where rootPath = (fst $ splitFileName f)


mkNewCabal :: FilePath -> IO ()
mkNewCabal f = (*=~/ [ed|main-is: *([A-Z][A-Za-z]+.hs)///main-is: AllInOne.hs|])
             <$> TIO.readFile f
             >>= TIO.writeFile f

field2FieldLines :: DPF.Field a -> [DPF.FieldLine a]
field2FieldLines (DPF.Field _ fl) = fl
field2FieldLines _ = []

keepFields :: [String] -> [DPF.Field a] -> [DPF.Field a]
keepFields names = filter ((`elem` map B8.pack names) . DPF.getName . DPF.fieldName)

getFields :: DPF.Field a -> [DPF.Field a]
getFields (DPF.Section _ _ f) = f
getFields _ = []

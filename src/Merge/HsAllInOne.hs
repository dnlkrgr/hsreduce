module Merge.HsAllInOne (hsAllInOne, recListDirectory) where


import "ghc" SrcLoc
import Debug.Trace
import "ghc" DynFlags
import "ghc" Name
import "ghc" Outputable hiding ((<>))
import "ghc" RdrName hiding (isQual, mkUnqual, mkQual)
import "syb" Data.Generics
import Control.Monad.Extra
import Control.Monad.Random
import Data.Char
import Data.Hashable
import Data.List
import GHC
import GHC.Paths
import System.Directory
import System.FilePath.Posix
import System.Posix.Files
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Distribution.Parsec.Parser as DF
import qualified Distribution.Parsec.Field as DFF
import Data.Maybe
import Util.Util
import Text.RE.TDFA.String
import Util.Types




-- TODO: 
-- [ ] remove all the ugly hacks / make decisions
  -- * qualify imports?
  -- * qualify names?
  -- * rename data constr?
    -- * why is it printed with "`"?
-- [ ] pragmas: make list of commonly used ones
-- [ ] does this work for alternative Preludes?
-- [ ] get the file paths for the Haskell files from the main Haskell file
-- [ ] make `isOperator` a stronger predicate


-- get data from cabal file
hsAllInOne :: FilePath -> IO ()
hsAllInOne filePath = do
  let (foldrName, _) = splitFileName filePath
      sourcePath     = "AllInOne.hs"
  files       <- nub <$> cabal2FilePaths filePath
  includeDirs <- nub <$> cabal2IncludeDirs filePath

  withCurrentDirectory foldrName $ do
    (prags, imps, decls) <- mergeModules files includeDirs
    let fileContent = unlines [prags, imps, showGhc decls]
    TIO.writeFile sourcePath (T.pack fileContent)

    go sourcePath fileContent
       
  where go sourcePath fileContent =
          getGhcOutput sourcePath Other >>= \case
            Nothing -> return ()
            Just [] -> return ()
            Just locs -> do
              let newFileContent = foldr mkQual fileContent $ map (fmap span2Locs)  $ traceShow (map showGhc locs) locs
              TIO.writeFile sourcePath (T.pack newFileContent)
              go sourcePath newFileContent

mkQual :: (String, (RealSrcLoc, RealSrcLoc)) -> String -> String
mkQual (newName, (startLoc, endLoc)) fileContent = 
  unlines $ prevLines ++ [newLineContent] ++ succLines
  where 
        contentLines   = lines fileContent
        lineStart      = srcLocLine startLoc
        colStart       = srcLocCol  startLoc
        colEnd         = srcLocCol  endLoc
        currentIndex   = lineStart-1
        prevLines      = take currentIndex contentLines 
        succLines      = drop lineStart contentLines
        lineContent    = contentLines !! currentIndex
        prefix         = take (colStart-1) lineContent
        suffix         = drop (colEnd-1) lineContent
        isInfix        = (==2) . length . filter (=='`') . drop (colStart-1) . take (colEnd-1) $ lineContent
        newLineContent = if isInfix then prefix ++ "`" ++  newName ++ "`" ++ suffix else prefix ++ newName ++ suffix 

span2Locs :: RealSrcSpan -> (RealSrcLoc, RealSrcLoc)
span2Locs s = (realSrcSpanStart s, realSrcSpanEnd s)


mergeModules :: [FilePath] -> [FilePath] -> IO (String, String, HsGroup GhcRn)
mergeModules filenames includeDirs = do
  modules <- mapM (renameModule includeDirs) filenames
  let ours =
        concatMap
          ( \(p, _, _) ->
              maybe [mkModuleName "Main"] ((: []) . unLoc)
                . hsmodName
                . unLoc
                $ parsedSource p
          )
          modules
      imps  = unlines . nub . map (\(_, i, _) -> unlines . map showGhc . mkImportsQual . removeImports ours $ i) $ modules
      decls = foldr (appendGroups . (\ (_, _, d) -> d)) emptyRnGroup $ modules
      enabledExtensions = 
        unlines . map showPragma . getPragmas $ showGhc decls

  return (enabledExtensions, imps , decls)


-- TODO: collect and return all pragmas
renameModule :: [FilePath] -> FilePath -> IO ( ParsedModule, [LImportDecl GhcRn], HsGroup GhcRn)
renameModule includeDirs fileName = do
  banner $ "Renaming: " ++ fileName
  let modName = map (\case '/' -> '.' ; c -> c) $ dropExtension fileName
  
  runGhc (Just libdir) $ do
    dflags          <- getSessionDynFlags
    (dflags', _, _) <- parseDynamicFlags dflags []
    _ <- setSessionDynFlags dflags' { includePaths = IncludeSpecs [] includeDirs }

    target <- guessTarget fileName Nothing
    setTargets [target]
    _ <- load LoadAllTargets
    modSum <- getModSummary $ mkModuleName modName

    p <- parseModule modSum
    t <- typecheckModule p

    return
      . (\(d, i, _, _) -> (p, tail i, d))
      . fromJust
      . everywhere (mkT unqualBinds . mkT (name2UniqueName (parsedSource p)))
      . renamedSource
      $ t

unqualBinds :: HsBindLR GhcRn GhcRn -> HsBindLR GhcRn GhcRn
unqualBinds fb@(FunBind _ (L l n) fm  _ _) 
  | gotQual (showGhc n) /= "" = newFB
  | otherwise = fb
  where newFM = everywhere (mkT unqualMatchCtxt) fm
        newFB = fb { fun_id = L l $ unqualName n, fun_matches = newFM }
unqualBinds hb = hb
  
unqualMatchCtxt :: HsMatchContext Name -> HsMatchContext Name
unqualMatchCtxt = fmap unqualName

unqualName :: Name -> Name
unqualName n = tidyNameOcc n $ mkOccName ns $ mkUnqual (showGhc n)
   where
     on = occName n
     ns = occNameSpace on

mkUnqual :: String -> String
mkUnqual = 
  (*=~/ [ed|(([A-Z][A-Za-z0-9_]*)\.)+${e}([A-Za-z][A-Za-z0-9_']*)///${e}|]) 

-- TODO: wird das jemals gebraucht? 
-- ja: falls sich die Importe in die Quere kommen
mkImportsQual :: [LImportDecl GhcRn] -> [LImportDecl GhcRn]
mkImportsQual = map (\(L l i) -> L l $ i {ideclQualified = True, ideclHiding = Nothing, ideclAs = Nothing})

removeImports :: [ModuleName] -> [LImportDecl GhcRn] -> [LImportDecl GhcRn]
removeImports ours = filter go
  where
    go (L _ i) = (unLoc . ideclName $ i) `notElem` ours


-- TODO: remove `isDataConName n` check, this is just now to make the code work, this probably doesn't work for the general case
name2UniqueName :: ParsedSource -> Name -> Name
name2UniqueName _ n
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

showGhc :: (Outputable a) => a -> String
showGhc = showPpr unsafeGlobalDynFlags

fieldLineBS :: DFF.FieldLine a -> B8.ByteString
fieldLineBS (DFF.FieldLine _ bs) = bs

-- TODO: die unteren beiden Funktionen zusammenfassen
cabal2FilePaths :: FilePath -> IO [FilePath]
cabal2FilePaths =
  fmap
    (
      map ((\f -> 
              if f == "Main.hs" 
                then f 
                else map (\case '.' -> '/'; c -> c) f ++ ".hs") 
          . B8.unpack 
          . fieldLineBS)
    . concatMap  field2FieldLines 
    . concatMap  (keepFields ["main-is", "other-modules"] . getFields) 
    . keepFields ["executable", "library"] 
    . concat 
    . either (const []) (:[]) . DF.readFields 
    )
  . BS.readFile 
  

cabal2IncludeDirs :: FilePath -> IO [FilePath]
cabal2IncludeDirs = 
  fmap
    (
      map ( B8.unpack . fieldLineBS)
    . concatMap  field2FieldLines 
    . concatMap (keepFields ["include-dirs"] . getFields)
    . keepFields ["executable", "library"] 
    . concat 
    . either (const []) (:[]) . DF.readFields 
    )
  . BS.readFile 

field2FieldLines :: DFF.Field a -> [DFF.FieldLine a]
field2FieldLines (DFF.Field _ fl) = fl
field2FieldLines _ = []

keepFields :: [String] -> [DFF.Field a] -> [DFF.Field a]
keepFields names = filter ((`elem` map B8.pack names) . DFF.getName . DFF.fieldName)

getFields :: DF.Field a -> [DF.Field a]
getFields (DF.Section _ _ f) = f
getFields _ = []

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

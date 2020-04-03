module Merge.HsAllInOne (hsAllInOne, recListDirectory) where


import "ghc" DynFlags
import "ghc" Name
import "ghc" Outputable hiding ((<>))
import "ghc" RdrName hiding (isQual)
import "syb" Data.Generics
import Control.Monad.Extra
import Control.Monad.Random
import Data.Char
import Data.Hashable
import Data.List
import Debug.Trace
import GHC
import GHC.Paths
import System.Directory
import System.FilePath.Posix
import System.Posix.Files
import Util.Util
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Distribution.Fields as DF
import qualified Distribution.Fields.Field as DFF
import Data.Maybe



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
  files <- nub <$> cabal2FilePaths filePath
  includeDirs <- nub <$> cabal2IncludeDirs filePath

  fileContent <- liftIO (withCurrentDirectory foldrName $ mergeModules files includeDirs)

  liftIO $ TIO.writeFile (foldrName </> "AllInOne.hs") (T.pack fileContent)


mergeModules :: [FilePath] -> [FilePath] -> IO String
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
      decls = showGhc . foldr (appendGroups . (\ (_, _, d) -> d)) emptyRnGroup $ modules
      enabledExtensions = 
        unlines $ map (\s -> "{-# language " ++ s ++ " #-}") $ filter (`isSubsequenceOf` decls) ["CPP", "BangPatterns", "MagicHash", "StandaloneDeriving", "TypeFamilies", "PatternGuards"]

  return $ unlines [enabledExtensions, imps , decls]



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
      . everywhere (mkT $ name2UniqueName (parsedSource p))
      . renamedSource
      $ t

-- TODO: wird das jemals gebraucht? 
-- ja: falls sich die Importe in die Quere kommen
mkImportsQual :: [LImportDecl GhcRn] -> [LImportDecl GhcRn]
mkImportsQual = map (\(L l i) -> L l $ i {ideclQualified = True, ideclHiding = Nothing, ideclAs = Nothing})

removeImports :: [ModuleName] -> [LImportDecl GhcRn] -> [LImportDecl GhcRn]
removeImports ours = filter go
  where
    go (L _ i) = (unLoc . ideclName $ i) `notElem` ours


name2UniqueName :: ParsedSource -> Name -> Name
name2UniqueName p n
  | "main" == showGhc (getRdrName n) = n
  | isSystemName    n                = n
  | isWiredInName   n                = n
  | isBuiltInSyntax n                = n
  | isDataConName   n                = n  -- our data constr should actually be overwritten
  | isOperator . showGhc $ getRdrName n,
    not $ "$main" `isPrefixOf` nameStableString n = n -- not our operator, do not touch

  | isOperator . showGhc $ getRdrName n,
    Just m <- nameModule_maybe n,
    "$main" `isPrefixOf` nameStableString n = tidyNameOcc n 
                                            . mkOccName ns 
                                            . renameOperator 
                                            $ os 
                                            ++ moduleNameString (moduleName m)
  | isOperator . showGhc $ getRdrName n,
    "$main" `isPrefixOf` nameStableString n = tidyNameOcc n 
                                            . mkOccName ns 
                                            . renameOperator 
                                            $ os

  | Just m <- nameModule_maybe n,
    "$main" `isPrefixOf` nameStableString n = tidyNameOcc n
                                            . mkOccName ns
                                            $ os 
                                            ++ "_" 
                                            ++ filter (/= '.') 
                                                      (moduleNameString (moduleName m))
  | not $ "$main" `isPrefixOf` nameStableString n 
  , Just m <- nameModule_maybe n
  , isQual p (moduleName m) on 
  = mkNameQual (moduleName m) on ns n

  -- , "$base" `isPrefixOf` nameStableString n 

  -- | Just m <- nameModule_maybe n = mkNameQual p (moduleName m) on ns n
  | Just m <- nameModule_maybe n
  , not $  any (`isPrefixOf` (moduleNameString $ moduleName m)) ["GHC", "System.IO", "Data.Either", "Data.Tuple", "Text.ParserCombinators", "Data.Semigroup.Internal", "Data.Typeable.Internal"]
  = mkNameQual (moduleName m) on ns n

  | otherwise = n 
  where
    on = occName n
    os = occNameString on
    ns = occNameSpace on
    mn = moduleName <$> nameModule_maybe n


isQual :: ParsedSource -> ModuleName -> OccName -> Bool
isQual (L _ p) mn on
  | Just (Qual _ _) <- everything orElse (Nothing `mkQ` findR imps mn on) p =
    True
  | otherwise = False
  where
    imps =
      mapMaybe ((\i -> sequence (unLoc $ ideclName i, unLoc <$> ideclAs i)) . unLoc) $ hsmodImports p
  

-- TODO: wird der zweite Guard gebraucht?
mkNameQual :: ModuleName -> OccName -> NameSpace -> Name -> Name
mkNameQual mn on ns n = tidyNameOcc n $ mkOccName ns $ moduleNameString mn ++ "." ++ showGhc on

-- TODO: wird der erste Guard gebraucht?
findR :: [(ModuleName, ModuleName)] -> ModuleName -> OccName -> RdrName -> Maybe RdrName
findR imps mn on rn@(Qual mn' on')
  | mn == mn' && on == on' = Just rn

  | length [() | (mnFull, mnAs) <- imps, mn' == mnAs, mn == mnFull] == 1 && on == on' = Just rn

  | otherwise = Nothing

findR _ _ _ _ = Nothing


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
          . DFF.fieldLineBS)
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
      map ( B8.unpack . DFF.fieldLineBS)
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

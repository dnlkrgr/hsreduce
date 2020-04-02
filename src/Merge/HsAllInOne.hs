
module Merge.HsAllInOne (hsAllInOne, recListDirectory) where

import "ghc" DynFlags
import "ghc" EnumSet
import "ghc" Name
import "ghc" Outputable hiding ((<>))
import "ghc" RdrName
import "ghc-boot-th" GHC.LanguageExtensions.Type
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
import Util.Util
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import System.Posix.Files
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Distribution.Fields as DF
import qualified Distribution.Fields.Field as DFF


-- get data from cabal file
hsAllInOne :: FilePath -> IO ()
hsAllInOne filePath = do
  let (foldrName, _) = splitFileName filePath
  files <- cabal2FilePaths filePath
  includeDirs <- cabal2IncludeDirs filePath
  fileContent <- liftIO (withCurrentDirectory foldrName $ mergeModules files includeDirs)
  debugPrint $ show $ length fileContent
  liftIO $ TIO.writeFile (foldrName </> "AllInOne.hs") (T.pack fileContent)
  return ()

mergeModules :: [FilePath] -> [FilePath] -> IO String
mergeModules filenames includeDirs = do
  modules <- mapM (renameModule includeDirs) filenames
  let ours =
        concatMap
          ( \(p, _, _, _) ->
              maybe [mkModuleName "Main"] ((: []) . unLoc)
                . hsmodName
                . unLoc
                $ parsedSource p
          )
          modules
      imps  = map (\(_, i, _, _) -> mkImportsQual . removeImports ours $ i) $ modules
      decls = foldr appendGroups emptyRnGroup $ map (\(_, _, d, _) -> d) modules
      _     = nub $ concatMap (\(_, _, _, p) -> toList p) modules
  -- mapM_ (putStrLn . (\p -> "{-# LANGUAGE " ++ p ++ " #-}")  . show) prags
  return $ unlines [unlines $ map showGhc imps , showGhc decls]

-- TODO: collect and return all pragmas
renameModule :: [FilePath] -> FilePath -> IO ( ParsedModule, [LImportDecl GhcRn], HsGroup GhcRn, EnumSet Extension)
renameModule includeDirs fileName = do
  getCurrentDirectory >>= print
  banner $ "Renaming: " ++ fileName
  let modName = map (\c -> if c == '/' then '.' else c) $ dropExtension fileName
  liftIO $ print modName
  putStrLn "\n\n"
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    (dflags', _, _) <- parseDynamicFlags dflags []
    _ <- setSessionDynFlags dflags' { includePaths = IncludeSpecs [] includeDirs }
    target <- guessTarget fileName Nothing
    setTargets [target]
    _ <- load LoadAllTargets
    modSum <- getModSummary $ mkModuleName modName
    p <- parseModule modSum
    t <- typecheckModule p
    return
      . (\(d, i, _, _) -> (p, tail i, d, extensionFlags dflags'))
      . (\(Just x) -> x)
      . everywhere (mkT $ name2UniqueName (parsedSource p))
      . renamedSource
      $ t

-- TODO: get all file paths from mentioned Haskell file
cabal2FilePaths :: FilePath -> IO [FilePath]
cabal2FilePaths =
  fmap
    (
      map ((\f -> 
              if f == "Main.hs" 
                then f 
                else map (\c -> if c == '.' then '/' else c) f ++ ".hs") 
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
field2FieldLines (DFF.Field _ fl)    = fl
field2FieldLines (DFF.Section _ _ _) = []

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


mkImportsQual :: [LImportDecl GhcRn] -> [LImportDecl GhcRn]
mkImportsQual = map (\(L l i) -> L l $ i {ideclQualified = True})

removeImports :: [ModuleName] -> [LImportDecl GhcRn] -> [LImportDecl GhcRn]
removeImports ours = filter go
  where
    go (L _ i) = (unLoc . ideclName $ i) `notElem` ours


-- TODO: does this work for alternative Preludes?
name2UniqueName :: ParsedSource -> Name -> Name
name2UniqueName p n
  | isSystemName n = n
  | isWiredInName n = n
  | isBuiltInSyntax n = n
  | "$ghc" `isPrefixOf` nameStableString n = n
  | "$base" `isPrefixOf` nameStableString n = n
  | "main" == showGhc (getRdrName n) = n
  | isOperator . showGhc $ getRdrName n,
    Just m <- nameModule_maybe n,
    "$main" `isPrefixOf` nameStableString n =
    tidyNameOcc n . mkOccName ns . renameOperator $ os ++ moduleNameString (moduleName m)
  | isOperator . showGhc $ getRdrName n,
    "$main" `isPrefixOf` nameStableString n =
    tidyNameOcc n . mkOccName ns . renameOperator $ os
  | Just m <- nameModule_maybe n,
    "$main" `isPrefixOf` nameStableString n =
    tidyNameOcc n
      . mkOccName ns
      $ os ++ "_" ++ moduleNameString (moduleName m)
  | Just m <- nameModule_maybe n,
    not $ "$main" `isPrefixOf` nameStableString n =
    mkNameQual p (moduleName m) on ns n
  | otherwise = n
  where
    on = occName n
    os = occNameString on
    ns = occNameSpace on

mkNameQual :: ParsedSource -> ModuleName -> OccName -> NameSpace -> Name -> Name
mkNameQual (L _ p) mn on ns n
  | Just (Qual mn' on') <- everything orElse (Nothing `mkQ` findR imps mn on) p =
    tidyNameOcc n $ mkOccName ns $ moduleNameString mn' ++ "." ++ showGhc on'
  | otherwise = tidyNameOcc n $ mkOccName ns $ moduleNameString mn ++ "." ++ showGhc on
  | otherwise = n
  where
    imps =
      concatMap
        ( maybe [] (: [])
            . (\i -> sequence (unLoc $ ideclName i, fmap unLoc $ ideclAs i))
            . unLoc
        )
        $ hsmodImports p

findR :: [(ModuleName, ModuleName)] -> ModuleName -> OccName -> RdrName -> Maybe RdrName
findR imps mn on rn@(Qual mn' on')
  | mn == mn' && on == on' = Just rn
  | length [() | (mnFull, mnAs) <- imps, mn' == mnAs, mn == mnFull] == 1 && on == on' = Just rn
  | otherwise = Nothing
findR _ _ _ _ = Nothing

-- TODO: make this a stronger predicate
-- this is a weak check, can't think of something better for now
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
    operatorSymbols = "<!@#$%^&*/>"

showGhc :: (Outputable a) => a -> String
showGhc = showPpr unsafeGlobalDynFlags

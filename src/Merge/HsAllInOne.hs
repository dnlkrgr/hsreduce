module Merge.HsAllInOne (hsmerge) where

import System.Directory
import qualified Data.Map as M
import SrcLoc
import Name
import RdrName hiding (isQual, mkUnqual, mkQual)
import Control.Monad.Extra
import Control.Monad.Random
import Data.Char
import Data.Hashable
import Data.List
import GHC hiding (GhcMode, extensions)
import Path
import qualified Data.Text as T
import Data.Generics.Uniplate.Data
import GhcPlugins hiding (extensions, (<>), isQual, mkUnqual, qualName, GhcMode, count, (<&&>))
import Data.Maybe
import Data.Either
import Util.Util
import Util.Types
import Parser.Parser
import HIE.Bios
import GHC.Paths
import Data.Void

hsmerge :: FilePath -> IO ()
hsmerge filePath = do
    cradle <- loadCradle . fromJust =<< findCradle filePath
    putStrLn . ("cradle: " <> ) . show $ cradle
    
    getCompilerOptions filePath (cradle :: Cradle Void) >>= \case
        CradleSuccess opts ->
            void . runGhc (Just libdir) $ do
                initSession opts >>= setTargets 
                void $ load LoadAllTargets

                modSums <- mgModSummaries <$> getModuleGraph 
                let ours = map (moduleName . ms_mod) modSums

                missingImportsAndAnnotatedAST <- flip traverse modSums $ \modSum -> do
                    t <- parseModule modSum >>= typecheckModule
                    
                    let 
                        myParsedSource        = unLoc . pm_parsed_source $ tm_parsed_module t
                        importsModuleNames    = map (unLoc . ideclName . unLoc) $ hsmodImports myParsedSource
                        (renamedGroups,_,_,_) = fromMaybe (error "HsAllInOne->hsmerge: no renamed source!") $ tm_renamed_source t
                        proposedNameChanges   = [ (l, name2ProposedChange importsModuleNames ours n) | L l n <- universeBi renamedGroups ] 
                        proposedImportsToAdd  = 
                            map ((\mn -> noLoc $ ImportDecl NoExt NoSourceText (noLoc mn) Nothing False False True False Nothing Nothing) 
                                . fromJust
                                . fst
                                . snd) 
                            $ filter (isJust . fst . snd) proposedNameChanges
                        proposedChanges       = 
                            M.fromList 
                                $  (map (fmap snd) $ proposedNameChanges)
                                <> [ ambiguousField2ProposedChanged importsModuleNames ours a | a <- universeBi renamedGroups ]
                                <> [ field2ProposedChange importsModuleNames ours f | f <- universeBi renamedGroups ]
                        newParsedSource = descendBi unqualSig . descendBi unqualBinds . descendBi (applyChange proposedChanges) $ myParsedSource

                    return (proposedImportsToAdd, newParsedSource)
                    
                d <- liftIO getCurrentDirectory
                let crd = cradleRootDir cradle 
                    root = if crd == "." then d else crd

                extensions <- 
                    liftIO 
                    . fmap (unlines . map show) 
                    . getAllPragmas 
                    . catMaybes 
                    . map (fmap (\f -> if f == filePath then root <> "/" <> f else f) . ml_hs_file . ms_location) 
                    $ modSums

                let 
                    annASTs      = map snd missingImportsAndAnnotatedAST
                    modName      = Just . noLoc $ mkModuleName "AllInOne"
                    importsToAdd = concatMap fst missingImportsAndAnnotatedAST
                    imports      = concatMap (qualImports . removeImports ours . hsmodImports) annASTs <> importsToAdd
                    decls        = concatMap hsmodDecls annASTs
                    mergedMod    = HsModule modName Nothing imports decls Nothing Nothing

                liftIO . writeFile "AllInOne.hs" $ unlines [extensions, oshow mergedMod]

        CradleFail err  -> error $ show err
        _               -> error "Cradle wasn't loaded successfully! Maybe you're missing a hie.yaml file?"

name2ProposedChange :: [ModuleName] -> [ModuleName] -> Name -> (Maybe ModuleName, (RdrName -> RdrName))
name2ProposedChange imports ours n
    -- don't rename things from the Main module to avoid making the test case print uninteresting output
    -- example: data cons renamed from T2 to T2_Main
    --   doesn't match in interestingness test matching on `fromList [T2, T2]`
    | showSDocUnsafe (pprNameUnqualified n) == "main" = (Nothing, id)
  
    -- built-in things
    | isSystemName    n = (Nothing, id)
    | isBuiltInSyntax n = (Nothing, id)
    | isDataConName   n, isOperator $ tail os = (Nothing, id)
    | isWiredInName   n, isOperator . oshow $ getRdrName n = (Nothing, id)
  
    -- not our operator, leave unmodified
    | isOperator . oshow $ getRdrName n, Just mn <- getModuleName n, mn `notElem` ours = (Nothing, id)

    -- our operator
    | Just mn <- getModuleName n, isOperator . oshow $ getRdrName n, mn `elem` ours 
    = (Nothing, (const (Unqual . mkOccName ns . renameOperator $ (os ++ filter (/= '.') (moduleNameString mn)))))
  
    -- our function / variable
    | Just mn <- getModuleName n, mn `elem` ours = (Nothing, (const (Unqual $ mangle mn on)))
    -- something external
    | Just mn <- getModuleName n = case findBestMatchingImport imports mn of
        Left  newMN  -> (Just newMN, const (Qual newMN on))
        Right newMN -> (Nothing, const (Qual newMN on))
    -- | Just mn <- getModuleName n = const . flip Qual on <$> findBestMatchingImport imports mn 

    | otherwise = (Nothing, id)
  where
      on = occName n
      ns = occNameSpace on
      os = occNameString on


findBestMatchingImport :: [ModuleName] -> ModuleName -> Either ModuleName ModuleName
findBestMatchingImport imports mn
    | mn `elem` imports                                                         = Right mn
    -- | mn `elem` imports    = trace'' "right mn" (const (oshow mn <> ", " <> oshow imports)) $ Right mn
    | Just (bestMatch, bestMatchLength) <- safeLast arst, bestMatchLength >= 2  = Right bestMatch 
    | otherwise                                                                 = Left mn
  where 
        components      = modname2components . T.pack $ moduleNameString mn
        importsSorted   = map (\i -> ((,) i) . length . filter (==True) . zipWith (==) components . modname2components . T.pack . moduleNameString $ i) imports
        arst            = sortOn snd importsSorted

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just $ last xs

ambiguousField2ProposedChanged :: [ModuleName] -> [ModuleName] -> AmbiguousFieldOcc GhcRn -> (SrcSpan, RdrName -> RdrName)
ambiguousField2ProposedChanged imports ours (Unambiguous n (L l rn))
    | Just mn <- moduleName <$> nameModule_maybe n , mn `elem` ours = (l, const (Unqual $ mangle mn on))
  
    | Just mn <- moduleName <$> nameModule_maybe n = (l, const (Qual (fromRight mn $ findBestMatchingImport imports mn) on))
  
    -- this seems to not be used for now
    -- | otherwise = Unambiguous n $ L l $ Unqual $ undefined
  where on = rdrNameOcc rn
ambiguousField2ProposedChanged _ ours (Ambiguous _ (L l (Qual mn on)))
    | mn `elem` ours = (l, const (Unqual $ mangle mn on))
-- don't know what to do otherwise yet, let it crash for now
ambiguousField2ProposedChanged _ _ _ = error "ambiguousField2ProposedChanged: incomplete pattern match"

field2ProposedChange :: [ModuleName] -> [ModuleName] -> FieldOcc GhcRn -> (SrcSpan, RdrName -> RdrName)
field2ProposedChange imports ours (FieldOcc n (L l rn))
    | Just mn <- moduleName <$> nameModule_maybe n , mn `elem` ours = (l, const (Unqual $ mangle mn on))
    | Just mn <- moduleName <$> nameModule_maybe n = (l, const (Qual (fromRight mn $ findBestMatchingImport imports mn) on))
  where 
      on = rdrNameOcc rn
field2ProposedChange _ _ _ = error "field2ProposedChange: incomplete pattern match"

applyChange :: M.Map SrcSpan (RdrName -> RdrName) -> Located RdrName -> Located RdrName
applyChange m (L l r) = 
    L l . maybe r ($ r) $ M.lookup l m

          
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

-- unqualOurNames :: [ModuleName] -> Name -> Name
-- unqualOurNames ours n
--     | Just m <- nameModule_maybe n , moduleName m `elem` ours = unqualName n
--     | otherwise = n

unqualSig :: Sig GhcPs -> Sig GhcPs
unqualSig (TypeSig x names t) = TypeSig x (map (fmap unqualName) names) t
unqualSig (ClassOpSig x b names t) = ClassOpSig x b (map (fmap unqualName) names) t
unqualSig s = s

unqualBinds :: HsBindLR GhcPs GhcPs -> HsBindLR GhcPs GhcPs
unqualBinds fb@(FunBind _ (L l n) fm  _ _)
    | isQual (T.pack $ oshow n) = newFB
    | otherwise = fb
  where 
      newFM = descendBi unqualMatchCtxt fm
      newFB = fb { fun_id = L l $ unqualName n, fun_matches = newFM }
unqualBinds hb = hb

unqualMatchCtxt :: HsMatchContext RdrName -> HsMatchContext RdrName
unqualMatchCtxt = fmap unqualName

unqualName :: RdrName -> RdrName
unqualName (Qual _ on) = Unqual on
unqualName n = n

--  | make all imports qualified, hiding nothing, no aliasing and no safe imports
qualImports :: [LImportDecl p] -> [LImportDecl p]
qualImports = map (\(L l i) ->
                       L l
                       $ i { ideclQualified = True
                           , ideclHiding    = Nothing
                           , ideclAs        = Nothing
                           , ideclSafe      = False})

-- | remove imports that come from "our" modules
removeImports :: [ModuleName] -> [LImportDecl p] -> [LImportDecl p]
removeImports ours = filter go
  where
      go (L _ i) = let modName = unLoc . ideclName $ i
                   in  modName `notElem` ours && ("Prelude" /= (moduleNameString modName))


-- | too naive check if something is an operator
-- TODO: use syntax from Haskell2010 report
isOperator :: String -> Bool
isOperator = not . any isAlphaNum

renameOperator :: String -> String
renameOperator = evalRand randomOpString . mkStdGen . hash

-- | create a random operator string
randomOpString :: MonadRandom m => m String
randomOpString = do
    s1 <- replicateM 4 $ do
              i <- getRandomR (0, length operatorSymbols - 1)
              return $ operatorSymbols !! i

    b  <- getRandom

    s2 <- if b then randomOpString else return ""

    return $ "<" <> s1 <> s2 <> ">"

  where
      operatorSymbols = "!#$%&*+<>?@^~"


-- ***************************************************************************
-- CABAL FILE UTILITIES
-- ***************************************************************************
getAllPragmas :: [FilePath] -> IO [Pragma]
getAllPragmas = fmap concat . mapM getPragmas . map (fromJust . parseAbsFile)

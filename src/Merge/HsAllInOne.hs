module Merge.HsAllInOne (dieForGhcSins, hsmerge) where

import System.Process
import System.Directory
import Debug.Trace
import qualified Data.Text.IO as TIO
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
import Data.Void
import GHC.Paths

dieForGhcSins :: IO ()
dieForGhcSins = do
    -- f <- parseAbsFile "/home/daniel/workspace/hsreduce-test-cases/head.hackage/packages/pandoc-2.9.2.1/AllInOne.hs"
    dir <- parseAbsDir =<< getCurrentDirectory
    f   <- parseRelFile "AllInOne.hs"
    fastCleanUp Cabal (dir </> f)

hsmerge :: FilePath -> IO ()
hsmerge filePath = do
    cradle <- loadCradle . fromJust =<< findCradle filePath
    putStrLn . ("cradle: " <> ) . show $ cradle
    
    getCompilerOptions filePath (cradle :: Cradle Void) >>= \case
        CradleSuccess opts -> do
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

                    proposedNameChanges   <- liftIO . forM (universeBi renamedGroups) $ \(L l n) -> do
                        temp <- name2ProposedChange importsModuleNames ours n
                        return (l, temp)

                    let proposedImportsToAdd  = 
                            map ((\mn -> noLoc $ ImportDecl NoExt NoSourceText (noLoc mn) Nothing False False True False Nothing Nothing) 
                                . fromJust
                                . fst
                                . snd) 
                            $ filter (isJust . fst . snd) proposedNameChanges

                    proposedChanges       <- liftIO $ do
                        temp1 <- forM (universeBi renamedGroups) $ ambiguousField2ProposedChanged importsModuleNames ours
                        temp2 <- forM (universeBi renamedGroups) $ field2ProposedChange importsModuleNames ours
                        return . M.fromList $  (map (fmap snd) $ proposedNameChanges) <> temp1 <> temp2
                       
                    let newParsedSource = transformBi unqualSig . transformBi unqualBinds . transformBi (applyChange proposedChanges) $ myParsedSource

                    return (proposedImportsToAdd, newParsedSource)
                    
                d <- liftIO getCurrentDirectory
                let crd = cradleRootDir cradle 
                    root = if crd == "." then d else crd

                extensions <- 
                    liftIO 
                    . fmap (unlines . map show) 
                    . getAllPragmas 
                    . catMaybes 
                    -- . map (fmap (\f -> if traceShow "arst" $ traceShow (show f) $ traceShow (show filePath) $ traceShow (show root) (f == filePath) then root <> "/" <> f else f) . ml_hs_file . ms_location) 
                    . map (fmap (\f -> if f == filePath then root <> "/" <> f else f) . ml_hs_file . ms_location) 
                    $ modSums

                let 
                    annASTs      = map snd missingImportsAndAnnotatedAST
                    modName      = Just . noLoc $ mkModuleName "AllInOne"
                    importsToAdd = concatMap fst missingImportsAndAnnotatedAST
                    imports      = concatMap (qualImports . removeImports ours . hsmodImports) annASTs <> importsToAdd
                    decls        = concatMap hsmodDecls annASTs
                    mergedMod    = HsModule modName Nothing imports decls Nothing Nothing
                    -- ormolu       = ParseResult mergedMod emptyAnns Nothing [] [] False

                -- TODO: 
                --      1. create ormolu parseresult
                --      2. print parseresult
                --      3. show the text
                liftIO . writeFile "AllInOne.hs" $ unlines [extensions, oshow mergedMod]

        CradleFail err  -> error $ show err
        _               -> error "Cradle wasn't loaded successfully! Maybe you're missing a hie.yaml file?"

name2ProposedChange :: [ModuleName] -> [ModuleName] -> Name -> IO (Maybe ModuleName, (RdrName -> RdrName))
name2ProposedChange imports ours n
    -- don't rename things from the Main module to avoid making the test case print uninteresting output
    -- example: data cons renamed from T2 to T2_Main
    --   doesn't match in interestingness test matching on `fromList [T2, T2]`
    | showSDocUnsafe (pprNameUnqualified n) == "main" = return (Nothing, id)
  
    -- built-in things
    | isSystemName    n = return (Nothing, id)
    | isBuiltInSyntax n = return (Nothing, id)
    | isDataConName   n, isOperator $ tail os = return (Nothing, id)
    | isWiredInName   n, isOperator . oshow $ getRdrName n = return (Nothing, id)
  
    -- not our operator, leave unmodified
    | isOperator . oshow $ getRdrName n, Just mn <- getModuleName n, mn `notElem` ours = return (Nothing, id)

    -- our operator
    | Just mn <- getModuleName n, isOperator . oshow $ getRdrName n, mn `elem` ours 
    = return (Nothing, (const (Unqual . mkOccName ns . renameOperator $ (os ++ filter (/= '.') (moduleNameString mn)))))
  
    -- our function / variable
    | Just mn <- getModuleName n, mn `elem` ours = return (Nothing, (const (Unqual $ mangle mn on)))

    -- something external
    | Just mn <- getModuleName n = findBestMatchingImport imports mn on >>= \case
        Left  newMN  -> return (Just newMN, const (Qual newMN on))
        Right newMN  -> return (Nothing, const (Qual newMN on))
    -- | Just mn <- getModuleName n = const . flip Qual on <$> findBestMatchingImport imports mn 

    | otherwise = return (Nothing, id)
  where
      on = occName n
      ns = occNameSpace on
      os = occNameString on


findBestMatchingImport :: [ModuleName] -> ModuleName -> OccName -> IO (Either ModuleName ModuleName)
findBestMatchingImport imports mn on
    | mn `elem` imports = return $ Right mn
    -- | mn `elem` imports    = trace'' "right mn" (const (oshow mn <> ", " <> oshow imports)) $ Right mn
    -- | Just (bestMatch, bestMatchLength) <- safeLast importsSorted, bestMatchLength >= 2 = return $ Right bestMatch 
    | otherwise = getMyHoogleOn imports mn on
  where 
        components    = modname2components . T.pack $ moduleNameString mn
        importsMapped = map (\i -> ((,) i) . length . filter (==True) . zipWith (==) components . modname2components . T.pack . moduleNameString $ i) imports
        importsSorted = sortOn snd importsMapped


-- | blessed be Neill Mitchell
getMyHoogleOn :: [ModuleName] -> ModuleName -> OccName -> IO (Either ModuleName ModuleName)
getMyHoogleOn imports defaultMN on = do
    (_, stdout, _) <- flip readCreateProcessWithExitCode "" $ (shell $ "hoogle " <> oshow on)

    let proposedMN = filter (`elem` imports) . map mkModuleName . map handleLines $ lines stdout

    return $ if not $ null proposedMN
        then Right $ head proposedMN
        else Left defaultMN

  where handleLines = head . filter (`notElem` ["module", "class", "newtype"]) . words 


safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just $ last xs


ambiguousField2ProposedChanged :: [ModuleName] -> [ModuleName] -> AmbiguousFieldOcc GhcRn -> IO (SrcSpan, RdrName -> RdrName)
ambiguousField2ProposedChanged imports ours (Unambiguous n (L l rn))
    | Just mn <- moduleName <$> nameModule_maybe n , mn `elem` ours = return (l, const (Unqual $ mangle mn on))
  
    | Just mn <- moduleName <$> nameModule_maybe n = do
        temp <- fromRight mn <$> findBestMatchingImport imports mn on
        return (l, const (Qual temp on))
  
    -- this seems to not be used for now
    -- | otherwise = Unambiguous n $ L l $ Unqual $ undefined
  where on = rdrNameOcc rn
ambiguousField2ProposedChanged _ ours (Ambiguous _ (L l (Qual mn on)))
    | mn `elem` ours = return (l, const (Unqual $ mangle mn on))
-- don't know what to do otherwise yet, let it crash for now
ambiguousField2ProposedChanged _ _ _ = error "ambiguousField2ProposedChanged: incomplete pattern match"


field2ProposedChange :: [ModuleName] -> [ModuleName] -> FieldOcc GhcRn -> IO (SrcSpan, RdrName -> RdrName)
field2ProposedChange imports ours (FieldOcc n (L l rn))
    | Just mn <- moduleName <$> nameModule_maybe n , mn `elem` ours = return (l, const (Unqual $ mangle mn on))
    | Just mn <- moduleName <$> nameModule_maybe n = do
        temp <- fromRight mn <$> findBestMatchingImport imports mn on
        return (l, const (Qual temp on))
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
      newFM = transformBi unqualMatchCtxt fm
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
getAllPragmas = fmap concat . mapM getPragmas . map (fromJust . parseAbsFile )

-- ***************************************************************************
-- CLEANING UP
-- ***************************************************************************
-- callCabal :: GhcMode -> IO ()
-- callCabal mode = do
--     srcSpans <- getGhcOutput Cabal mode (dir </> f)
--     print srcSpans
--     return ()

fastCleanUp :: Tool -> Path Abs File -> IO ()
fastCleanUp tool sourcePath = do
    cleanUp tool Indent sourcePath
--    cleanUp tool MissingImport sourcePath -- add imports
--    cleanUp tool HiddenImport sourcePath  -- clean up imports
--    cleanUp tool NotInScope sourcePath       -- clean up uses of hidden modules
--    -- cleanUp tool PerhapsYouMeant sourcePath

allowedToLoop :: [GhcMode]
allowedToLoop = [Indent]

cleanUp :: Tool -> GhcMode -> Path Abs File -> IO ()
cleanUp tool mode sourcePath = do
    fileContent <- TIO.readFile (fromAbsFile sourcePath)
    
    getGhcOutput tool mode sourcePath >>= \case
        Nothing -> return ()
        Just [] -> return ()
        Just mySpans -> do
            banner (show mode)
    
            let 
                rightSpans = (if mode == Indent then id else filter ((/="") . fst)) . map (\(Right t, s) -> (t, s)) . filter (isRight . fst) $ mySpans
    
                newFileContent = case mode of
    
                    Indent -> foldr insertIndent fileContent . map (fmap span2Locs) $ rightSpans
    
                    MissingImport -> 
                        let contentLines = T.lines fileContent
                            prefix = T.unlines $ takeWhile (not . ("import qualified" `T.isPrefixOf`)) contentLines
                            suffix = T.unlines $ dropWhile (not . ("import qualified" `T.isPrefixOf`)) contentLines
                        in
                            prefix <> (T.unlines . map ("import qualified " <>) . map (trace'' "cleanUp" show) . nub . map fst $ rightSpans) <> suffix
    
                    HiddenImport ->
                        T.unlines 
                        . map (\(i, l) -> 
                                let myLines = (map (\(m, s) -> (m, srcLocLine . fst . span2Locs $ s)) rightSpans) 
                                in 
                                    if  i `elem` map snd myLines && "import qualified" `T.isPrefixOf` l 
                                    then 
                                        let wl      = T.words l 
                                            modName = fst . head $ filter ((==i) . snd) myLines
                                        in traceShow ("Changing " <> last wl <> " to " <> modName <> " at line " <> (T.pack $ show i)) . T.unwords $ init wl <> [modName] 
                                    else 
                                        l) 
                        . zip [1..]
                        . T.lines 
                        $ fileContent
                
                    _ -> foldr replaceWithGhcOutput fileContent . map (fmap span2Locs) $ rightSpans
    
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

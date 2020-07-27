module Merge.HsAllInOne (dieForGhcSins, hsmerge) where

import Digraph 
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
    dir <- parseAbsDir =<< getCurrentDirectory
    f   <- parseRelFile "AllInOne.hs"
    putStrLn "cleaning up"
    fastCleanUp Ghc (dir </> f)


hsmerge :: FilePath -> IO ()
hsmerge filePath = do
    cradle <- loadCradle . fromMaybe (error "cradle could not be found!") =<< findCradle filePath
    putStrLn . ("cradle: " <> ) $ show cradle
    
    getCompilerOptions filePath (cradle :: Cradle Void) >>= \case
        CradleSuccess opts -> do
            void . runGhc (Just libdir) $ do
                initSession opts >>= setTargets 
                void $ load LoadAllTargets

                mg <- getModuleGraph 
                let modSums = concatMap flattenSCC $ topSortModuleGraph False mg Nothing 

                let ours = map (moduleName . ms_mod) modSums

                missingImportsAndAnnotatedAST <- flip traverse modSums $ \modSum -> do

                    t <- parseModule modSum >>= typecheckModule
                    
                    let 
                        myParsedSource        = (if (oshow . moduleName . ms_mod) modSum == "Text.Pandoc.Extensions" then transformBi grst else id) . unLoc . pm_parsed_source $ tm_parsed_module t
                        importsModuleNames    = map (unLoc . ideclName . unLoc) $ hsmodImports myParsedSource
                        (renamedGroups,_,_,_) = fromMaybe (error "HsAllInOne->hsmerge: no renamed source!") $ tm_renamed_source t

                    proposedNameChanges   <- liftIO . forM (universeBi renamedGroups) $ \(L l n) -> do
                        temp <- name2ProposedChange importsModuleNames ours n
                        return (l, temp)

                    let proposedImportsToAdd  = 
                            map (\mn -> noLoc $ ImportDecl NoExt NoSourceText (noLoc mn) Nothing False False True False Nothing Nothing)
                            . catMaybes
                            $ map (arst . snd) proposedNameChanges


                    proposedChanges       <- liftIO $ do
                        temp1 <- forM (universeBi renamedGroups) $ ambiguousField2ProposedChanged importsModuleNames ours
                        temp2 <- forM (universeBi renamedGroups) $ field2ProposedChange importsModuleNames ours
                        return . M.fromList $ (map (fmap crst) $ proposedNameChanges) <> temp1 <> temp2
                    
                       
                    let 
                        drst = M.fromList $ map ((\Temp{..} -> (oshow $ fromJust brst, crst $ fromJust brst)) . snd) $ filter (isJust . brst . snd) proposedNameChanges

                    
                    if 
                        (oshow . moduleName . ms_mod) modSum `elem` ["Text.Pandoc.Extensions", "Text.Pandoc.Data"]
                    then do
                        liftIO $ print $ oshow $ M.lookup ("Extension") drst
                        liftIO $ print $ oshow $ M.lookup ("T.Text") drst
                        liftIO $ print $ oshow drst
                    else return ()

                    let newParsedSource = transformBi unqualFamEqn . transformBi unqualSig . transformBi unqualBinds . transformBi (applyChange drst) $ myParsedSource

                    return (proposedImportsToAdd, newParsedSource)

                    
                d <- liftIO getCurrentDirectory
                let crd = cradleRootDir cradle 
                    root = if crd == "." then d else crd

                extensions <- 
                    liftIO 
                    . fmap (unlines . map show . nub) 
                    . getAllPragmas 
                    . catMaybes 
                    . map (fmap (\f -> if f == filePath then root <> "/" <> f else f) . ml_hs_file . ms_location) 
                    $ modSums

                let 
                    annASTs      = map snd missingImportsAndAnnotatedAST
                    modName      = Just . noLoc $ mkModuleName "AllInOne"
                    importsToAdd = concatMap fst missingImportsAndAnnotatedAST
                    imports      = nub $ concatMap (qualImports . removeImports ours . hsmodImports) annASTs <> importsToAdd
                    decls        = concatMap hsmodDecls annASTs
                    mergedMod    = HsModule modName Nothing imports decls Nothing Nothing

                liftIO . writeFile "AllInOne.hs" $ unlines [extensions, oshow mergedMod]

        CradleFail err  -> error $ show err
        _               -> error "Cradle wasn't loaded successfully! Maybe you're missing a hie.yaml file?"

grst :: HsSplice GhcPs -> HsSplice GhcPs
grst s = traceShow (gshow s) $ traceShow (oshow s) $ s

instance Eq (ImportDecl GhcPs) where
    i1 == i2 = oshow i1 == oshow i2

data Temp = Temp {
      arst :: Maybe ModuleName
    , brst :: Maybe RdrName
    , crst :: RdrName -> RdrName
    }

name2ProposedChange :: [ModuleName] -> [ModuleName] -> Name -> IO Temp
name2ProposedChange imports ours n
    -- don't rename things from the Main module to avoid making the test case print uninteresting output
    -- example: data cons renamed from T2 to T2_Main
    --   doesn't match in interestingness test matching on `fromList [T2, T2]`
    | showSDocUnsafe (pprNameUnqualified n) == "main" = return $ Temp Nothing Nothing id
  
    -- built-in things
    | isSystemName    n                                     = return $ Temp Nothing Nothing id
    | isBuiltInSyntax n                                     = return $ Temp Nothing Nothing id
    | isDataConName   n, isOperator $ tail os               = return $ Temp Nothing Nothing id
    | isWiredInName   n, isOperator . oshow $ getRdrName n  = return $ Temp Nothing Nothing id
  
    -- not our operator, leave unmodified
    | isOperator . oshow $ getRdrName n, Just mn <- getModuleName n, mn `notElem` ours = return $ Temp Nothing Nothing id

    -- our operator
    | Just mn <- getModuleName n, isOperator . oshow $ getRdrName n, mn `elem` ours 
    = return $ Temp Nothing (Just $ (Unqual . mkOccName ns . renameOperator $ (os ++ filter (/= '.') (moduleNameString mn)))) (const (Unqual . mkOccName ns . renameOperator $ (os ++ filter (/= '.') (moduleNameString mn))))
  
    -- our function / variable
    | Just mn <- getModuleName n, mn `elem` ours = return $ Temp Nothing (Just . Unqual $ mangle mn on) (const (Unqual $ mangle mn on))

    -- something external
    | Just mn <- getModuleName n, mn `notElem` stuffFromBase = findBestMatchingImport imports mn on >>= \case
        Left  newMN  -> return $ Temp (Just newMN) (Just $ Qual newMN on) (const (Qual newMN on))
        Right newMN  -> return $ Temp Nothing (Just $ Qual newMN on) (const (Qual newMN on))

    -- qualifying things from base to avoid people importing those modules aliased
    | Just mn <- getModuleName n, mn `elem` stuffFromBase = return $ Temp (Just mn) (Just $ Qual mn on) (const (Qual mn on))

    | otherwise = return $ Temp Nothing Nothing id
  where
      on = occName n
      rn = getRdrName n
      ns = occNameSpace on
      os = occNameString on
      stuffFromBase = 
          map mkModuleName 
                [ "Control.Monad"
                , "Control.Monad.Fail"
                , "GHC.Classes"
                , "GHC.Err"
                , "GHC.Exts"
                , "GHC.List"
                , "GHC.Magic"
                , "GHC.Maybe"
                , "GHC.Num"
                , "GHC.Prim"
                , "GHC.Read"
                , "GHC.Show"
                , "GHC.Types"
                , "GHC.Enum"
                , "GHC.Real"
                , "System.IO"
                , "Data.String"
                , "GHC.Base" ]


-- *** START OF CRAP CODE
findBestMatchingImport :: [ModuleName] -> ModuleName -> OccName -> IO (Either ModuleName ModuleName)
findBestMatchingImport imports mn _
    | mn `elem` imports                              = return $ Right mn
    | Just shortMN <- tryShortenedModName imports mn = return $ Right shortMN
    | otherwise                                      = return $ Left mn -- getMyHoogleOn imports mn on
    -- | otherwise = return . Right $ tryShortenedModName imports mn 


-- tryShortenedModName :: [ModuleName] -> ModuleName -> ModuleName
-- tryShortenedModName imports mn = imports !! i
--   where
--     mnString = moduleNameString mn
--     i = fst . head . sortOn snd . zip [0..] $ map ((levenshteinDistance defaultEditCosts mnString) . moduleNameString) imports

tryShortenedModName :: [ModuleName] -> ModuleName -> Maybe ModuleName
tryShortenedModName imports mn = 
    fmap (mkModuleName . T.unpack) . foldr (go imports) (Just mnString) $ modname2components mnString
  where 
        mnString = T.pack $ moduleNameString mn

        go goImports _ (Just goMN)
            | (mkModuleName $ T.unpack goMN) `elem` goImports = Just goMN
            | (init $ modname2components goMN) /= []          = Just . T.intercalate "." . init $ modname2components goMN
            | otherwise = Nothing
        go _ _ Nothing  = Nothing

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


applyChange :: M.Map String RdrName -> RdrName -> RdrName
applyChange drst r = 
    case M.lookup (oshow r) drst of
        Nothing -> r
        Just g  -> g

-- applyChange :: M.Map SrcSpan (RdrName -> RdrName) -> M.Map String RdrName -> Located RdrName -> Located RdrName
-- applyChange m drst (L l r) = 
--     L l $ case M.lookup l m of
--         Nothing -> case M.lookup (oshow r) drst of
--             Nothing -> r
--             Just g  -> g
--         Just f  -> f r

          
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

unqualFamEqn :: FamEqn GhcPs (HsTyPats GhcPs) (LHsType GhcPs) -> FamEqn GhcPs (HsTyPats GhcPs) (LHsType GhcPs)
unqualFamEqn (FamEqn _ n pats fixity rhs) 
    | isQual (T.pack . oshow $ unLoc n) = FamEqn NoExt (unqualName <$> n) pats fixity rhs
unqualFamEqn f = f


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
getAllPragmas = fmap concat . mapM getPragmas . map (\f -> fromMaybe (error $ "could not parse path as absolute file: " <> f) $ parseAbsFile f)

-- ***************************************************************************
-- CLEANING UP
-- ***************************************************************************
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

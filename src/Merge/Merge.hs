module Merge.Merge (hsmerge) where


import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe
import qualified Data.Map as M
import Data.IORef
import Control.Monad
import qualified Data.List.NonEmpty as NE
import Control.Monad.Random 
import Data.Generics.Uniplate.Data 
import Data.Hashable (hash)
import Data.List
import Data.Traversable (for)
import Data.Void (Void)
import Debug.Trace 
import Digraph (flattenSCC)
import GHC hiding (GhcMode)
import GHC.Paths (libdir)
import GhcPlugins hiding ((<>), GhcMode)
import HIE.Bios
    ( Cradle (cradleRootDir),
      CradleLoadResult (CradleFail, CradleSuccess),
      findCradle,
      getCompilerOptions,
      initSession,
      loadCradle,
    )
import Util.Parser (getPragmas)
import Path 
import Path.IO
import TcRnTypes (tcg_rdr_env)
import Util.Types
import Util.Util


hsmerge :: ProjectType -> T.Text -> IO ()
hsmerge isExecutable targetName = do
    let 
        filePath = "hie.yaml"
        targetTypeS = case isExecutable of
            Executable -> "exe"
            Library -> "lib"
        fileContent = "cradle: {cabal: {component: \"" <> targetTypeS <> ":" <> targetName <> "\" }}"

    TIO.writeFile filePath fileContent

    cradle <- loadCradle . fromMaybe (error "cradle could not be found!") =<< findCradle filePath
    putStrLn . ("cradle: " <>) $ show cradle

    getCompilerOptions filePath (cradle :: Cradle Void) >>= \case
        CradleSuccess opts -> do
            runGhc (Just libdir) $ do
                initSession opts >>= setTargets
                void $ load LoadAllTargets

                mg <- getModuleGraph

                let modSums = concatMap flattenSCC $ topSortModuleGraph False mg Nothing
                    ours = map (moduleName . ms_mod) modSums

                liftIO $ putStrLn "merging these modules together:"
                liftIO $ print $ map oshow ours

                modName2Map <- liftIO . newIORef $ M.empty

                importsAndannASTs <- for modSums $ \modSum -> do
                    t <- typecheckModule =<< parseModule modSum
                    dynFlags <- getDynFlags

                    solveReexportMap <- liftIO $ readIORef modName2Map

                    let myMN = ms_mod modSum
                        rdrEnv = tcg_rdr_env . fst $ tm_internals_ t

                        -- get imports
                        importsModuleNames =
                            qualImports
                                . removeImports ours
                                . hsmodImports
                                . unLoc
                                . pm_parsed_source
                                $ tm_parsed_module t

                        -- rename module
                        (renamedGroups, imports, mexports, _) = fromMaybe (error "HsAllInOne->hsmerge: no renamed source!") $ tm_renamed_source t
                        newGroups =
                            transformBi unqualSig
                                . transformBi unqualFamEqnClsInst
                                . transformBi unqualBinds
                                . transformBi (renameName rdrEnv solveReexportMap ours myMN)
                                . transformBi (renameField rdrEnv solveReexportMap ours myMN)
                                . transformBi (renameAmbiguousField rdrEnv solveReexportMap ours myMN)
                                $ renamedGroups

                    let exportNames = case mexports of
                            Just exports -> [ n | n :: Name <- concatMap (ieNames . unLoc . fst) exports]
                            Nothing -> []
                    let name2ModName = M.fromList $ mapMaybe (sequence . (\n -> (n, getModuleName rdrEnv solveReexportMap ours myMN n))) $ exportNames <> [ n | n :: Name <- universeBi renamedGroups ]
                    
                    liftIO $ modifyIORef modName2Map $ M.insert (moduleName myMN) (map (unLoc . ideclName . unLoc) imports, name2ModName)

                    return (importsModuleNames, mshow dynFlags (newGroups :: HsGroup GhcRn))

                d <- fromAbsDir <$> liftIO getCurrentDir
                let crd = cradleRootDir cradle
                    root = if crd == "." then d else crd

                extensions <-
                    liftIO
                        . fmap (map show . nub)
                        . getAllPragmas
                        . mapMaybe (fmap (\f -> if f == filePath then root <> "/" <> f else f) . ml_hs_file . ms_location)
                        $ modSums

                let imports = nub $ concatMap fst importsAndannASTs
                    decls = unlines $ map snd importsAndannASTs

                liftIO . writeFile "AllInOne.hs" $ unlines $
                    extensions
                        <> [ if "Main.hs" `isInfixOf` filePath then "module Main where" else "module AllInOne where",
                             "import qualified Prelude"
                           ]
                        <> map oshow imports
                        <> [decls]

            putStrLn "cleaning up"
            dir <- getCurrentDir
            f <- parseRelFile "AllInOne.hs"
            cleanUp Ghc Indent (dir </> f)
        CradleFail err -> error $ show err
        _ -> error "Cradle wasn't loaded successfully! Maybe you're missing a hie.yaml file?"

renameName :: GlobalRdrEnv -> M.Map ModuleName ([ModuleName], M.Map Name ModuleName) -> [ModuleName] -> Module -> Name -> Name
renameName rdrEnv solveReexportMap ours myMN@(Module unitId _) n
    | oshow n == "main" = n

    -- -- built-in things
    | isBuiltInSyntax n = n
    -- our operator / function / variable
    | Just mn <- getModuleName rdrEnv solveReexportMap ours myMN n,
      mn `elem` ours =
        mkInternalName u (mangle mn on) noSrcSpan
    -- something external
    | Just mn <- getModuleName rdrEnv solveReexportMap ours myMN n =
        -- using my unit id shouldn't be a problem because
        -- we're just printing these names and not checking later from which module the names come
        -- mkExternalName u (mkModule unitId mn) on noSrcSpan
        mkExternalName u (mkModule unitId mn) on noSrcSpan
    | otherwise = n
    where
        u = nameUnique n
        on = occName n

getModuleName :: GlobalRdrEnv -> M.Map ModuleName ([ModuleName], M.Map Name ModuleName) -> [ModuleName] -> Module -> Name -> Maybe ModuleName
getModuleName env solveReexportMap ours myMN n
    | (gre_lcl <$> rdrElt) == Just True = Just $ moduleName myMN
    | otherwise = do
        imports <- gre_imp <$> rdrElt
        importMN <- is_mod . is_decl <$> listToMaybe imports
        exactMN <- moduleName <$> nameModule_maybe n
        if importMN `elem` ours && importMN /= exactMN
            then case resolveImportedReexport solveReexportMap n importMN of
                Nothing -> Just exactMN
                e -> e
            else Just importMN
    where
        rdrElt = lookupGRE_Name env n

resolveImportedReexport :: M.Map ModuleName ([ModuleName], M.Map Name ModuleName) -> Name -> ModuleName -> Maybe ModuleName
resolveImportedReexport solveReexportMap n importMN = case M.lookup importMN solveReexportMap of
    Just (imports, tempMap) -> case M.lookup n tempMap of
        Just realMN -> Just realMN
        Nothing -> listToMaybe $ mapMaybe (resolveImportedReexport solveReexportMap n) imports
    Nothing -> Nothing

getModuleNameFromRdrName :: GlobalRdrEnv -> Module -> RdrName -> Maybe ModuleName
getModuleNameFromRdrName env mn n
    | (gre_lcl <$> rdrElt) == Just True = Just $ moduleName mn
    | otherwise = do
        imports <- gre_imp <$> rdrElt
        is_mod . is_decl <$> listToMaybe imports
    where
        rdrElt = listToMaybe $ lookupGRE_RdrName n env

renameAmbiguousField :: GlobalRdrEnv -> M.Map ModuleName ([ModuleName], M.Map Name ModuleName) -> [ModuleName] -> Module -> AmbiguousFieldOcc GhcRn -> AmbiguousFieldOcc GhcRn
renameAmbiguousField rdrEnv solveReexportMap ours myMN@(Module unitId _) f@(Unambiguous n (L l rn))
    -- internal
    | Just mn <- getModuleName rdrEnv solveReexportMap ours myMN n,
      mn `elem` ours =
        Unambiguous (mkInternalName u (mangle mn on) noSrcSpan) . L l . Unqual $ mangle mn on
    -- external
    | Just mn <- getModuleName rdrEnv solveReexportMap ours myMN n =
        Unambiguous (mkExternalName u (mkModule unitId mn) on noSrcSpan) . L l $ Qual mn on
    -- internal
    | Qual mn _ <- rn,
      mn `elem` ours =
        Unambiguous (mkInternalName u (mangle mn on) noSrcSpan) . L l . Unqual $ mangle mn on
    | otherwise = f
    where
        u = nameUnique n
        on = rdrNameOcc rn
renameAmbiguousField rdrEnv _ ours myMN f@(Ambiguous _ (L l rn))
    | Just mn <- getModuleNameFromRdrName rdrEnv myMN rn, mn `elem` ours = Ambiguous NoExtField . L l . Unqual $ mangle mn on
    | Just mn <- getModuleNameFromRdrName rdrEnv myMN rn = Ambiguous NoExtField . L l $ Qual mn on
    | Qual mn _ <- rn, mn `elem` ours = Ambiguous NoExtField . L l . Unqual $ mangle mn on
    | otherwise = f
    where
        on = rdrNameOcc rn
renameAmbiguousField _ _ _ _ f = f

renameField :: GlobalRdrEnv -> M.Map ModuleName ([ModuleName], M.Map Name ModuleName) -> [ModuleName] -> Module -> FieldOcc GhcRn -> FieldOcc GhcRn
renameField rdrEnv solveReexportMap ours myMN@(Module unitId _) (FieldOcc n (L l rn))
    | Just mn <- getModuleName rdrEnv solveReexportMap ours myMN n, mn `elem` ours = FieldOcc (mkInternalName u (mangle mn on) noSrcSpan) . L l . Unqual $ mangle mn on
    | Just mn <- getModuleName rdrEnv solveReexportMap ours myMN n = FieldOcc (mkExternalName u (mkModule unitId mn) on noSrcSpan) . L l $ Qual mn on
    where
        u = nameUnique n
        on = rdrNameOcc rn
renameField _ _ _ _ f = f

-- ***************************************************************************
-- MERGING MODULES UTILITIES

-- ***************************************************************************
-- only unqual names in associated type families!
unqualFamEqnClsInst :: p ~ GhcRn => ClsInstDecl p -> ClsInstDecl p
unqualFamEqnClsInst = transformBi unqualFamEqn

unqualFamEqn :: (pass ~ GhcRn) => FamEqn pass (LHsType GhcRn) -> FamEqn pass (LHsType GhcRn)
unqualFamEqn fe@FamEqn {feqn_tycon = L l n} = fe {feqn_tycon = L l $ unqualName n}
unqualFamEqn f = f

unqualSig :: p ~ GhcRn => Sig p -> Sig p
unqualSig (TypeSig x names t) = TypeSig x (map (fmap unqualName) names) t
unqualSig (ClassOpSig x b names t) = ClassOpSig x b (map (fmap unqualName) names) t
unqualSig s = s

unqualBinds :: p ~ GhcRn => HsBindLR p p -> HsBindLR p p
unqualBinds fb@(FunBind _ (L l n) fm _ _) = newFB
    where
        newFM = transformBi unqualMatchCtxt fm
        newFB = fb {fun_id = L l $ unqualName n, fun_matches = newFM}
unqualBinds hb = hb

unqualMatchCtxt :: HsMatchContext Name -> HsMatchContext Name
unqualMatchCtxt = fmap unqualName

unqualName :: Name -> Name
unqualName n 
    | isInternalName n = n
    | otherwise = mkInternalName u on noSrcSpan
    where
        u = nameUnique n
        on = occName n

mangle :: ModuleName -> OccName -> OccName
mangle mn on 
    | isSymOcc on, Just os <- mOS = mkOccName ns $ T.unpack $ NE.head os `T.cons` renameOperator (T.pack $ NE.toList os <> filter (/= '.') (moduleNameString mn))
    | otherwise, Just os <- mOS = mkOccName ns $ NE.toList os <> "_" <> filter (/= '.') (moduleNameString mn)
    | otherwise = on
    where
        mOS = NE.nonEmpty $ occNameString on
        ns = occNameSpace on

--  | make all imports qualified, hiding nothing, no aliasing and no safe imports
qualImports :: [LImportDecl p] -> [LImportDecl p]
qualImports =
    map
        ( \(L l i) ->
              L l $
                  i
                      { ideclQualified = QualifiedPre,
                        ideclHiding = Nothing,
                        ideclAs = Nothing,
                        ideclSafe = False
                      }
        )

-- | remove imports that come from "our" modules
removeImports :: [ModuleName] -> [LImportDecl p] -> [LImportDecl p]
removeImports ours = filter go
    where
        go (L _ i) =
            let modName = unLoc . ideclName $ i
             in modName `notElem` ours && "Prelude" /= moduleNameString modName

renameOperator :: T.Text -> T.Text
renameOperator = evalRand randomOpString . mkStdGen . hash

-- | create a random operator string
randomOpString :: MonadRandom m => m T.Text
randomOpString = do
    s1 <- replicateM 2 $ do
        i <- getRandomR (0, length operatorSymbols - 1)
        return $ operatorSymbols !! i

    b <- getRandom

    s2 <- if b then randomOpString else return ""

    return $ "<" <> T.pack s1 <> s2 <> ">"
    where
        operatorSymbols = "!#$%&*+<>?@^~"

-- ***************************************************************************
-- CABAL FILE UTILITIES

-- ***************************************************************************

getAllPragmas :: [FilePath] -> IO [Pragma]
getAllPragmas =
    fmap concat . mapM (getPragmas <=< resolveFile')

-- ***************************************************************************
-- CLEANING UP

-- ***************************************************************************

allowedToLoop :: [GhcMode]
allowedToLoop = [Indent, PerhapsYouMeant]

cleanUp :: Tool -> GhcMode -> Path Abs File -> IO ()
cleanUp tool mode sourcePath = do
    fileContent <- TIO.readFile (fromAbsFile sourcePath)

    getGhcOutput mode sourcePath >>= \case
        Nothing -> return ()
        Just [] -> return ()
        Just mySpans -> do
            banner (show mode)

            let rightSpans =
                    (if mode == Indent then id else filter ((/= "") . fst)) mySpans
                newFileContent = case mode of
                    Indent -> foldr (insertIndent . fmap span2Locs) fileContent rightSpans
                    _ -> fileContent

            TIO.writeFile (fromAbsFile sourcePath) newFileContent
            when (mode `elem` allowedToLoop) $ cleanUp tool mode sourcePath

insertIndent :: (T.Text, (RealSrcLoc, RealSrcLoc)) -> T.Text -> T.Text
insertIndent (_, (startLoc, _)) fileContent =
    T.unlines $ prevLines <> [traceShow ("inserting indent at line " <> show (currentIndex + 1)) newLineContent] <> succLines
    where
        contentLines = T.lines fileContent
        lineStart = srcLocLine startLoc
        currentIndex = lineStart -1
        prevLines = take currentIndex contentLines
        succLines = drop lineStart contentLines
        currentLine = contentLines !! currentIndex
        newLineContent = "  " <> currentLine

span2Locs :: RealSrcSpan -> (RealSrcLoc, RealSrcLoc)
span2Locs s = (realSrcSpanStart s, realSrcSpanEnd s)

instance {-# OVERLAPPABLE #-} Eq (ImportDecl GhcPs) where
    i1 == i2 = oshow i1 == oshow i2
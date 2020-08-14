module HsAllInOne where

import TcRnTypes
import Control.Applicative
import Control.Monad.Random
import Data.Either
import Data.Function hiding (on)
import Data.Generics.Uniplate.Data
import Data.Hashable
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Traversable
import Data.Void
import Debug.Trace
import Digraph
import GHC hiding (GhcMode, extensions)
import GHC.Paths
import GhcPlugins hiding ((<&&>), (<>), GhcMode, count, extensions, isQual, mkUnqual, qualName)
import HIE.Bios
import Lens.Micro.Platform
import Parser
import Path
import System.Directory
import System.Process
import Text.EditDistance
import Types
import Util

dieForGhcSins :: IO ()
dieForGhcSins = do
    dir <- parseAbsDir =<< getCurrentDirectory
    f <- parseRelFile "AllInOne.hs"
    putStrLn "cleaning up"
    fastCleanUp Ghc (dir </> f)

hsmerge :: FilePath -> IO ()
hsmerge filePath = do
    cradle <- loadCradle . fromMaybe (error "cradle could not be found!") =<< findCradle filePath
    putStrLn . ("cradle: " <>) $ show cradle

    getCompilerOptions filePath (cradle :: Cradle Void) >>= \case
        CradleSuccess opts ->
            void . runGhc (Just libdir) $ do
                initSession opts >>= setTargets
                void $ load LoadAllTargets

                mg <- getModuleGraph

                let modSums = concatMap flattenSCC $ topSortModuleGraph False mg Nothing
                    ours = map (moduleName . ms_mod) modSums

                missingImportsAndAnnotatedAST <- for modSums $ \modSum -> do
                    t <- parseModule modSum >>= typecheckModule

                    let rdrEnv = tcg_rdr_env . fst $ tm_internals_ t

                    let myParsedSource = unLoc . pm_parsed_source $ tm_parsed_module t
                        importsModuleNames = map (unLoc . ideclName . unLoc) $ hsmodImports myParsedSource
                        (renamedGroups, _, _, _) = fromMaybe (error "HsAllInOne->hsmerge: no renamed source!") $ tm_renamed_source t

                    proposedNameChanges <- liftIO . forM (universeBi renamedGroups :: [Located Name]) $ \(L l n) -> do
                        when ("Affine" `isInfixOf` oshow n ) $ do
                            print $ oshow n
                            print $ oshow $ occName n
                            print $ oshow $ getMNFromEnv rdrEnv $ occName n
                            print $ maybe False (`elem` ours) $ getMNFromEnv rdrEnv $ occName n
                            print $ oshow ours
                            print $ nameStableString n

                        temp <- name2ProposedChange rdrEnv importsModuleNames ours n
                        --(if temp == Change Nothing Nothing then traceShow (oshow n) . traceShow (oshow $ getModuleName n) else id) $
                        return (l, temp)

                    let proposedImportsToAdd =
                            map (\mn -> noLoc $ ImportDecl NoExt NoSourceText (noLoc mn) Nothing False False True False Nothing Nothing)
                                . (mkModuleName "Prelude" :)
                                . map (\(AddImportChangeName mn _) -> mn)
                                . filter
                                    ( \case
                                          AddImportChangeName _ _ -> True
                                          _ -> False
                                    )
                                . map snd
                                $ proposedNameChanges

                    proposedChanges <- liftIO $ do
                        temp1 <- map (fmap snd) <$> forM (universeBi renamedGroups) (ambiguousField2ProposedChanged rdrEnv importsModuleNames ours)
                        temp2 <- map (fmap snd) <$> forM (universeBi renamedGroups) (field2ProposedChange rdrEnv importsModuleNames ours)

                        let temp3 =
                                catMaybes
                                    . map
                                        ( sequence
                                              . fmap
                                                  ( \case
                                                        AddImportChangeName _ newRN -> Just $ snd newRN
                                                        ChangeName newRN -> Just $ snd newRN
                                                        NoChange -> Nothing
                                                  )
                                        )
                                    $ proposedNameChanges

                        -- liftIO $ print $ oshow $ M.lookup "TextType" $ M.fromList $ map (first oshow) drst
                        -- liftIO $ print $ oshow $ M.lookup "P'.TextType" $ M.fromList $ map (first oshow) drst

                        return . M.fromList $ temp3 <> temp1 <> temp2

                    -- if
                    --     (oshow . moduleName . ms_mod) modSum `elem` ["Text.Pandoc.Extensions", "Text.Pandoc.Data"]
                    -- then do
                    --     liftIO $ print $ oshow $ M.lookup ("Extension") drst
                    --     liftIO $ print $ oshow $ M.lookup ("T.Text") drst
                    --     liftIO $ print $ oshow drst
                    -- else return ()

                    let newParsedSource =
                            transformBi unqualFamEqn
                                . transformBi unqualSig
                                . transformBi unqualBinds
                                . transformBi (applyChange proposedChanges)
                                $ myParsedSource

                    return (proposedImportsToAdd, newParsedSource)

                d <- liftIO getCurrentDirectory
                let crd = cradleRootDir cradle
                    root = if crd == "." then d else crd

                extensions <-
                    liftIO
                        . fmap (unlines . map show . nub)
                        . getAllPragmas
                        . mapMaybe (fmap (\f -> if f == filePath then root <> "/" <> f else f) . ml_hs_file . ms_location)
                        $ modSums

                let annASTs = map snd missingImportsAndAnnotatedAST
                    modName = Just . noLoc $ mkModuleName "AllInOne"
                    importsToAdd = concatMap fst missingImportsAndAnnotatedAST
                    imports = nub $ concatMap (qualImports . removeImports ours . hsmodImports) annASTs <> importsToAdd
                    decls = concatMap hsmodDecls annASTs
                    mergedMod = HsModule modName Nothing imports decls Nothing Nothing

                liftIO . writeFile "AllInOne.hs" $ unlines [extensions, oshow mergedMod]
        CradleFail err -> error $ show err
        _ -> error "Cradle wasn't loaded successfully! Maybe you're missing a hie.yaml file?"

data Change
    = AddImportChangeName ModuleName (RdrName, RdrName)
    | ChangeName (RdrName, RdrName)
    | NoChange
    deriving (Eq)

-- run this in the maybe monad
getMNFromEnv :: GlobalRdrEnv -> OccName -> Maybe ModuleName
getMNFromEnv rdrEnv on = do
    temp1 <- listToMaybe $ lookupGlobalRdrEnv rdrEnv on
    is_mod . is_decl <$> (listToMaybe $ gre_imp temp1)

name2ProposedChange :: GlobalRdrEnv -> [ModuleName] -> [ModuleName] -> Name -> IO Change
name2ProposedChange rdrEnv imports ours n
    -- don't rename things from the Main module to avoid making the test case print uninteresting output
    -- example: data cons renamed from T2 to T2_Main
    --   doesn't match in interestingness test matching on `fromList [T2, T2]`

    -- \| (if "Text" `isInfixOf` oshow n then traceShow (oshow $ getModuleName n) . traceShow (oshow rn) . traceShow (os) . traceShow (oshow on) else id) $ showSDocUnsafe (pprNameUnqualified n) == "main"       = return $ Change Nothing Nothing

    | showSDocUnsafe (pprNameUnqualified n) == "main" = return $ NoChange
    -- built-in things
    | isSystemName n = return $ NoChange
    | isBuiltInSyntax n = return $ NoChange
    -- \| isDataConName   n, isOperator $ tail os         = return $ Change Nothing Nothing
    -- \| isWiredInName n, isOperator . oshow $ getRdrName n = return $ NoChange

    -- External operators should just be qualified
    -- -- not our operator, leave unmodified
    -- \| isOperator . oshow $ getRdrName n, Just mn <- getModuleName n, mn `notElem` ours = return $ Change Nothing Nothing

    -- our operator
    | Just mn <- getModuleName n,
      isOperator . oshow $ getRdrName n,
      mn `elem` ours =
        return $ ChangeName $ (rn, Unqual . mkOccName ns $ head os : renameOperator (os ++ filter (/= '.') (moduleNameString mn)))
    -- our function / variable
    | Just mn <- getModuleName n,
      mn `elem` ours =
        return $ ChangeName $ (rn, Unqual $ mangle mn on)
    -- something external
    | Just mn <- getModuleName n,
      mn `notElem` stuffFromBase =
        -- if mn `elem` imports
        --     then return $ ChangeName (rn, Qual mn on)
        --     else do
        --         if isQual . T.pack $ oshow n
        --         then do
        --             return $ ChangeName (rn, Unqual on)

        --         else do
        --             print $ oshow n
        --             print $ oshow on
        --             return $ NoChange

        case findBestMatchingImport imports mn on of
            Nothing -> do
                -- return NoChange
                -- return $ AddImportChangeName newMN (rn, Qual newMN on)


                -- when ("try" `isInfixOf` oshow n) $ do
                --     print "findBestImport"
                --     -- print $ nameStableString n
                --     -- print $ oshow n
                --     -- print $ oshow on

                -- if (isQual . T.pack $ oshow n) || (isQual . T.pack $ oshow on)
                --     then return $ ChangeName (rn, Unqual on)
                --     else return $ NoChange

                case getMNFromEnv rdrEnv on of
                    Nothing -> return $ ChangeName (rn, Unqual on)
                    Just newMN -> return $ ChangeName (rn, Qual newMN on)

            Just newMN -> return $ ChangeName (rn, Qual newMN on)
    -- qualifying things from base to avoid people importing those modules aliased
    | Just mn <- getModuleName n, mn `elem` stuffFromBase = return $ AddImportChangeName mn (rn, Qual mn on)
    | otherwise = return NoChange
    where
        on = occName n
        rn = getRdrName n
        ns = occNameSpace on
        os = occNameString on
        stuffFromBase =
            map
                mkModuleName
                [ "Control.Monad",
                  "Control.Monad.Fail",
                  "GHC.Classes",
                  "GHC.Err",
                  "GHC.Exts",
                  "GHC.List",
                  "GHC.Magic",
                  "GHC.Maybe",
                  "GHC.Num",
                  "GHC.Prim",
                  "GHC.Read",
                  "GHC.Show",
                  "GHC.Types",
                  "GHC.Enum",
                  "GHC.Real",
                  "System.IO",
                  "Data.String",
                  "GHC.Base"
                ]

findBestMatchingImport :: [ModuleName] -> ModuleName -> OccName -> Maybe ModuleName
findBestMatchingImport imports mn on
    | mn `elem` imports = Just mn
    | otherwise = Nothing

getMyHoogleOn :: [ModuleName] -> ModuleName -> OccName -> IO (Either ModuleName ModuleName)
getMyHoogleOn imports defaultMN on = do
    (_, stdout, _) <- flip readCreateProcessWithExitCode "" $ shell $ "hoogle --count=100 " <> oshow on
    -- (_, stdout, _) <- flip readCreateProcessWithExitCode "" $ shell $ "hoogle --count=100 " <> oshow on <> " | grep " <> oshow on

    let -- proposedMN              = handleLines Nothing defaultMN (oshow on) stdout
        proposedMNinImports = handleLines (Just imports) defaultMN (oshow on) stdout
    -- shortenedName           = tryShortenedModName proposedMN defaultMN

    marst <- hoogleModuleName (Just imports) (T.pack $ oshow defaultMN)
    mbrst <- hoogleModuleName Nothing (T.pack $ oshow defaultMN)

    return $ case () of
        _
            -- searching for matching imports to suggestion module name might show something
            | stdout == "No results found", Just mn <- marst -> Right mn
            -- searching for suggestion module name alone might show something
            | stdout == "No results found", Just mn <- mbrst -> Left mn -- brst imports defaultMN on defaultMN
            -- something in the first search matched the imports
            | not $ null proposedMNinImports -> Right $ head proposedMNinImports
            -- handleLines didn't turn anything interesting
            | Just crst <- mbrst -> Left crst
            -- nothing worked
            | otherwise -> Left defaultMN
            -- \| Just mn <- shortenedName                          -> brst imports defaultMN on mn


-- convert the output from hoogle
-- check all lines if the second or third word matches the search term
-- order
handleLines :: Maybe [ModuleName] -> ModuleName -> String -> String -> [ModuleName]
handleLines mimports defaultMN name s =
    map fst . sortOn snd . map (\i -> (i, levenshteinDistance defaultEditCosts (moduleNameString defaultMN) . oshow $ i))
        . ( case mimports of
                Nothing -> id
                Just imports -> filter (`elem` imports)
          )
        . map (mkModuleName . head . filter (`notElem` ["module", "class", "newtype"]) . words)
        . nub
        $ secondWordMatches ls <> thirdWordMatches ls
    where
        ls = filter (/= "") $ lines s
        nthWordMatches searchTerm n = filter (liftA2 (&&) ((>= n) . length) ((== searchTerm) . (!! (n - 1))) . words)
        secondWordMatches = nthWordMatches name 2
        thirdWordMatches = nthWordMatches name 3

hoogleModuleName :: Maybe [ModuleName] -> T.Text -> IO (Maybe ModuleName)
hoogleModuleName mimports goMN = do
    (_, stdout, _) <- flip readCreateProcessWithExitCode "" $ shell $ "hoogle --count=100 " <> T.unpack goMN

    let mnS = T.unpack goMN
        proposedMNinImports = handleLines mimports (mkModuleName mnS) mnS stdout
        goComponents = modname2components goMN

    case () of
        _
            | stdout == "No results found" -> return Nothing
            | not $ null proposedMNinImports -> return . Just $ head proposedMNinImports
            | init goComponents /= [] -> hoogleModuleName mimports (T.intercalate "." $ init goComponents)
            | otherwise -> return Nothing

-- tryShortenedModName :: [ModuleName] -> ModuleName -> Maybe ModuleName
-- tryShortenedModName imports mn =
--     fmap (mkModuleName . T.unpack) . foldr (go imports) (Just mnString) $ modname2components mnString
--     where
--         mnString = T.pack $ moduleNameString mn
--         go goImports _ (Just goMN)
--             | mkModuleName (T.unpack goMN) `elem` goImports = Just goMN
--             | init (modname2components goMN) /= [] = Just . T.intercalate "." . init $ modname2components goMN
--             | otherwise = Nothing
--         go _ _ Nothing = Nothing

ambiguousField2ProposedChanged :: GlobalRdrEnv -> [ModuleName] -> [ModuleName] -> AmbiguousFieldOcc GhcRn -> IO (SrcSpan, (RdrName, RdrName))
ambiguousField2ProposedChanged rdrEnv imports ours (Unambiguous n (L l rn))
    | Just mn <- moduleName <$> nameModule_maybe n, mn `elem` ours = return (l, (rn, Unqual $ mangle mn on))
    | Just mn <- moduleName <$> nameModule_maybe n = do
        case findBestMatchingImport imports mn on of
            Nothing -> return (l, (rn, Unqual on))
            Just temp -> case getMNFromEnv rdrEnv on of 
                Nothing -> return (l, (rn, Unqual on))
                Just temp -> return (l, (rn, Qual temp on))
    where
        -- this seems to not be used for now
        -- \| otherwise = Unambiguous n $ L l $ Unqual $ undefined
        on = rdrNameOcc rn
ambiguousField2ProposedChanged _ _ ours (Ambiguous _ (L l rn@(Qual mn on)))
    | mn `elem` ours = return (l, (rn, Unqual $ mangle mn on))
-- don't know what to do otherwise yet, let it crash for now
ambiguousField2ProposedChanged _ _ _ _ = error "ambiguousField2ProposedChanged: incomplete pattern match"

field2ProposedChange :: GlobalRdrEnv -> [ModuleName] -> [ModuleName] -> FieldOcc GhcRn -> IO (SrcSpan, (RdrName, RdrName))
field2ProposedChange rdrEnv imports ours (FieldOcc n (L l rn))
    | Just mn <- moduleName <$> nameModule_maybe n, mn `elem` ours = return (l, (rn, Unqual $ mangle mn on))
    | Just mn <- moduleName <$> nameModule_maybe n = do
        case findBestMatchingImport imports mn on of
            Nothing -> return (l, (rn, Unqual on))
            Just temp -> case getMNFromEnv rdrEnv on of 
                Nothing -> return (l, (rn, Unqual on))
                Just temp -> return (l, (rn, Qual temp on))

        -- temp <- fromRight mn <$> findBestMatchingImport imports mn on
        -- return (l, (rn, Qual temp on))
    where
        on = rdrNameOcc rn
field2ProposedChange _ _ _ _ = error "field2ProposedChange: incomplete pattern match"

applyChange :: M.Map SrcSpan RdrName -> Located RdrName -> Located RdrName
applyChange m (L l r) = L l $ fromMaybe r (M.lookup l m)

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
unqualFamEqn fe@FamEqn {}
    | isQual (T.pack . oshow $ unLoc $ feqn_tycon fe) = fe {feqn_tycon = unqualName <$> feqn_tycon fe}
unqualFamEqn f = f

unqualSig :: Sig GhcPs -> Sig GhcPs
unqualSig (TypeSig x names t) = TypeSig x (map (fmap unqualName) names) t
unqualSig (ClassOpSig x b names t) = ClassOpSig x b (map (fmap unqualName) names) t
unqualSig s = s

unqualBinds :: HsBindLR GhcPs GhcPs -> HsBindLR GhcPs GhcPs
unqualBinds fb@(FunBind _ (L l n) fm _ _)
    | isQual (T.pack $ oshow n) = newFB
    | otherwise = fb
    where
        newFM = transformBi unqualMatchCtxt fm
        newFB = fb {fun_id = L l $ unqualName n, fun_matches = newFM}
unqualBinds hb = hb

unqualMatchCtxt :: HsMatchContext RdrName -> HsMatchContext RdrName
unqualMatchCtxt = fmap unqualName

unqualName :: RdrName -> RdrName
unqualName (Qual _ on) = Unqual on
unqualName n = n

--  | make all imports qualified, hiding nothing, no aliasing and no safe imports
qualImports :: [LImportDecl p] -> [LImportDecl p]
qualImports =
    map
        ( \(L l i) ->
              L l $
                  i
                      { ideclQualified = True,
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

renameOperator :: String -> String
renameOperator = evalRand randomOpString . mkStdGen . hash

-- | create a random operator string
randomOpString :: MonadRandom m => m String
randomOpString = do
    s1 <- replicateM 4 $ do
        i <- getRandomR (0, length operatorSymbols - 1)
        return $ operatorSymbols !! i

    b <- getRandom

    s2 <- if b then randomOpString else return ""

    return $ "<" <> s1 <> s2 <> ">"
    where
        operatorSymbols = "!#$%&*+<>?@^~"

-- ***************************************************************************
-- CABAL FILE UTILITIES

-- ***************************************************************************

getAllPragmas :: [FilePath] -> IO [Pragma]
getAllPragmas =
    fmap concat . mapM (getPragmas . (\f -> fromMaybe (error $ "could not parse path as absolute file: " <> f) $ parseAbsFile f))

-- ***************************************************************************
-- CLEANING UP

-- ***************************************************************************

fastCleanUp :: Tool -> Path Abs File -> IO ()
fastCleanUp tool sourcePath = do
    cleanUp tool Indent sourcePath
    -- cleanUp tool PerhapsYouMeant sourcePath
    -- cleanUp tool Indent sourcePath

    --    cleanUp tool MissingImport sourcePath -- add imports
    --    cleanUp tool HiddenImport sourcePath  -- clean up imports
    --    cleanUp tool NotInScope sourcePath       -- clean up uses of hidden modules

allowedToLoop :: [GhcMode]
allowedToLoop = [Indent]

cleanUp :: Tool -> GhcMode -> Path Abs File -> IO ()
cleanUp tool mode sourcePath = do
    fileContent <- TIO.readFile (fromAbsFile sourcePath)

    rstate <- case mode of
        PerhapsYouMeant -> Just <$> parse True [] [] sourcePath
        _ -> return Nothing

    getGhcOutput tool mode sourcePath >>= \case
        Nothing -> return ()
        Just [] -> return ()
        Just mySpans -> do
            banner (show mode)

            let rightSpans =
                    (if mode == Indent then id else filter ((/= "") . fst)) $ mySpans
                newFileContent = case mode of
                    Indent -> foldr (insertIndent . fmap span2Locs) fileContent rightSpans
                    MissingImport ->
                        let contentLines = T.lines fileContent
                            prefix = T.unlines $ takeWhile (not . ("import qualified" `T.isPrefixOf`)) contentLines
                            suffix = T.unlines $ dropWhile (not . ("import qualified" `T.isPrefixOf`)) contentLines
                         in prefix
                                <> (T.unlines . map (("import qualified " <>)) . nub . map fst $ rightSpans)
                                <> suffix
                    HiddenImport ->
                        T.unlines
                            . zipWith
                                ( \i l ->
                                      let myLines = undefined -- map (fmap (srcLocLine . fst . span2Locs)) rightSpans
                                       in if (i :: Integer) `elem` map snd myLines && "import qualified" `T.isPrefixOf` l
                                              then
                                                  let wl = T.words l
                                                      modName = fst . head $ filter ((== i) . snd) myLines
                                                   in traceShow ("Changing " <> last wl <> " to " <> modName <> " at line " <> T.pack (show i))
                                                          . T.unwords
                                                          $ init wl <> [modName]
                                              else l
                                )
                                [1 ..]
                            . T.lines
                            $ fileContent
                    PerhapsYouMeant ->
                        maybe
                            fileContent
                            ( \state ->
                                  applySuggestions state mySpans
                            )
                            $ traceShow (show $ isJust rstate) rstate
                    _ -> foldr (replaceWithGhcOutput . fmap span2Locs) fileContent rightSpans

            TIO.writeFile (fromAbsFile sourcePath) newFileContent
            when (mode `elem` allowedToLoop) $ cleanUp tool mode sourcePath

replaceWithGhcOutput :: (T.Text, (Int, Int, Int, Int)) -> T.Text -> T.Text
replaceWithGhcOutput (newName, (lineStart, colStart, _, colEnd)) fileContent =
    T.unlines $
        prevLines
            ++ [ traceShow
                     ( "changing at line "
                           <> show (currentIndex + 1)
                           <> ": "
                           <> show oldName
                           <> " -> "
                           <> show realNewName
                     )
                     newLineContent
               ]
            ++ succLines
    where
        contentLines = T.lines fileContent
        currentIndex = lineStart -1
        prevLines = take currentIndex contentLines
        succLines = drop lineStart contentLines
        currentLine = contentLines !! currentIndex
        oldName = T.take (colEnd - colStart) $ T.drop (colStart - 1) currentLine
        prefix = T.take (colStart -1) currentLine
        suffix = T.drop (colEnd -1) currentLine
        isInfix = (== 2) . T.length . T.filter (== '`') . T.drop (colStart -1) . T.take (colEnd -1) $ currentLine
        realNewName =
            if "Internal" `elem` T.words oldName
                then removeInternal id oldName
                else newName
        newLineContent =
            if isInfix
                then prefix <> "`" <> realNewName <> "`" <> suffix
                else prefix <> realNewName <> suffix

insertIndent :: (T.Text, (Int, Int, Int, Int)) -> T.Text -> T.Text
insertIndent (_, (lineStart, _, _, _)) fileContent =
    T.unlines $ prevLines ++ [traceShow ("inserting indent at line " <> show (currentIndex + 1)) newLineContent] ++ succLines
    where
        contentLines = T.lines fileContent
        currentIndex = lineStart -1
        prevLines = take currentIndex contentLines
        succLines = drop lineStart contentLines
        currentLine = contentLines !! currentIndex
        newLineContent = "  " <> currentLine

span2Locs :: RealSrcSpan -> (Int, Int, Int, Int)
span2Locs s = (srcLocLine start, srcLocCol start, srcLocLine end, srcLocCol end)
  where 
      start = realSrcSpanStart s
      end = realSrcSpanEnd s

applySuggestions :: RState -> [(T.Text, RealSrcSpan)] -> T.Text
applySuggestions rstate (map (\(a, b) -> (span2Locs b, a)) -> suggestions) = 
    showState $ rstate & parsed %~ transformBi (apply suggestions)

apply :: [((Int, Int, Int, Int), T.Text)] -> Located RdrName -> Located RdrName
apply suggestions n@(L l (Unqual on))
    -- | not . isQual . T.pack $ oshow n, not . null $ filter f suggestions =
    --      mkSystemName un (mkOccName ns $ T.unpack $ head $ filter f suggestions )
    | Just result <- (lookup (span2Locs $ (\(RealSrcSpan l1) -> l1) l) suggestions) = traceShow result $ traceShow (oshow on) $ L l $ Qual (mkModuleName $ T.unpack . T.intercalate "." . init $ T.splitOn "." result) on
    | True = n
apply _ n = n
    -- where
    --     on = occName n
    --     ns = occNameSpace on
    --     un = nameUnique n
    --     f  = T.isInfixOf (T.pack (oshow n)) 


instance Eq (ImportDecl GhcPs) where
    i1 == i2 = oshow i1 == oshow i2

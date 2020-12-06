module Merge.Merge (hsmerge) where

import Debug.Trace
import Text.EditDistance
import qualified Data.Map as M
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad
import qualified Data.List.NonEmpty as NE
import Control.Monad.Random 
import Data.Generics.Uniplate.Data 
import Data.Hashable (hash)
import Data.List
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Traversable (for)
import Data.Void (Void)
import Debug.Trace (traceShow)
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

-- namesToWatch = ["IntersectionOf", "Dim", "Index", "IxValue", "~", "VertexId"]
namesToWatch :: [String]
namesToWatch = ["number"]
-- namesToWatch = ["try", "Seq", "fromDistinctAscList", "number"]
-- namesToWatch = ["deriveJSON", "defaultOptions", "D"]
-- namesToWatch = ["Iso", "iso", "~", "HasDataOf", "ToJSON", "Object", "valueConName"]

hsmerge :: FilePath -> IO ()
hsmerge filePath = do
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

                liftIO $ print $ map oshow $ ours

                moduleName2imports <- liftIO $ newIORef M.empty

                importsAndannASTs <- for modSums $ \modSum -> do
                    t <- typecheckModule =<< parseModule modSum
                    dynFlags <- getDynFlags

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
                        (renamedGroups, _, _, _) = fromMaybe (error "HsAllInOne->hsmerge: no renamed source!") $ tm_renamed_source t


                    firstGroups <- liftIO $ transformBiM (renameName moduleName2imports rdrEnv ours myMN) renamedGroups

                    let newGroups =

                            transformBi unqualSig
                                . transformBi unqualFamEqnClsInst
                                . transformBi unqualBinds
                                -- . transformBi (\n -> let newN = renameName rdrEnv ours myMN n 
                                --                      in (if oshow n `elem` namesToWatch 
                                --                          then 
                                --                              traceShow @String ""
                                --                              . traceShow @String "afterRename"
                                --                              . traceShow (oshow newN) 
                                --                              . traceShow (isExternalName newN) 
                                --                          else id ) newN)
                                . transformBi (renameField rdrEnv ours myMN)
                                . transformBi (renameAmbiguousField rdrEnv ours myMN)
                                $ firstGroups

                    let myImports = map (unLoc . fmap (unLoc . ideclName)) . hsmodImports . unLoc . pm_parsed_source $ tm_parsed_module t

                    liftIO $ writeIORef moduleName2imports . M.insert (moduleName myMN) myImports =<< readIORef moduleName2imports



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

renameName :: IORef (M.Map ModuleName [ModuleName]) -> GlobalRdrEnv -> [ModuleName] -> Module -> Name -> IO Name
renameName ref rdrEnv ours myMN@(Module unitId _) n
    | ( if oshow n `elem` namesToWatch
            then
                traceShow @String ""
                    . traceShow @String "renameName"
                    . traceShow ("name: " <> oshow n)
                    . traceShow ("my module: " <> oshow myMN)
                    . traceShow ("namestableString " <> nameStableString n)
                    . traceShow ("GRE lookup: " <> oshow (lookupGRE_Name rdrEnv n))
                    . traceShow ("is local? " <> oshow (gre_lcl <$> lookupGRE_Name rdrEnv n))
                    . traceShow ("nameModule: " <> oshow (nameModule_maybe n))
                    . traceShow ("getModuleName: " <> oshow (getModuleName rdrEnv ours myMN n))
                    . traceShow (show $ isSystemName n)
                    . traceShow (show $ isBuiltInSyntax n)
                    . traceShow (show $ isWiredInName n)
            else id
      )
          False =
        pure n

    | oshow n == "main" = pure n
    | oshow n == "~" || oshow n == "~" = pure n

    -- -- built-in things
    | isBuiltInSyntax n = pure n
    -- our operator / function / variable
    | otherwise = do
        getModuleNameM ref rdrEnv ours myMN n >>= \case
            Just mn -> 
                if mn `elem` ours
                    then pure $ mkInternalName u (mangle mn on) noSrcSpan
                    else pure $ mkExternalName u (mkModule unitId mn) on noSrcSpan
            _ -> pure n
    where
        u = nameUnique n
        on = occName n

getModuleNameM :: IORef (M.Map ModuleName [ModuleName]) -> GlobalRdrEnv -> [ModuleName] -> Module -> Name -> IO (Maybe ModuleName)
getModuleNameM ref env ours myMN n
    | (if oshow n `elem` namesToWatch then traceShow ("header getModuleNameM: " <> oshow n) else id) $ False = undefined
    | (gre_lcl <$> rdrElt) == Just True = do

        when (oshow n `elem` namesToWatch) $ do
            print "gre_lcl, getModuleNameM:"
            print $ oshow myMN
        pure . Just $ moduleName myMN
    | otherwise = do
        let mModuleNames = do
                imports <- gre_imp <$> rdrElt
                importMN <- is_mod . is_decl <$> (listToMaybe imports)
                exactMN <- moduleName <$> nameModule_maybe n
                Just (importMN, exactMN)

        when (oshow n `elem` namesToWatch) $ do
            print "otherwise, getModuleNameM:"
            print $ oshow mModuleNames
        case (if oshow n `elem` namesToWatch then traceShow (oshow mModuleNames) mModuleNames else mModuleNames) of
            Just (importMN, exactMN) -> do
                -- case 1: the importing module is one of ours and it's re-exporting an external module
                -- case 2: the importing module is one of ours and it's re-exporting one of our modules => should we recurse then?
                if importMN `elem` ours && importMN /= exactMN
                    then do
                        readIORef ref >>= pure . M.lookup importMN . (\e -> if oshow n `elem` namesToWatch then traceShow (oshow e) e else e) >>= \case
                            -- we should always have a list here
                            Just imports -> do
                                case listToMaybe $ sortOn snd $ map (\i -> (i, levenshteinDistance defaultEditCosts (oshow exactMN) $ oshow i)) imports of
                                    Just (i, _) -> pure $ Just i
                                    Nothing -> pure $ Just exactMN
                            _ -> pure $ Just exactMN
                    else pure $ Just importMN
            _ -> pure Nothing
    where
        rdrElt = lookupGRE_Name env n

getModuleName :: GlobalRdrEnv -> [ModuleName] -> Module -> Name -> Maybe ModuleName
getModuleName env ours myMN n
    -- \| (if oshow n `elem` namesToWatch then traceShow () else id) $ False = myMN
    | (gre_lcl <$> rdrElt) == Just True = Just $ moduleName myMN
    | otherwise = do
        imports <- gre_imp <$> rdrElt
        importMN <- is_mod . is_decl <$> (listToMaybe imports)
        exactMN <- moduleName <$> nameModule_maybe n
        if importMN `elem` ours && importMN /= exactMN
            then pure exactMN
            else pure importMN
    where
        rdrElt = lookupGRE_Name env n        

getModuleNameFromRdrName :: GlobalRdrEnv -> Module -> RdrName -> Maybe ModuleName
getModuleNameFromRdrName env mn n
    | (gre_lcl <$> rdrElt) == Just True = Just $ moduleName mn
    | otherwise = do
        imports <- gre_imp <$> rdrElt
        is_mod . is_decl <$> (listToMaybe imports)
    where
        rdrElt = listToMaybe $ lookupGRE_RdrName n env

renameAmbiguousField :: GlobalRdrEnv -> [ModuleName] -> Module -> AmbiguousFieldOcc GhcRn -> AmbiguousFieldOcc GhcRn
renameAmbiguousField rdrEnv ours myMN@(Module unitId _) f@(Unambiguous n (L l rn))
--     | ( if (oshow n `elem` ["HasDataOf", "_unV"])
--             then
--                 traceShow @String ""
--                     . traceShow @String "renameField"
--                     . traceShow ("field: " <> gshow f)
--                     . traceShow ("name: " <> oshow n)
--                     . traceShow ("my module: " <> oshow myMN)
--                     . traceShow ("namestableString " <> nameStableString n)
--                     . traceShow ("GRE lookup: " <> oshow (lookupGRE_Name rdrEnv n))
--                     . traceShow ("is local? " <> oshow (gre_lcl <$> lookupGRE_Name rdrEnv n))
--                     . traceShow ("nameModule: " <> oshow (nameModule_maybe n))
--                     . traceShow ("getModuleName: " <> oshow (getModuleName rdrEnv ours myMN n))
--                     . traceShow ("rdrName: " <> oshow (rn))
--             else id
--       )
--           False = f
    -- internal
    | Just mn <- getModuleName rdrEnv ours myMN n,
      mn `elem` ours =
        Unambiguous (mkInternalName u (mangle mn on) noSrcSpan) . L l . Unqual $ mangle mn on
    -- external
    | Just mn <- getModuleName rdrEnv ours myMN n =
        Unambiguous (mkExternalName u (mkModule unitId mn) on noSrcSpan) . L l $ Qual mn on
    -- internal
    | Qual mn _ <- rn,
      mn `elem` ours =
        Unambiguous (mkInternalName u (mangle mn on) noSrcSpan) . L l . Unqual $ mangle mn on
    | otherwise = f
    where
        u = nameUnique n
        on = rdrNameOcc rn
renameAmbiguousField rdrEnv ours myMN f@(Ambiguous _ (L l rn))
    | Just mn <- getModuleNameFromRdrName rdrEnv myMN rn, mn `elem` ours = Ambiguous NoExt . L l . Unqual $ mangle mn on
    | Just mn <- getModuleNameFromRdrName rdrEnv myMN rn = Ambiguous NoExt . L l $ Qual mn on
    | Qual mn _ <- rn, mn `elem` ours = Ambiguous NoExt . L l . Unqual $ mangle mn on
    | otherwise = f
    where
        on = rdrNameOcc rn
renameAmbiguousField _ _ _ f = f

renameField :: GlobalRdrEnv -> [ModuleName] -> Module -> FieldOcc GhcRn -> FieldOcc GhcRn
renameField rdrEnv ours myMN@(Module unitId _) (FieldOcc n (L l rn))
--     | ( if (oshow n `elem` ["HasDataOf", "_unV"])
--             then
--                 traceShow @String ""
--                     . traceShow @String "renameField"
--                     . traceShow ("name: " <> oshow n)
--                     . traceShow ("my module: " <> oshow myMN)
--                     . traceShow ("namestableString " <> nameStableString n)
--                     . traceShow ("GRE lookup: " <> oshow (lookupGRE_Name rdrEnv n))
--                     . traceShow ("is local? " <> oshow (gre_lcl <$> lookupGRE_Name rdrEnv n))
--                     . traceShow ("nameModule: " <> oshow (nameModule_maybe n))
--                     . traceShow ("getModuleName: " <> oshow (getModuleName rdrEnv ours myMN n))
--                     . traceShow ("rdrName: " <> oshow (rn))
--             else id
--       )
--           False = f
        
    | Just mn <- getModuleName rdrEnv ours myMN n, mn `elem` ours = FieldOcc (mkInternalName u (mangle mn on) noSrcSpan) . L l . Unqual $ mangle mn on
    | Just mn <- getModuleName rdrEnv ours myMN n = FieldOcc (mkExternalName u (mkModule unitId mn) on noSrcSpan) . L l $ Qual mn on
    where
        u = nameUnique n
        on = rdrNameOcc rn
renameField _ _ _ f = f

-- ***************************************************************************
-- MERGING MODULES UTILITIES

-- ***************************************************************************
-- only unqual names associated type families!
unqualFamEqnClsInst :: p ~ GhcRn => ClsInstDecl p -> ClsInstDecl p
unqualFamEqnClsInst = transformBi unqualFamEqn

unqualFamEqn :: p ~ GhcRn => FamEqn p (HsTyPats p) (LHsType p) -> FamEqn p (HsTyPats p) (LHsType p)
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
    | isSymOcc on, Just os <- mOS = mkOccName ns $ NE.head os : renameOperator (NE.toList os <> filter (/= '.') (moduleNameString mn))
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
    s1 <- replicateM 2 $ do
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
    fmap concat . mapM (getPragmas <=< resolveFile')

-- ***************************************************************************
-- CLEANING UP
-- ***************************************************************************

allowedToLoop :: [GhcMode]
allowedToLoop = [Indent]

cleanUp :: Tool -> GhcMode -> Path Abs File -> IO ()
cleanUp tool mode sourcePath = do
    fileContent <- TIO.readFile (fromAbsFile sourcePath)

    getGhcOutput mode sourcePath >>= \case
        Nothing -> return ()
        Just [] -> return ()
        Just mySpans -> do
            banner (show mode)

            let rightSpans =
                    (if mode == Indent then id else filter ((/= "") . fst)) $ mySpans
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
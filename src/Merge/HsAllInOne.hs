module HsAllInOne where

import Data.Bifunctor
import Control.Monad.Random (MonadRandom, evalRand, getRandom, getRandomR, mkStdGen, replicateM, void, when)
import Data.Generics.Uniplate.Data
import Data.Hashable (hash)
import Text.EditDistance
import Data.List (isInfixOf, nub, sortOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Traversable (for)
import Data.Void (Void)
import Debug.Trace (traceShow)
import Digraph (flattenSCC)
import GHC hiding (GhcMode, extensions)
import GHC.Paths (libdir)
import GhcPlugins hiding ((<&&>), (<>), GhcMode, count, extensions, getHscEnv, isQual, mkUnqual, qualName)
import HIE.Bios
import Parser (getPragmas)
import Path ((</>), Abs, File, Path, fromAbsFile, parseAbsDir, parseAbsFile, parseRelFile)
import System.Directory (getCurrentDirectory)
import TcRnTypes (tcg_rdr_env)
import Types
import Util

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


                missingImportsAndAnnotatedAST <- for modSums $ \modSum -> do
                    t <- typecheckModule =<< parseModule modSum

                    let rdrEnv = tcg_rdr_env . fst $ tm_internals_ t
                        myParsedSource = unLoc . pm_parsed_source $ tm_parsed_module t
                        importsModuleNames = mkModuleName "Prelude" : (map (unLoc . ideclName . unLoc) $ hsmodImports myParsedSource)
                        (renamedGroups, _, _, _) = fromMaybe (error "HsAllInOne->hsmerge: no renamed source!") $ tm_renamed_source t

                        proposedNameChanges = flip map (universeBi renamedGroups :: [Located Name]) $ \(L l n) ->
                            let temp = name2ProposedChange rdrEnv importsModuleNames ours n
                            in  (l,) $ temp
                            -- in (if "Interval" `isInfixOf` oshow n then traceShow (oshow temp) else id) $ (l,) $ temp
                        otherNames = map (fmap snd) proposedNameChanges
                        ambiguousFields = map (fmap snd) $ flip map (universeBi renamedGroups) (ambiguousField2ProposedChanged rdrEnv importsModuleNames ours)
                        fields = map (fmap snd) $ flip map (universeBi renamedGroups) (field2ProposedChange rdrEnv importsModuleNames ours)
                        proposedChanges = M.fromList $ otherNames <> ambiguousFields <> fields
                        rdr2NewRdr = M.fromList $ map (first (occNameString . rdrNameOcc)) $ map snd $ proposedNameChanges


                    return . 
                        transformBi unqualFamEqnClsInst
                            . transformBi unqualSig
                            . transformBi unqualBinds
                            . transformBi (qualSplices rdrEnv importsModuleNames ours rdr2NewRdr)
                            . transformBi (applyChange proposedChanges)
                            $ myParsedSource

                d <- liftIO getCurrentDirectory
                let crd = cradleRootDir cradle
                    root = if crd == "." then d else crd

                extensions <-
                    liftIO
                        . fmap (unlines . map show . nub)
                        . getAllPragmas
                        . mapMaybe (fmap (\f -> if f == filePath then root <> "/" <> f else f) . ml_hs_file . ms_location)
                        $ modSums

                let annASTs = missingImportsAndAnnotatedAST
                    modName = Just . noLoc $ mkModuleName "AllInOne"
                    imports = nub $ concatMap (qualImports . removeImports ours . hsmodImports) annASTs
                    decls = concatMap hsmodDecls annASTs
                    mergedMod = HsModule modName Nothing imports decls Nothing Nothing

                liftIO . writeFile "AllInOne.hs" $ unlines [extensions, oshow mergedMod]

            putStrLn "cleaning up"
            dir <- parseAbsDir =<< getCurrentDirectory
            f <- parseRelFile "AllInOne.hs"
            cleanUp Ghc Indent (dir </> f)
        CradleFail err -> error $ show err
        _ -> error "Cradle wasn't loaded successfully! Maybe you're missing a hie.yaml file?"

renameRdrName :: GlobalRdrEnv -> [ModuleName] -> [ModuleName] -> M.Map String RdrName -> RdrName -> RdrName
renameRdrName rdrEnv imports ours rdr2NewRdr rn@(Unqual on) = 
    case getNameFromEnv rdrEnv imports ours (mkModuleName "") on of
        Unqual _ -> 
            -- (if "Interval" `isInfixOf` oshow on then traceShow "\nhey" . traceShow (oshow (Unqual $ mkOccName ns $ dropWhile (=='\'') os)) . traceShow (oshow $ rdr2NewRdr) . traceShow (oshow $ M.lookup (Unqual $ mkOccName ns $ dropWhile (=='\'') os) rdr2NewRdr) else id) $ 
                fromMaybe rn $ M.lookup (dropWhile (=='\'') os) rdr2NewRdr
        n -> n
  where 
      ns = occNameSpace on
      os = occNameString on
renameRdrName rdrEnv imports ours _ (Qual mn on) = getNameFromEnv rdrEnv imports ours mn on
renameRdrName _ _ _ _ n = n

qualSplices :: GlobalRdrEnv -> [ModuleName] -> [ModuleName] -> M.Map String RdrName -> HsSplice GhcPs -> HsSplice GhcPs
qualSplices rdrEnv imports ours rdr2NewRdr = transformBi (renameRdrName rdrEnv imports ours rdr2NewRdr)

-- run this in the maybe monad
getNameFromEnv :: GlobalRdrEnv -> [ModuleName] -> [ModuleName] -> ModuleName -> OccName -> RdrName
getNameFromEnv rdrEnv imports ours mn on =
    if mn `elem` imports
        then Qual mn on
        else case moduleNamesFromEnv of
            [] -> Unqual on
            (newMN : _) -> 
                 -- (if "element" `isInfixOf` oshow on 
                 --    then traceShow "getNameFromEnv" . traceShow (oshow on) . traceShow (oshow mn) . traceShow (oshow newMN) 
                 --    else id) $ 
                Qual (if newMN `notElem` ours then newMN else mn) on
    where
        moduleNamesFromEnv =
            sortOn (levenshteinDistance defaultEditCosts (oshow mn) . oshow) [ importMN
              | rdrElement <- lookupGlobalRdrEnv rdrEnv on,
                possibleImport <- gre_imp rdrElement,
                let importMN = is_mod $ is_decl possibleImport
            ]

name2ProposedChange :: GlobalRdrEnv -> [ModuleName] -> [ModuleName] -> Name -> (RdrName, RdrName)
name2ProposedChange rdrEnv imports ours n
    | showSDocUnsafe (pprNameUnqualified n) == "main" = (rn, rn)
    -- built-in things
    | isSystemName n = (rn, rn)
    | isBuiltInSyntax n = (rn, rn)
    -- our operator
    | Just mn <- getModuleName n,
      isOperator . oshow $ getRdrName n,
      mn `elem` ours =
        (rn, Unqual . mkOccName ns $ head os : renameOperator (os ++ filter (/= '.') (moduleNameString mn)))
    -- our function / variable
    | Just mn <- getModuleName n,
      mn `elem` ours =
        (rn, Unqual $ mangle mn on)
    -- something external
    | Just mn <- getModuleName n = do
        (rn,) $ getNameFromEnv rdrEnv imports ours mn on
    -- if mn `elem` imports
    -- then
    --     return $ ChangeName (rn, Qual mn on)
    -- else
    --     case getNameFromEnv rdrEnv mn on of
    --         [] -> return $ ChangeName (rn, Unqual on)
    --         namesFromGlobalEnv ->
    --             let newMN = head namesFromGlobalEnv
    --             in if newMN `elem` ours
    --                 then return $ AddImportChangeName mn (rn, Qual mn on)
    --                 else return $ ChangeName (rn, Qual (head namesFromGlobalEnv) on)
    | otherwise = (rn, rn)
    where
        on = occName n
        rn = getRdrName n
        ns = occNameSpace on
        os = occNameString on

ambiguousField2ProposedChanged :: GlobalRdrEnv -> [ModuleName] -> [ModuleName] -> AmbiguousFieldOcc GhcRn -> (SrcSpan, (RdrName, RdrName))
ambiguousField2ProposedChanged rdrEnv imports ours (Unambiguous n (L l rn))
    | Just mn <- getModuleName n, mn `elem` ours = (l, (rn, Unqual $ mangle mn on))
    | Just mn <- getModuleName n = do
        (l,) . (rn,) $ getNameFromEnv rdrEnv imports ours mn on
    where
        on = rdrNameOcc rn
ambiguousField2ProposedChanged _ _ ours (Ambiguous _ (L l rn@(Qual mn on)))
    | mn `elem` ours = (l, (rn, Unqual $ mangle mn on))
-- don't know what to do otherwise yet, let it crash for now
ambiguousField2ProposedChanged _ _ _ _ = error "ambiguousField2ProposedChanged: incomplete pattern match"

field2ProposedChange :: GlobalRdrEnv -> [ModuleName] -> [ModuleName] -> FieldOcc GhcRn -> (SrcSpan, (RdrName, RdrName))
field2ProposedChange rdrEnv imports ours (FieldOcc n (L l rn))
    | Just mn <- getModuleName n, mn `elem` ours = (l, (rn, Unqual $ mangle mn on))
    | Just mn <- getModuleName n = do
        (l,) . (rn,) $ getNameFromEnv rdrEnv imports ours mn on
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
unqualFamEqnClsInst :: ClsInstDecl GhcPs -> ClsInstDecl GhcPs
unqualFamEqnClsInst = transformBi unqualFamEqn

unqualFamEqn :: FamEqn GhcPs (HsTyPats GhcPs) (LHsType GhcPs) -> FamEqn GhcPs (HsTyPats GhcPs) (LHsType GhcPs)
unqualFamEqn fe@FamEqn {} =
    case feqn_tycon fe of
        L l (Qual _ on) -> fe {feqn_tycon = L l $ Unqual on}
        _ -> fe
unqualFamEqn f = f

unqualSig :: Sig GhcPs -> Sig GhcPs
unqualSig (TypeSig x names t) = TypeSig x (map (fmap unqualName) names) t
unqualSig (ClassOpSig x b names t) = ClassOpSig x b (map (fmap unqualName) names) t
unqualSig s = s

unqualBinds :: HsBindLR GhcPs GhcPs -> HsBindLR GhcPs GhcPs
unqualBinds fb@(FunBind _ (L l n) fm _ _) =
    case fun_id fb of
        L _ (Qual _ _) -> newFB
        _ -> fb
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

            let rightSpans =
                    (if mode == Indent then id else filter ((/= "") . fst)) $ mySpans
                newFileContent = case mode of
                    Indent -> foldr (insertIndent . fmap span2Locs) fileContent rightSpans
                    _ -> fileContent

            TIO.writeFile (fromAbsFile sourcePath) newFileContent
            when (mode `elem` allowedToLoop) $ cleanUp tool mode sourcePath

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

instance Eq (ImportDecl GhcPs) where
    i1 == i2 = oshow i1 == oshow i2
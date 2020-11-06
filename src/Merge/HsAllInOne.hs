module Merge.HsAllInOne (hsmerge) where

import qualified Data.List.NonEmpty as NE
import Control.Monad.Random (MonadRandom, evalRand, getRandom, getRandomR, mkStdGen, replicateM, void)
import Data.Generics.Uniplate.Data (transformBi)
import Data.Hashable (hash)
import Data.List (nub)
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
import Parser.Parser (getPragmas)
import Path 
import Path.IO (getCurrentDir)
import TcRnTypes (tcg_rdr_env)
import Util.Types
import Util.Util

-- namesToWatch = ["IntersectionOf", "Dim", "Index", "IxValue", "~", "VertexId"]
namesToWatch :: [String]
namesToWatch = ["deriveJSON", "defaultOptions", "D"]
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
                        newGroups =
                            transformBi unqualSig
                                . transformBi unqualFamEqnClsInst
                                . transformBi unqualBinds
                                -- . transformBi (renameName rdrEnv ours myMN)
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
                                $ renamedGroups

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
                        <> [ "module AllInOne where",
                             "import qualified Prelude"
                           ]
                        <> map oshow imports
                        <> [decls]

            -- putStrLn "cleaning up"
            -- dir <- getCurrentDir
            -- f <- parseRelFile "AllInOne.hs"
            -- cleanUp Ghc Indent (dir </> f)
            -- cleanUp Ghc PerhapsYouMeant (dir </> f)
        CradleFail err -> error $ show err
        _ -> error "Cradle wasn't loaded successfully! Maybe you're missing a hie.yaml file?"

renameName :: GlobalRdrEnv -> [ModuleName] -> Module -> Name -> Name
renameName rdrEnv ours myMN@(Module unitId _) n
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
        n

    | oshow n == "main" = n
    | oshow n == "~" || oshow n == "~" = n

    -- -- built-in things
    | isBuiltInSyntax n = n
    -- our operator / function / variable
    | Just mn <- getModuleName rdrEnv ours myMN n,
      mn `elem` ours =
        mkInternalName u (mangle mn on) noSrcSpan
    -- something external
    | Just mn <- getModuleName rdrEnv ours myMN n =
        -- using my unit id shouldn't be a problem because
        -- we're just printing these names and not checking later from which module the names come
        -- mkExternalName u (mkModule unitId mn) on noSrcSpan
        (if oshow n `elem` namesToWatch then traceShow @String "" . traceShow @String "mkExternalName" . traceShow (oshow $ mkModule unitId mn) . traceShow (oshow on) else id) $ mkExternalName u (mkModule unitId mn) on noSrcSpan
    | otherwise = n
    where
        u = nameUnique n
        on = occName n

getModuleName :: GlobalRdrEnv -> [ModuleName] -> Module -> Name -> Maybe ModuleName
getModuleName env ours myMN n
    -- | (if oshow n `elem` namesToWatch then traceShow () else id) $ False = myMN
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
    fmap concat . mapM (getPragmas . (\f -> fromMaybe (error $ "could not parse path as absolute file: " <> f) $ parseAbsFile f))

-- ***************************************************************************
-- CLEANING UP

-- ***************************************************************************

instance {-# OVERLAPPABLE #-} Eq (ImportDecl GhcPs) where
    i1 == i2 = oshow i1 == oshow i2
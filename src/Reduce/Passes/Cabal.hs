module Reduce.Passes.Cabal where

import Control.Concurrent.STM.Lifted
import Control.Monad.Reader
import Distribution.ModuleName
import Distribution.PackageDescription
import Util.Types
import Util.Util

cabalActions :: [R IO ()]
cabalActions =
    -- [ rmvBenchmarks,
    --   rmvTestSuites,
    --   rmvExtraSrcFiles,
    [ runPass rmvModuleNames
    ]

-- rmvModuleNames :: R IO ()
-- rmvModuleNames = do
--     conf <- ask
--     readTVarIO (_tState conf) >>= \case
--         s@CabalState{..} -> do
--             let p = packageDescription _pkgDesc
--             tryNewState "Cabal:rmvLibrary" $ const (s { _pkgDesc = transformBi f _pkgDesc })
--         _ -> return ()
--
--     where
rmvModuleNames :: Pass
rmvModuleNames = mkCabalPass "rmvModuleNames" f
    where
        f :: WaysToChange [ModuleName]
        f = map (\mn modNames -> filter (/= mn) modNames)

rmvLibrary :: R IO ()
rmvLibrary = do
    conf <- ask
    readTVarIO (_tState conf) >>= \case
        s@CabalState {..} -> do
            let p = packageDescription _pkgDesc
            tryNewState "Cabal:rmvLibrary" $ const (s {_pkgDesc = _pkgDesc {packageDescription = p {library = Nothing}}})
        _ -> return ()

rmvExtraSrcFiles :: R IO ()
rmvExtraSrcFiles = do
    conf <- ask
    readTVarIO (_tState conf) >>= \case
        s@CabalState {..} -> do
            let p = packageDescription _pkgDesc
            tryNewState "Cabal:rmvExtraSrcFiles" $ const (s {_pkgDesc = _pkgDesc {packageDescription = p {extraSrcFiles = []}}})
        _ -> return ()

rmvBenchmarks :: R IO ()
rmvBenchmarks = do
    conf <- ask
    readTVarIO (_tState conf) >>= \case
        s@CabalState {..} -> do
            tryNewState "Cabal:rmvBenchmarks" $ const (s {_pkgDesc = _pkgDesc {condBenchmarks = []}})
        _ -> return ()

rmvTestSuites :: R IO ()
rmvTestSuites = do
    conf <- ask
    readTVarIO (_tState conf) >>= \case
        s@CabalState {..} -> do
            tryNewState "Cabal:rmvTestSuites" $ const (s {_pkgDesc = _pkgDesc {condTestSuites = []}})
        _ -> return ()

module Passes.RemoveUnused.Pragmas where

import Ormolu.Parser.Result as OPR (ParseResult, prParsedSource, prExtensions)
import Ormolu.Parser.Pragma as OPP (Pragma(..))
import Ormolu.Printer (printModule)
import Control.Monad.State.Strict
import Data.Foldable

import Types
import Util
import qualified Data.Text as T

reduce :: FilePath -> FilePath -> OPR.ParseResult -> IO OPR.ParseResult
reduce test sourceFile oldOrmolu = do
    putStrLn "\n***Performing RemovePragmas***"
    debugPrint $ "Size of old ormolu: " ++ (show . T.length $ printModule oldOrmolu)
    let pragmas = prExtensions oldOrmolu
    _ormolu <$> execStateT (traverse tryAllPragmas pragmas) (ReduceState test sourceFile oldOrmolu)

tryAllPragmas :: OPP.Pragma -> StateT ReduceState IO ()
tryAllPragmas pragmaToTry@(PragmaLanguage ss)
    | length ss == 1 = tryToRemovePragma pragmaToTry
    | otherwise = do
        oldState@(ReduceState test sourceFile oldOrmolu) <- get
        traverse_ tryLanguagePragma ss
tryAllPragmas pragmaToTry = tryToRemovePragma pragmaToTry

tryToRemovePragma :: OPP.Pragma -> StateT ReduceState IO ()
tryToRemovePragma pragmaToTry = do
  oldState@(ReduceState test sourceFile oldOrmolu) <- get
  let oldPragmas = prExtensions oldOrmolu
      newOrmolu = oldOrmolu { prExtensions = filter (/= pragmaToTry) oldPragmas }
  testAndUpdateState newOrmolu () ()

tryLanguagePragma :: String -> StateT ReduceState IO ()
tryLanguagePragma s = do
    oldState@(ReduceState _ _ oldOrmolu) <- get
    liftIO . print . show . prExtensions $ oldOrmolu
    let PragmaLanguage ss:restExtensions = prExtensions oldOrmolu
        newOrmolu = oldOrmolu { prExtensions = PragmaLanguage (filter (/= s) ss) : restExtensions}
    testAndUpdateState newOrmolu () ()
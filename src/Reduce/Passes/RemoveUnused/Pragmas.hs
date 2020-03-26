module Reduce.Passes.RemoveUnused.Pragmas where

import Ormolu.Parser.Result as OPR (ParseResult, prExtensions)
import Ormolu.Parser.Pragma as OPP (Pragma(..))
import Ormolu.Printer (printModule)
import Control.Monad.State.Strict
import Data.Foldable

import Reduce.Types
import Reduce.Util
import qualified Data.Text as T

reduce :: OPR.ParseResult -> R OPR.ParseResult
reduce oldOrmolu = do
    liftIO $ putStrLn "\n***Performing RemovePragmas***"
    liftIO $ debugPrint $ "Size of old ormolu: " ++ (show . T.length $ printModule oldOrmolu)
    let pragmas = prExtensions oldOrmolu
    traverse_ tryAllPragmas pragmas
    gets _ormolu

tryAllPragmas :: OPP.Pragma -> R ()
tryAllPragmas pragmaToTry@(PragmaLanguage ss)
    | length ss == 1 = tryToRemovePragma pragmaToTry
    | otherwise = traverse_ tryLanguagePragma ss
tryAllPragmas pragmaToTry = tryToRemovePragma pragmaToTry

tryToRemovePragma :: OPP.Pragma -> R ()
tryToRemovePragma pragmaToTry = do
  liftIO $ putStrLn $ "trying pragma: " ++ show pragmaToTry
  oldOrmolu <- gets _ormolu
  let oldPragmas = prExtensions oldOrmolu
      newOrmolu  = oldOrmolu { prExtensions = filter (/= pragmaToTry) oldPragmas }
  testAndUpdateState newOrmolu

tryLanguagePragma :: String -> R ()
tryLanguagePragma s = do
    oldOrmolu <- gets _ormolu
    liftIO . print . show . prExtensions $ oldOrmolu
    let PragmaLanguage ss:restExtensions = prExtensions oldOrmolu
        newOrmolu = oldOrmolu { prExtensions = PragmaLanguage (filter (/= s) ss) : restExtensions}
    testAndUpdateState newOrmolu
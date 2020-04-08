module Reduce.Passes.RemoveUnused.Pragmas (reduce) where

import Control.Monad.State.Strict
import Data.Foldable
import qualified Data.Text as T

import Util.Types
import Util.Util

reduce :: R ()
reduce = do
    liftIO $ putStrLn "\n***Performing RemovePragmas***"
    oldOrmolu <- get
    liftIO $ debugPrint $ "Size of old ormolu: " ++ (show . T.length . T.pack . showGhc $ _parsed oldOrmolu)
    traverse_ tryToRemovePragma (_pragmas oldOrmolu)

tryToRemovePragma :: Pragma -> R ()
tryToRemovePragma pragmaToTry = do
  liftIO $ putStrLn $ "trying pragma: " ++ show pragmaToTry
  modify $ \s -> s { _pragmas = filter (/= pragmaToTry) (_pragmas s)}
  newState <- get
  testAndUpdateState newState

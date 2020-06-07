module Reduce.Passes.RemoveUnused.Pragmas (reduce) where

import Control.Monad.State.Strict
import Data.Foldable
import qualified Data.Text as T

import Util.Types
import Util.Util

reduce :: R ()
reduce = do
    liftIO $ putStrLn "\n***Performing RemovePragmas***"
    oldState <- get
    liftIO . putStrLn $ "Size of old state: " ++ (show . T.length . showState $ oldState)
    traverse_ tryToRemovePragma (_pragmas oldState)

tryToRemovePragma :: Pragma -> R ()
tryToRemovePragma pragmaToTry = do
  liftIO $ putStrLn $ "trying pragma: " ++ show pragmaToTry
  newState <- gets $ \s -> s { _pragmas = filter (/= pragmaToTry) (_pragmas s)}
  testAndUpdateStateFlex () () newState

module Reduce.Passes.RemoveUnused.Imports (reduce) where

import Control.Monad.State.Strict
import qualified Data.Text as T
import Util.Types
import Util.Util
import GHC

-- | run ghc with -Wunused-binds -ddump-json and delete imports that are mentioned there
reduce :: R ()
reduce = do
    oldState <- get
    liftIO $ putStrLn "\n***Removing Imports***"
    liftIO . putStrLn $ "Size of old state: " ++ (show . T.length . showState $ oldState)
    runPass rmvImports

rmvImports :: WaysToChange (HsModule GhcPs)
rmvImports = handleSubList f (map getLoc . hsmodImports) 
  where f loc m = m { hsmodImports = filter ((/= loc) . getLoc) (hsmodImports m) }

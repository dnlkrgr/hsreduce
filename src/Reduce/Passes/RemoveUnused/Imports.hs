
module Reduce.Passes.RemoveUnused.Imports (reduce) where

import Control.Monad.State.Strict
import qualified Data.Text as T
import Util.Types
import Util.Util
import GHC
import Data.Generics.Uniplate.Data

-- | run ghc with -Wunused-binds -ddump-json and delete imports that are mentioned there
reduce :: R ()
reduce = do
    oldState <- get
    liftIO $ putStrLn "\n***Removing Imports***"
    liftIO $ putStrLn $ "Size of old state: " ++ (show . T.length . showState $ oldState)
    void . (descendBiM (fastTryR removeImports)) $ _parsed oldState

removeImports :: Located [LImportDecl GhcPs] -> Maybe (R (Located [LImportDecl GhcPs]))
removeImports = Just . reduceListOfSubelements (map getLoc) (\loc imports -> filter ((/= loc) . getLoc) imports)

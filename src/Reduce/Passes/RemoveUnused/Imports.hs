module Reduce.Passes.RemoveUnused.Imports (reduce) where

import Data.Foldable
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
    traverse_ removeImport . hsmodImports . unLoc . _parsed $ oldState

removeImport :: LImportDecl GhcPs -> R ()
removeImport (L l _) = testAndUpdateStateFlex () () =<< changeImports (filter ((/= l) . getLoc)) <$> get 

changeImports :: ([LImportDecl GhcPs] -> [LImportDecl GhcPs]) -> RState -> RState
changeImports f oldState =
    let L moduleLoc oldModule = _parsed oldState
        newImports            = f $ hsmodImports oldModule
    in oldState {_parsed = L moduleLoc oldModule {hsmodImports = newImports}}

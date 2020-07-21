module Reduce.Passes.DataTypes (inlineNewType) where

import Control.Concurrent.STM
import Lens.Micro.Platform
import Data.Generics.Uniplate.Data
import Control.Monad.State
import Control.Monad.Reader
import GHC

import Util.Types
import Util.Util

-- this should prob. live in its own module
-- *** BEGINNING OF NEW STUFF

inlineNewType :: R ()
inlineNewType = do
    printInfo "inlineNewType"

    conf <- ask
    ast <- _parsed <$> (liftIO . atomically $ readTVar (_tState conf) )


    forM_ 
        [ (unLoc newtypeName, unLoc argName, unLoc constrName) 
        | DataDecl _ newtypeName _ _ (HsDataDefn _ _ _ _ _ [L _ (ConDeclH98 _ constrName _ _ _ (PrefixCon [L _ (HsTyVar _ _ argName)]) _)] _ ) :: TyClDecl GhcPs <- universeBi ast]
        inlineNewType_

inlineNewType_ :: (RdrName, RdrName, RdrName) -> R ()
inlineNewType_ (nn, an, cn) = do
    conf     <- ask
    oldState <- liftIO . readTVarIO $ _tState conf

    let 
        oldAST      = oldState ^. parsed
        newAST      = transformBi (inlineAtPatterns cn) $ transformBi (inlineAtType nn an) oldAST
        sizeDiff    = length (lshow oldAST) - length (lshow newAST)
        newState    = oldState & parsed .~ newAST & isAlive .~ (oldState ^. isAlive || oshow oldAST /= oshow newAST)
 
    liftIO (tryNewValue conf newState) >>= \case
        True  -> liftIO . atomically $ do
            writeTVar (_tState conf) newState
 
            updateStatistics_ conf "rmvUnusedParams" True sizeDiff

        False -> liftIO $ updateStatistics conf "rmvUnusedParams" False 0
 

inlineAtType :: RdrName -> RdrName -> HsType GhcPs -> HsType GhcPs
inlineAtType newtypeName argName (HsTyVar x p (L l tyvarName)) 
    | newtypeName == tyvarName = HsTyVar x p (L l argName)
inlineAtType _ _ t = t

-- look if name pops up, if yes, change it
inlineAtPatterns :: RdrName -> Pat GhcPs -> Pat GhcPs
inlineAtPatterns constrName (ConPatIn (L _ name) (PrefixCon [arg])) 
    | constrName == name = unLoc arg
inlineAtPatterns _ p = p


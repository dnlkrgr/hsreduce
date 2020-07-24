module Reduce.Passes.DataTypes (inlineType) where

import Control.Concurrent.STM
import Lens.Micro.Platform
import Data.Generics.Uniplate.Data
import Control.Monad.State
import Control.Monad.Reader
import GHC

import Util.Types
import Util.Util

inlineType :: R ()
inlineType = do
    printInfo "inlineType"

    conf <- ask
    ast <- _parsed <$> (liftIO . atomically $ readTVar (_tState conf) )

    -- data decls / newtypes / type synonyms
    forM_ 
        ([ (unLoc newtypeName, argName, Just constrName) 
        | DataDecl _ newtypeName _ _ (HsDataDefn _ _ _ _ _ [ConDeclP constrName [TyVarP argName]] _ )   :: TyClDecl GhcPs <- universeBi ast]
        <> [ (unLoc newtypeName, argName, Nothing) | SynDecl _ newtypeName _ _ (TyVarP argName)         :: TyClDecl GhcPs <- universeBi ast])
        inlineTypeHelper

pattern ConDeclP :: RdrName -> [LBangType GhcPs] -> LConDecl GhcPs
pattern ConDeclP constrName args <- L _ (ConDeclH98 _ (L _ constrName) _ _ _ (PrefixCon args) _)

pattern TyVarP :: RdrName -> LHsType GhcPs
pattern TyVarP name <- L _ (HsTyVar _ _ (L _ name))

inlineTypeHelper :: (RdrName, RdrName, Maybe RdrName) -> R ()
inlineTypeHelper (nn, argName, mConstrName) = do
    conf     <- ask
    oldState <- liftIO . readTVarIO $ _tState conf

    let 
        oldAST      = oldState ^. parsed
        newAST      = (case mConstrName of
            Nothing             -> id
            Just constrName     -> transformBi (handlePatterns constrName)) $ transformBi (handleTypes nn argName) oldAST
        sizeDiff    = length (lshow oldAST) - length (lshow newAST)
        newState    = oldState & parsed .~ newAST & isAlive .~ (oldState ^. isAlive || oshow oldAST /= oshow newAST)
 
    liftIO (tryNewValue conf newState) >>= \case
        True  -> liftIO . atomically $ do
            writeTVar (_tState conf) newState
 
            updateStatistics_ conf "rmvUnusedParams" True sizeDiff

        False -> liftIO $ updateStatistics conf "rmvUnusedParams" False 0
 

handleTypes :: RdrName -> RdrName -> HsType GhcPs -> HsType GhcPs
handleTypes newtypeName argName t@(HsTyVar x p (L l tyvarName)) 
    | newtypeName == tyvarName  = HsTyVar x p (L l argName)
    | otherwise                 = t
handleTypes _ _ t = t

handlePatterns :: RdrName -> Pat GhcPs -> Pat GhcPs
handlePatterns constrName p@(ConPatIn (L _ name) (PrefixCon [arg])) 
    | constrName == name    = unLoc arg
    | otherwise             = p
handlePatterns _ p = p
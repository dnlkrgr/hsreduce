module Passes.DataTypes (inline, rmvConArgs) where

import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.State
import Data.Generics.Uniplate.Data
import GHC
import Lens.Micro.Platform
import Types
import Util

passId :: String
passId = "rmvConArgs"

rmvConArgs :: R ()
rmvConArgs = do
    printInfo passId

    conf <- ask
    ast <- _parsed <$> (liftIO . atomically $ readTVar (_tState conf))

    -- data decls / newtypes / type synonyms
    forM_
        ( [(constrName, map unLoc args) | PrefixConP constrName args :: LConDecl GhcPs <- universeBi ast]
          <> [(constrName, map (unLoc . cd_fld_type . unLoc) args) | RecConP constrName args :: LConDecl GhcPs <- universeBi ast]
        )
        rmvConArgsHelper

pattern PrefixConP :: RdrName -> [LBangType GhcPs] -> LConDecl GhcPs
pattern PrefixConP constrName args <- L _ (ConDeclH98 _ (L _ constrName) _ _ _ (PrefixCon args) _)

pattern RecConP :: RdrName -> [LConDeclField GhcPs] -> LConDecl GhcPs
pattern RecConP constrName args <- L _ (ConDeclH98 _ (L _ constrName) _ _ _ (RecCon (L _ args)) _)

pattern TyVarP :: RdrName -> LHsType GhcPs
pattern TyVarP name <- L _ (HsTyVar _ _ (L _ name))

-- TODO: change this
rmvConArgsHelper :: (RdrName, [a]) -> R ()
rmvConArgsHelper (conId, args) = do
    conf <- ask
    let is = [1 .. length args] -- args2UnitPositions args
    liftIO $ atomically $ modifyTVar (_tState conf) $ \s -> s & numRmvdArgs .~ 0

    forM_ is $ \i ->
        liftIO $
            tryNewState
                passId
                ( \oldState ->
                      let oldAST = oldState ^. parsed
                          newI = i - fromIntegral (oldState ^. numRmvdArgs)
                          newAST =
                              transformBi (rmvArgsFromExpr conId (length args) i)
                                  . transformBi (rmvArgsFromPat conId newI)
                                  . transformBi (rmvArgsFromConDecl conId newI)
                                  $ oldAST
                          newState =
                              oldState
                                  & parsed .~ newAST
                                  & isAlive .~ (oldState ^. isAlive || oshow oldAST /= oshow newAST)
                                  & numRmvdArgs +~ 1
                       in newState
                )
                conf

rmvArgsFromConDecl :: RdrName -> Int -> ConDecl GhcPs -> ConDecl GhcPs
rmvArgsFromConDecl conId i c@(ConDeclH98 x n fa tvs ctxt (PrefixCon args) doc)
    | conId == unLoc n = ConDeclH98 x n fa tvs ctxt (PrefixCon $ delete i args) doc
    | otherwise = c
rmvArgsFromConDecl conId i c@(ConDeclH98 x n fa tvs ctxt (RecCon (L l args)) doc)
    | conId == unLoc n = ConDeclH98 x n fa tvs ctxt (RecCon . L l $ delete i args) doc
    | otherwise = c
rmvArgsFromConDecl _ _ c = c

rmvArgsFromPat :: RdrName -> Int -> Pat GhcPs -> Pat GhcPs
rmvArgsFromPat constrName n p@(ConPatIn (L l name) (PrefixCon args))
    | constrName == name = ConPatIn (L l name) (PrefixCon $ delete n args)
    | otherwise = p
rmvArgsFromPat _ _ p = p

-- ***************************************************************************
-- INLINE TYPE

-- ***************************************************************************

inline :: R ()
inline = do
    printInfo "inline"

    conf <- ask
    ast <- _parsed <$> (liftIO . atomically $ readTVar (_tState conf))

    -- data decls / newtypes / type synonyms
    forM_
        ( [ (unLoc newtypeName, argName, Just constrName)
            | DataDecl _ newtypeName _ _ (HsDataDefn _ _ _ _ _ [PrefixConP constrName [TyVarP argName]] _) :: TyClDecl GhcPs <- universeBi ast
          ]
              <> [(unLoc newtypeName, argName, Nothing) | SynDecl _ newtypeName _ _ (TyVarP argName) :: TyClDecl GhcPs <- universeBi ast]
        )
        inlineTypeHelper

inlineTypeHelper :: (RdrName, RdrName, Maybe RdrName) -> R ()
inlineTypeHelper (nn, argName, mConstrName) = do
    liftIO
        . tryNewState
            "inlineType"
            ( parsed %~ \oldAST ->
                  ( case mConstrName of
                        Nothing -> id
                        Just constrName -> transformBi (inlineTypeAtPat constrName)
                  )
                      . transformBi (inlineTypeAtType nn argName)
                      $ oldAST
            )
        =<< ask

inlineTypeAtType :: RdrName -> RdrName -> HsType GhcPs -> HsType GhcPs
inlineTypeAtType newtypeName argName t@(HsTyVar x p (L l tyvarName))
    | newtypeName == tyvarName = HsTyVar x p (L l argName)
    | otherwise = t
inlineTypeAtType _ _ t = t

inlineTypeAtPat :: RdrName -> Pat GhcPs -> Pat GhcPs
inlineTypeAtPat constrName p@(ConPatIn (L _ name) (PrefixCon [arg]))
    | constrName == name = unLoc arg
    | otherwise = p
inlineTypeAtPat _ p = p
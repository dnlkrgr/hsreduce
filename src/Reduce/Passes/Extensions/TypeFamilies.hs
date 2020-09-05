module Reduce.Passes.Extensions.TypeFamilies (rmvEquations, apply) where

import SrcLoc
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Generics.Uniplate.Data
import Data.List
import GHC
import Lens.Micro.Platform
import Outputable hiding ((<>))
import Util.Types
import Util.Util

rmvEquations :: WaysToChange (HsDecl GhcPs)
rmvEquations = handleSubList f p
    where
        p (TyClD _ FamDecl {tcdFam = d}) = case fdInfo d of
            ClosedTypeFamily (Just equations) -> map getLoc equations
            _ -> []
        p _ = []
        f loc (TyClD _ t@FamDecl {tcdFam = d}) = case fdInfo d of
            ClosedTypeFamily (Just equations) ->
                TyClD NoExt $ t {tcdFam = d {fdInfo = ClosedTypeFamily . Just $ filter ((/= loc) . getLoc) equations}}
            _ -> TyClD NoExt t
        f _ t = t

apply :: R ()
apply = do
    let passId = "apply type families"
    printInfo passId

    ast <- fmap _parsed . liftIO . readTVarIO =<< asks _tState

    void $ transformBiM applyHelper ast

arstP :: (Outputable.Outputable a1, Outputable.Outputable a2) => a1 -> a2 -> Bool
arstP feqn_rhs = (oshow feqn_rhs `isInfixOf`) . oshow

applyHelper :: p ~ GhcPs => FamEqn p (HsTyPats p) (LHsType p) -> R (FamEqn p (HsTyPats p) (LHsType p))
applyHelper f@(FamEqn {..}) = do
    conf <- ask
    let index = fst . head . filter (arstP feqn_rhs . snd) $ zip [1 ..] feqn_pats

    ast <- liftIO . fmap _parsed $ readTVarIO (_tState conf)
    forM_ [ t | (t :: LHsType GhcPs) <- universeBi ast ] $ \(L l _) -> do
        liftIO $
            tryNewState
                "apply type families"
                ( \oldState ->
                      let oldAST = oldState ^. parsed
                          tycon = unLoc feqn_tycon
                          c = if any (arstP feqn_rhs) feqn_pats 
                                -- the rhs is one of the patterns
                                -- get the index of the pattern
                                -- find occurrences of the type family
                                -- replace them by nth pattern
                                then takeNthArgument tycon (length feqn_pats) index
                                else replaceWithRHs tycon (unLoc feqn_rhs)
                          newAST = 

                              transformBi (overwriteAtLoc l c) oldAST
                          newState =
                              oldState
                                  & parsed .~ newAST
                                  & numRmvdArgs +~ 1
                       in newState
                )
                conf
    return f
    -- the rhs is not found in any of the patterns
    -- find occurrences of the type family
    -- replace them by rhs
applyHelper f = return f


replaceWithRHs :: p ~ GhcPs => IdP p -> HsType p -> HsType p -> HsType p
replaceWithRHs tycon rhs t 
    | oshow tycon `isInfixOf` oshow t = rhs
    | oshow tycon `isPrefixOf` oshow t = rhs
    | otherwise = t

-- the rhs is one of the patterns sub type expressions
-- get the index of the pattern
-- find occurrences of the sub type expression
-- replace them by the right hand side

-- TODO: see if we have a HsAppTy and count the arguments
takeNthArgument :: p ~ GhcPs => IdP p -> Int -> Int -> HsType p -> HsType p
takeNthArgument tycon n i t
    | oshow tycon `isInfixOf` oshow t = takeNthArgumentHelper n i t
    | oshow tycon `isPrefixOf` oshow t = takeNthArgumentHelper n i t
    | otherwise = t

takeNthArgumentHelper :: Int -> Int -> HsType GhcPs -> HsType GhcPs
takeNthArgumentHelper n i (HsAppTy _ (L _ a) (L _ b))
    | n == i = b
    | otherwise = takeNthArgumentHelper (n -1) i a
takeNthArgumentHelper _ _ t = t
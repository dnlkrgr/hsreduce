module Reduce.Passes.Extensions.TypeFamilies (rmvEquations, apply, familyResultSig) where

import Data.Generics.Uniplate.Data
import Data.List
import GHC hiding (Pass)
import Outputable hiding ((<>))
import Util.Types
import Util.Util

familyResultSig :: Pass
familyResultSig = mkPass "familyResultSig" f
    where
        f :: WaysToChange (FamilyResultSig GhcPs)
        f (NoSig _) = []
        f (XFamilyResultSig _) = []
        f _ = [const (NoSig NoExt)]

rmvEquations :: Pass
rmvEquations = mkPass "typefamilies:rmvEquations" f
    where
        f :: WaysToChange (HsDecl GhcPs)
        f = handleSubList g p
            where
                p (TyClD _ FamDecl {tcdFam = d}) = case fdInfo d of
                    ClosedTypeFamily (Just equations) -> map getLoc equations
                    _ -> []
                p _ = []
                g loc (TyClD _ t@FamDecl {tcdFam = d}) = case fdInfo d of
                    ClosedTypeFamily (Just equations) ->
                        TyClD NoExt $ t {tcdFam = d {fdInfo = ClosedTypeFamily . Just $ filter ((/= loc) . getLoc) equations}}
                    _ -> TyClD NoExt t
                g _ t = t

apply :: Pass
apply = AST "typefamilies:apply" $ \ast ->
    concatMap (\FamEqn{..} -> 
        let index = fst . head . filter (isContainedIn feqn_rhs . snd) $ zip [1 ..] feqn_pats
            tycon = unLoc feqn_tycon

        in map (\(L l _) -> 
                let c =
                        if any (isContainedIn feqn_rhs) feqn_pats
                        then 
                            -- the rhs is one of the patterns
                            -- get the index of the pattern
                            -- find occurrences of the type family
                            -- replace them by nth pattern
                            takeNthArgument tycon (length feqn_pats) index
                        else replaceWithRHs tycon feqn_rhs
                in transformBi (overwriteAtLoc l c)
                )
                [t | (t :: LHsType GhcPs) <- universeBi ast, isContainedIn tycon t]
        )
        [ f | f@FamEqn{} :: FamEqn GhcPs (HsTyPats GhcPs) (LHsType GhcPs) <- universeBi ast]

isContainedIn :: (Outputable.Outputable a1, Outputable.Outputable a2) => a1 -> a2 -> Bool
isContainedIn feqn_rhs = (oshow feqn_rhs `isInfixOf`) . oshow

replaceWithRHs :: p ~ GhcPs => IdP p -> LHsType p -> HsType p -> HsType p
replaceWithRHs tycon (unLoc -> rhs) t
    | oshow tycon `isPrefixOf` oshow t = rhs
    | oshow tycon `isInfixOf` oshow t = rhs
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
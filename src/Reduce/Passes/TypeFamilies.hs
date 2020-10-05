module Reduce.Passes.TypeFamilies (rmvEquations, apply, familyResultSig) where

import Data.Generics.Uniplate.Data (transformBi, universeBi)
import Data.List (isInfixOf)
import GHC
    ( FamEqn
          ( FamEqn,
            feqn_bndrs,
            feqn_ext,
            feqn_fixity,
            feqn_pats,
            feqn_rhs,
            feqn_tycon
          ),
      FamilyDecl (fdInfo),
      FamilyInfo (ClosedTypeFamily),
      FamilyResultSig (NoSig, XFamilyResultSig),
      GenLocated (L),
      GhcPs,
      HsDecl (TyClD),
      HsTyPats,
      HsType (HsAppTy, HsTyVar),
      IdP,
      LHsType,
      NoExt (NoExt),
      RdrName,
      TyClDecl (FamDecl, tcdFam),
      getLoc,
      unLoc,
    )
import Outputable (Outputable)
import Util.Types (Pass (AST), WaysToChange)
import Util.Util (handleSubList, mkPass, oshow, overwriteAtLoc)

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
    concatMap
        ( \FamEqn {..} ->
              let index = fst . head . filter (isContainedIn feqn_rhs . snd) $ zip [1 ..] feqn_pats
                  tycon = unLoc feqn_tycon
               in map
                      ( \(L l _) ->
                            let c =
                                    if any (isContainedIn feqn_rhs) feqn_pats
                                        then -- the rhs is one of the patterns
                                        -- get the index of the pattern
                                        -- find occurrences of the type family
                                        -- replace them by nth pattern

                                        -- traceShow (oshow tTemp)
                                        -- . traceShow (oshow tycon)
                                        -- . traceShow (oshow feqn_pats)
                                        -- . traceShow (oshow feqn_rhs)
                                        -- . traceShow (oshow $ takeNthArgument (length feqn_pats) index tTemp)
                                            takeNthArgument (length feqn_pats) index
                                        else replaceWithRHs tycon feqn_rhs
                             in transformBi (overwriteAtLoc l c)
                      )
                      [t | (t :: LHsType GhcPs) <- universeBi ast, typeContainsTyCon tycon (unLoc t)]
        )
        [f | f@FamEqn {} :: FamEqn GhcPs (HsTyPats GhcPs) (LHsType GhcPs) <- universeBi ast]

isContainedIn :: (Outputable.Outputable a1, Outputable.Outputable a2) => a1 -> a2 -> Bool
isContainedIn feqn_rhs = (oshow feqn_rhs `isInfixOf`) . oshow

replaceWithRHs :: p ~ GhcPs => IdP p -> LHsType p -> HsType p -> HsType p
replaceWithRHs tycon (unLoc -> rhs) t
    | not $ tycon `isContainedIn` rhs = rhs
    | otherwise = t

-- the rhs is one of the patterns sub type expressions
-- get the index of the pattern
-- find occurrences of the sub type expression
-- replace them by the right hand side

-- TODO: see if we have a HsAppTy and count the arguments
takeNthArgument :: Int -> Int -> HsType GhcPs -> HsType GhcPs
takeNthArgument n i (HsAppTy _ (L _ a) (L _ b))
    | n == i = b
    | otherwise = takeNthArgument (n -1) i a
takeNthArgument _ _ t = t

typeContainsTyCon :: RdrName -> HsType GhcPs -> Bool
typeContainsTyCon tycon (HsTyVar _ _ (L _ name)) = tycon == name
typeContainsTyCon tycon (HsAppTy _ (L _ t) _) = typeContainsTyCon tycon t
typeContainsTyCon _ _ = False
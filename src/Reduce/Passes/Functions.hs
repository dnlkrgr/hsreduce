module Reduce.Passes.Functions (inline, etaReduceMatches, rmvMatches, rmvRHSs, rmvGuards) where

import Data.Generics.Uniplate.Data (transformBi, universeBi)
import GHC
    ( GRHS (GRHS),
      GRHSs (GRHSs, grhssGRHSs),
      GenLocated (L),
      GhcPs,
      HsBindLR (FunBind),
      HsExpr (HsApp, HsVar),
      LGRHS,
      LHsExpr,
      LHsLocalBinds,
      LMatch,
      Match (Match, m_grhss, m_pats),
      MatchGroup (MG),
      NoExt (NoExt),
      getLoc,
      unLoc,
    )
import Util.Types (Pass (AST), WaysToChange)
import Util.Util (handleSubList, mkPass, oshow)

-- function with:
-- - one match
-- - without guards
-- - with one rhs
-- - where the body is applying some function to an argument
etaReduceMatches :: Pass
etaReduceMatches = mkPass "etaReduceMatches" f
    where
        f :: p ~ GhcPs => WaysToChange [LMatch p (LHsExpr p)]
        f [L l1 m@(Match {m_pats = pats@(_ : _), m_grhss = g@(GRHSs {grhssGRHSs = [L l2 (GRHS _ guards (L l3 (HsApp _ lExpr rExpr)))]})})]
            | let sPat = oshow (last pats),
              sPat == "_" || sPat == oshow rExpr =
                [const [L l1 (m {m_pats = init pats, m_grhss = g {grhssGRHSs = [L l2 (GRHS NoExt guards (L l3 (unLoc lExpr)))]}})]]
            | otherwise = []
        f _ = []

-- only one match and rhs is just a function name
inline :: Pass
inline = AST "inlineFunctions" $ \ast ->
    map
        ( \(funId, body) ->
              transformBi (replaceFunIdWithBody funId body)
        )
        -- f = g
        ( [ (funId, body)
            | (FunBind _ (L _ funId) (MG _ lmatches _) _ _) :: HsBindLR GhcPs GhcPs <- universeBi ast,
              let matches = map unLoc $ unLoc lmatches,
              length matches == 1,
              length (m_pats $ head $ matches) == 0,
              length (grhssGRHSs $ m_grhss $ head $ matches) == 1,
              GRHS _ _ body :: GRHS GhcPs (LHsExpr GhcPs) <- unLoc . head . grhssGRHSs . m_grhss <$> matches
          ]
              -- f x = g x, when eta reducing somehow isn't applicable
              <> [ (funId, lExpr)
                   | (FunBind _ (L _ funId) (MG _ (L _ [L _ (Match {m_pats = pats@(_ : _), m_grhss = (GRHSs {grhssGRHSs = [L _ (GRHS _ _ (L _ (HsApp _ lExpr rExpr)))]})})]) _) _ _) :: HsBindLR GhcPs GhcPs <- universeBi ast,
                     let sPat = oshow (last pats),
                     sPat == "_" || sPat == oshow rExpr
                 ]
                 -- f [L l1 m@(Match {m_pats = pats@(_ : _), m_grhss = g@(GRHSs {grhssGRHSs = [L l2 (GRHS _ guards (L l3 (HsApp _ lExpr rExpr)))]})})]
                 --         [const [L l1 (m {m_pats = init pats, m_grhss = g {grhssGRHSs = [L l2 (GRHS NoExt guards (L l3 (unLoc lExpr)))]}})]]
                 --     | otherwise = []
        )
    where
        replaceFunIdWithBody funName (L _ body) old@(HsVar _ (L _ n))
            | funName == n = body
            | otherwise = old
        replaceFunIdWithBody _ _ old = old

-- ***************************************************************************

-- MATCHES

-- ***************************************************************************

rmvRHSs :: Pass
rmvRHSs = mkPass "rmvRHSs" f
    where
        f :: WaysToChange (Match GhcPs (LHsExpr GhcPs))
        f (Match _ _ _ (GRHSs _ [] _)) = []
        f mm = handleSubList g p mm
            where
                p = \case
                    -- reverse because the lower have to be tried first
                    MatchP iterGRHSs _ -> map getLoc . reverse $ iterGRHSs
                    _ -> []
                g grhsLoc = \case
                    m@(Match _ _ _ (GRHSs _ grhss lb)) ->
                        let newGRHSs = filter ((/= grhsLoc) . getLoc) grhss
                         in case newGRHSs of
                                [] -> m
                                _ -> m {m_grhss = GRHSs NoExt newGRHSs lb}
                    m -> m

rmvMatches :: Pass
rmvMatches = mkPass "rmvMatches" f
    where
        f :: WaysToChange [LMatch GhcPs (LHsExpr GhcPs)]
        f = handleSubList (\loc -> filter ((/= loc) . getLoc)) (map getLoc)

-- <> [ filter (\(L _ (Match _ _ _ grhss@GRHSs{})) -> showSDocUnsafe (pprGRHSs LambdaExpr grhss) /= "-> undefined")
-- ,  filter (\(L _ (Match _ _ _ (GRHSs _ grhs _))) -> not (all ( ("undefined" `isSubsequenceOf`) . showSDocUnsafe . pprGRHS LambdaExpr . unLoc) grhs))]

rmvGuards :: Pass
rmvGuards = mkPass "rmvGuards" f
    where
        f :: WaysToChange (GRHS GhcPs (LHsExpr GhcPs))
        f (GRHS _ [] _) = []
        f g@(GRHS _ _ body) = [const (GRHS NoExt [] body)] <> handleSubList h p g
            where
                p (GRHS _ stmts _) = map getLoc stmts
                p _ = []
                h loc (GRHS _ s b) = GRHS NoExt (filter ((/= loc) . getLoc) s) b
                h _ _ = g
        f _ = []

-- ***************************************************************************

-- PATTERN SYNONYMS

-- ***************************************************************************

pattern MatchP :: [LGRHS GhcPs (LHsExpr GhcPs)] -> LHsLocalBinds GhcPs -> Match GhcPs (LHsExpr GhcPs)
pattern MatchP grhss binds <- Match _ _ _ (GRHSs _ grhss binds)
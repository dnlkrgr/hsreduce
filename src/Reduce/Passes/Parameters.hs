module Reduce.Passes.Parameters (rmvUnusedParams, reduce) where

import Data.Generics.Uniplate.Data (transformBi, universeBi)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import GHC hiding (Pass)
import Util.Types (Pass (AST))
import Util.Util
import qualified Data.Text as T

reduce ::
    T.Text ->
    (ParsedSource -> [(RdrName, Int)]) ->
    (RdrName -> ParsedSource -> [Int]) ->
    (RdrName -> Int -> NE.NonEmpty Int -> Int -> Int -> ParsedSource -> ParsedSource) ->
    Pass
reduce passId getNamesAndParamsLengths getArgsLength myTransform = AST passId $ \ast ->
    concatMap
        ( \(name, lenArgs) ->
              map
                  ( \i ->
                        let mTemp = NE.nonEmpty (getArgsLength name ast)
                         in case mTemp of
                                Just newLenArgs ->
                                    if NE.head newLenArgs >= 1
                                        then
                                            let nRmvdArgs = lenArgs - NE.head newLenArgs
                                                newI = i - nRmvdArgs
                                             in myTransform name lenArgs newLenArgs i newI
                                        else id
                                _ -> id
                  )
                  [1 .. lenArgs]
        )
        (getNamesAndParamsLengths ast)

rmvUnusedParams :: Pass
rmvUnusedParams =
    reduce
        "Functions.rmvUnusedParams"
        ( \ast ->
                [ (funId, length pats)
                | FunBind _ (unLoc -> funId) funMG _ _ :: HsBindLR GhcPs GhcPs <- universeBi ast,
                    -- infix matches can't be handled yet, GHC panics when trying to print what hsreduce produces
                    (not . any isInfixMatch) . map unLoc . unLoc $ mg_alts funMG,
                    pats <- maybeToList $ NE.head <$> NE.nonEmpty (matchgroup2ListOfPats funMG)
                ]
        )
        getPatsLength
        ( \funId _ newLenArgs _ newI ->
              transformBi (rmvArgsFromExpr funId (NE.head newLenArgs) newI)
                  . transformBi (handleSigs funId newI)
                  . transformBi (handleFunBinds funId newI)
        )

getPatsLength :: RdrName -> ParsedSource -> [Int]
getPatsLength name ast =
    [ length . m_pats . unLoc $ alternatives
      | FunBind _ (L _ funId) funMG _ _ :: HsBindLR GhcPs GhcPs <- universeBi ast,
        name == funId,
        alternatives <- unLoc $ mg_alts funMG
    ]

-- get indices of wildcard patterns
matchgroup2ListOfPats :: MatchGroup GhcPs (LHsExpr GhcPs) -> [[LPat GhcPs]]
matchgroup2ListOfPats mg = [m_pats . unLoc $ match | match <- unLoc $ mg_alts mg]
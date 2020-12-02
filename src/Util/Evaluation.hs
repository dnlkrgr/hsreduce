module Util.Evaluation where

import Data.Time.Clock
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Control.Monad
import Path
import Path.IO
import System.Process
import System.Exit
import Data.Word
import Control.Applicative
import System.Random.Shuffle
import Reduce.Passes
import Data.List
import Data.Maybe
import Debug.Trace
import Data.IORef
import Util.Types
import Util.Util

drawRandomOrdering :: IORef [[Pass]] -> IO [Pass]
drawRandomOrdering ref = do
    visitedPasses <- readIORef ref
    myPasses <- shuffleM $ allPurePasses
    if myPasses `elem` visitedPasses
        then drawRandomOrdering ref
        else do
            writeIORef ref $ myPasses : visitedPasses
            return myPasses

passesGridSearch :: IO ()
passesGridSearch = do
    print $ "loaded with these testcases: " <> show testCases

    -- create stats dir
    statsDir <- liftA2 (</>) getHomeDir (pure [reldir|.hsreduce|])
    ensureDir statsDir

    t <- getCurrentTime
    resultFile <- liftA2 (</>) (pure statsDir) (parseRelFile $ "pass_orderings_" <> show t <> "_stats.csv")
    appendFile (fromAbsFile resultFile) $ ("end byte size, end token number, end name number, time spent, successful invocations, total invocations, token diff, name diff, td/time spent, nd/time spent, pass ordering" <> "\n")

    ref <- newIORef []

    let numberOfRounds = 10000
    forM_ [1 :: Int .. numberOfRounds] $ \i -> do

        putStrLn ""
        putStrLn $ "Round Progress: [" <> show i <> "/" <> (show numberOfRounds) <> "]"

        myPasses <- drawRandomOrdering ref

        -- resultFile <- liftA2 (</>) (pure statsDir) (parseRelFile $ "ordering_" <> "_stats.csv")
        -- let myPasses = allPurePasses

        let commandString = 
                "~/.cabal/bin/hsreduce reduce --test interesting.sh --sourceFile Bug.hs --numberOfThreads 4 --recordStatistics --timeOut 30 --customPassOrdering \"" 
                <> (show myPasses)
                <> "\""


        results <- fmap catMaybes . forM testCases $ \testCaseName -> do

            putStrLn $ "Running on <" <> testCaseName <> ">"

            let 
                testCaseDir = "../hsreduce-test-cases/" <> testCaseName <> "/"

            (flip readCreateProcessWithExitCode "" $ (shell $ "nix-shell --run '" <> commandString <> "'") {cwd = Just testCaseDir}) >>= \case

                (ExitSuccess, _, _) -> do
                    let 
                        reducedFileName = testCaseDir <> "Bug_hsreduce.hs"
                        performanceFileName = testCaseDir <> "hsreduce_performance.csv"

                    tokenNumber <- countTokensM reducedFileName
                    nameNumber <- countNamesM reducedFileName

                    (_: timeSpent: _: _: _: byteSize: _: successfulInvocations: totalInvocations : tokenDiff : nameDiff : _) <-
                        map T.unpack 
                        . T.words 
                        . T.map (\c -> if c == ',' then ' ' else c) 
                        . last 
                        . T.lines 
                        <$> TIO.readFile performanceFileName

                    pure $ Just $ traceShowId (read byteSize :: Word64, tokenNumber, nameNumber, read timeSpent :: Double, read successfulInvocations :: Word64, read totalInvocations :: Word64, read tokenDiff :: Integer, read nameDiff :: Integer)

                e -> do
                    print e 
                    pure Nothing

        let (bs, tn, nn, ts, si, ti, td, nd) = foldr (\(bl, tnl, nnl, cl, sil, til, tdl, ndl) (br, tnr, nnr, cr, sir, tir, tdr, ndr) -> (bl + br, tnl + tnr, nnl + nnr, cl + cr, sil + sir, til + tir, tdl + tdr, ndl + ndr)) (0, 0, 0, 0, 0, 0, 0, 0) results

        appendFile (fromAbsFile resultFile) $ (intercalate "," [show bs, show tn, show nn, show ts, show si, show ti, show td, show nd, show (fromIntegral td / ts), show (fromIntegral nd / ts), show myPasses] <> "\n")
    where
            testCases = 
                -- [ "ticket14779" ]
                [ "ticket14779",
                "ticket14040",
                "ticket14270",
                "ticket15696_1",
                "ticket16979",   
                "ticket18098",
                "ticket8763"]
                --   "ticket15696_2" ]

-- -- passesGridSearch :: IO ()
-- -- passesGridSearch = do
-- --     mresults <- forM (zip [1..] allPurePasses) $ \(index, orderedPasses) -> do
-- --         print $ "Looking at ordering nr. " <> show index <> " of " <> (show $ length $ permutations passesThatCanBeReordered)

-- --         let 
-- --             testCaseDir = "../hsreduce-test-cases/ticket14779/"
-- --             commandString = 
-- --                 "hsreduce reduce --test interesting.sh --sourceFile Bug.hs --numberOfThreads 8 --recordStatistics --timeOut 30 --customPassOrdering \"" 
-- --                 <> (show orderedPasses) 
-- --                 <> "\""

-- --         (flip readCreateProcessWithExitCode "" $ (shell $ "nix-shell --run '" <> commandString <> "'") {cwd = Just testCaseDir}) >>= \case
-- --             (ExitSuccess, _, _) -> do
-- --                 let 
-- --                     reducedFileName = testCaseDir <> "Bug_hsreduce.hs"
-- --                     performanceFileName = testCaseDir <> "hsreduce_performance.csv"

-- --                 Just tokenNumber <- countTokens reducedFileName

-- --                 [_, timeSpent, _, _, _, byteSize, _] <- 
-- --                     map T.unpack . T.words . T.map (\c -> if c == ',' then ' ' else c) . last . T.lines <$> TIO.readFile performanceFileName

-- --                 pure $ Just (show orderedPasses, tokenNumber, byteSize, timeSpent)
-- --             e -> print e >> pure Nothing

-- --     print mresults


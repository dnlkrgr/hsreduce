module Util.Evaluation where

import Util.Types
import Control.Exception
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import DynFlags
import GHC hiding (Parsed)
import Control.Monad.IO.Class
import Lexer
import Parser
import Control.Monad
import GHC.Paths (libdir)
import Util.Parser
import StringBuffer
import SrcLoc
import FastString
import Path
import Path.IO
import System.Process
import System.Exit
import Data.Word
import Control.Applicative
import System.Random.Shuffle
-- import Reduce.Passes
import Data.List
import Data.Maybe
import Debug.Trace


-- passesGridSearch :: IO ()
-- passesGridSearch = do
--     print $ "loaded with these testcases: " <> show testCases

--     statsDir <- liftA2 (</>) getHomeDir (pure [reldir|.hsreduce|])

--     removeDirRecur statsDir >> ensureDir statsDir

--     let numberOfRounds = 8
--     forM_ [1 :: Int .. numberOfRounds] $ \i -> do

--         putStrLn ""
--         putStrLn $ "Round Progress: [" <> show i <> "/" <> (show numberOfRounds) <> "]"

--         resultFile <- liftA2 (</>) (pure statsDir) (parseRelFile $ "ordering_" <> show i <> "_stats.csv")

--         myPasses <- shuffleM $ allPurePasses
--         print $ "length myPasses: " <> (show $ length myPasses)

--         -- resultFile <- liftA2 (</>) (pure statsDir) (parseRelFile $ "ordering_" <> "_stats.csv")
--         -- let myPasses = allPurePasses

--         forM_ (zip [1 :: Int ..] $ myPasses) $ \(index, iterPass) -> do

--             putStrLn ""
--             putStrLn $ "Pass Progress: [" <> show index <> "/" <> (show $ length $ allPurePasses) <> "]"
--             putStrLn $ "Calling hsreduce without <" <> show iterPass <> ">"

--             let newPasses = filter (/= iterPass) myPasses

--             print $ "length newPasses: " <> (show $ length newPasses)

--             let commandString = 
--                     "hsreduce reduce --test interesting.sh --sourceFile Bug.hs --numberOfThreads 8 --recordStatistics --timeOut 30 --customPassOrdering \"" 
--                     <> (show newPasses)
--                     <> "\""


--             results <- fmap catMaybes . forM testCases $ \testCaseName -> do

--                 putStrLn $ "Running on <" <> testCaseName <> ">"

--                 let 
--                     testCaseDir = "../hsreduce-test-cases/" <> testCaseName <> "/"

--                 (flip readCreateProcessWithExitCode "" $ (shell $ "nix-shell --run '" <> commandString <> "'") {cwd = Just testCaseDir}) >>= \case

--                     (ExitSuccess, _, _) -> do
--                         let 
--                             reducedFileName = testCaseDir <> "Bug_hsreduce.hs"
--                             performanceFileName = testCaseDir <> "hsreduce_performance.csv"


--                         Just tokenNumber <- countTokens reducedFileName

--                         [_, timeSpent, _, _, _, byteSize, _, successfulInvocations] <- 
--                             map T.unpack 
--                             . T.words 
--                             . T.map (\c -> if c == ',' then ' ' else c) 
--                             . last 
--                             . T.lines 
--                             <$> TIO.readFile performanceFileName

--                         pure $ Just $ traceShowId (tokenNumber, read byteSize :: Word64, read timeSpent :: Double, read successfulInvocations :: Word64)

--                     e -> do
--                         print e 
--                         pure Nothing

--             let (tn, bs, ts, si) = foldr (\(al, bl, cl, sil) (ar, br, cr, sir) -> (al + ar, bl + br, cl + cr, sil + sir)) (0, 0, 0, 0) results

--             appendFile (fromAbsFile resultFile) $ (intercalate "," [show iterPass, show tn, show bs, show ts, show si] <> "\n")
--     where
--             testCases = 
--                 -- [ "ticket14779" ]
--                 [ "ticket14040",
--                   "ticket14270",   
--                   "ticket14779",
--                   "ticket15696_1",
--                   "ticket16979",   
--                   "ticket18098"]
--                 --   "ticket8763",    
--                 --   "ticket15696_2" ]

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


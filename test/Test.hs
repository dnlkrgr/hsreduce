module Test where

import Data.Foldable
import Path 
import Data.Maybe
import Control.Monad
import Data.Char
-- import Data.Time
import Test.Hspec
-- import Test.QuickCheck

import Util.Util
import Reduce.HsReduce (hsreduce)
import qualified Reduce.Passes.RemoveUnused.Imports as Imports
import qualified Reduce.Passes.RemoveUnused.Pragmas as Pragmas
import qualified Reduce.Passes.RemoveUnused.Exports as Exports
import qualified Reduce.Passes.RemoveUnused.Decls   as Decls
import qualified Reduce.Passes.Stubbing             as Stubbing

-- main :: IO ()
-- main = do
--     startTime <- utctDayTime <$> getCurrentTime
--     let tickets = [ "trying-out" ]
--     -- let tickets = [ "ticket14779", "ticket15696_2", "ticket15753", "ticket14270"]
--     -- let tickets = [ "ticket15753", "ticket14270"]
--     -- let tickets = [ "ticket14040_1", "ticket14040_2", "ticket14270", "ticket14779", "ticket15696_1", "ticket15696_2", "ticket15753", "ticket16979" ]
--     forM_ tickets $ \ticket -> do
--         putStrLn $ "\n[debug] Reducing file: " ++ ticket
--         let 
--             test        = "test-cases/" ++ ticket ++ "/interesting.sh"
--             oldFilePath = "test-cases/" ++ ticket ++ "/" ++ headToUpper ticket ++ ".hs"
--         hsreduce test oldFilePath Nothing
-- 
--     endTime <- utctDayTime <$> getCurrentTime
--     print $ "Testing took " ++ show (round (endTime - startTime) `div` 60 :: Int) ++ " minutes."


testRegressions :: IO ()
testRegressions = undefined
-- testRegressions = hspec $ do
--     let 
--         root    = "/home/daniel/workspace/hsreduce/test-cases/regressions/"
--         test    = fromJust . parseAbsFile $ root <> "interesting.sh"
--         sources = 
--             map (\(src, a, m, e) -> (fromJust . parseAbsFile . (root <>) . (<> ".hs") $ src, a, m, e)) 
--                 [ ("Imports",   Imports.reduce, Nothing, "module Imports where\n")
--                 , ("Pragmas",   Pragmas.reduce, Nothing, "module Pragmas where\n")
--                 , ("Exports",   Exports.reduce, Nothing, "module Exports (\n    ) where\ndata Arst = Brst | Crst\na = 3\nb = const True\nc = \"arst\"\n")
--                 , ("Exports2",  Exports.reduce, Nothing, "module Exports (\n        Arst(..), a, b, c\n    ) where\ndata Arst = Brst | Crst\na = 3\nb = const True\nc = \"arst\"\n")
--                 -- *****
--                 -- Decls
--                 -- *****
--                 , ("Decls",     Decls.reduce,                        Nothing,            "{-# LANGUAGE GADTs #-}\nmodule Decls where\n")
--                 -- deleting only some of the fun ids in a line
--                 , ("FunIds",    minireduce (Decls.rmvDecls Nothing), (Just "funids.sh"), "module FunIds (\n        foo\n    ) where\nfoo x = 3\n")
--                 , ("Cons",      minireduce (fastTryR Decls.rmvCons), Nothing,            "{-# LANGUAGE GADTs #-}\nmodule Cons where\nnewtype Unit\nnewtype RUnit\ndata Arst\ndata Car\ndata Expr a\n")
--                 -- ********
--                 -- Stubbing
--                 -- ********
--                 -- , ("Undefined", minireduce (fastTry Stubbing.expr2Undefined), Nothing, "module Undefined where\nfoo x = undefined\n")
--                 , ("Unit",      minireduce (fastTry Stubbing.type2Unit),      Nothing, "module Unit where\narst :: ()\narst = undefined\n")
--                 ]

    -- TODO: make this parametric, give a list of test cases with their reduce functions and a title
    describe "regressions" $ do
        results <- runIO $ forM sources $ \(src, a, mt, expected) -> do
            let 
                filePath    = fromAbsFile src
                newFilePath = take (length filePath - 3) filePath <> "_hsreduce.hs"
                realTest    = case mt of
                    Nothing -> test
                    Just t  -> fromJust . parseAbsFile $ root <> t


            hsreduce realTest src (Just a)
            fileContent <- readFile newFilePath

            return (drop (length root) filePath, (fileContent, expected))

        forM_ results (\(filePath, (fileContent, expected)) -> it filePath  $ fileContent `shouldBe` expected)


headToUpper :: String -> String
headToUpper [] = []
headToUpper (x : xs) = toUpper x : xs

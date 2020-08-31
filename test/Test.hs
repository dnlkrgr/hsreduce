module Main where

import Data.Foldable
import Path 
import Data.Maybe
import Control.Monad
import Test.Hspec

import Reduce.Driver (hsreduce)
import qualified Reduce.Passes.Remove.Imports        as Imports
import qualified Reduce.Passes.Remove.Pragmas        as Pragmas
import qualified Reduce.Passes.Remove.Exports        as Exports
import qualified Reduce.Passes.Remove.Decls          as Decls
import qualified Reduce.Passes.Remove.Parameters     as Parameters
import qualified Reduce.Passes.Stubbing              as Stubbing
import qualified Reduce.Passes.DataTypes             as DataTypes
import qualified Reduce.Passes.Simplify.Expr as Expr
import qualified Reduce.Passes.Simplify.Types as Types
import Util.Util


main :: IO ()
main = hspec $ do
    let 
        root    = "test-cases/regressions/"
        test    = fromJust . parseRelFile $ root <> "interesting.sh"
        sources = 
            map (\(src, a, m, e) -> (fromJust . parseRelFile . (root <>) $ src, a, m, e)) 
                [ ("Imports",       Imports.reduce,             Nothing,                "module Imports where")
                , ("Pragmas",       Pragmas.reduce,             Nothing,                "module Pragmas where")
                -- Exports: removing explicit exports
                -- Exports2: removing exports starting from implicit export all
                , ("Exports",       Exports.reduce,             Nothing,                "module Exports (\n    ) where\ndata Arst = Brst | Crst\na = 3\nb = const True\nc = \"arst\"")
                , ("Exports2",      Exports.reduce,             Nothing,                "module Exports (\n    ) where\ndata Arst = Brst | Crst\na = 3\nb = const True\nc = \"arst\"")
                -- *****
                -- Decls
                -- *****
                , ("Decls",         Decls.fast,                 Nothing,                "{-# LANGUAGE GADTs #-}\nmodule Decls where\n")
                -- deleting only some of the fun ids in a line
                , ("FunIds",        Decls.slow >> Decls.fast,   (Just "funids.sh"),     "module FunIds (\n        foo\n    ) where\nfoo x = 3")
                , ("Cons",          Decls.slow,                 Nothing,                "{-# LANGUAGE GADTs #-}\nmodule Cons where\nnewtype Unit\nnewtype RUnit\ndata Arst\ndata Car\ndata Expr a\n")
                -- ********
                -- Stubbing
                -- ********
                -- , ("Undefined", minireduce (fastTry Stubbing.expr2Undefined), Nothing, "module Undefined where\nfoo x = undefined\n")
                , ("Unit",          runPass "" Types.type2Unit >> runPass "" Expr.expr2Undefined,            Nothing,                "module Unit where\narst :: ()\narst = undefined")

                , ("Params",        Parameters.reduce,          Nothing,                "module Params where\nbrst = arst\narst :: ()\narst = undefined")
                , ("ConArgs",       DataTypes.rmvConArgs,       Nothing,                "module Arst (\n    ) where\ndata Arst a b = Arst {}\ne :: Arst () () -> Arst () ()\ne (Arst) = Arst")
                , ("InlineTypes",   DataTypes.inline,           Nothing,                "module Inline where\ndata Arst = Arst String\ntype Brst = Int\nf :: String -> ()\nf (\"arst\") = ()\ng :: Int -> ()\ng 3 = ()")
                ]

    -- TODO: make this parametric, give a list of test cases with their reduce functions and a title
    describe "regressions" $ do
        results <- runIO $ forM sources $ \(src, a, mt, expected) -> do
            let 
                filePath    = fromRelFile src
                newFilePath = filePath <> "_hsreduce.hs"
                realTest    = case mt of
                    Nothing -> test
                    Just t  -> fromJust . parseRelFile $ root <> t

            hsreduce 1 (fromRelFile realTest) (fromRelFile src <> ".hs") (Just a)
            fileContent <- readFile newFilePath

            return (drop (length root) filePath, (fileContent, expected))

        forM_ results (\(filePath, (fileContent, expected)) -> it filePath $ fileContent `shouldBe` expected)
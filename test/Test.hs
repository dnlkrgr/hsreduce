module Main where

import Test.HUnit.Lang
import System.Timeout
import qualified Data.Text as T
import Data.Foldable
import Path 
import Data.Maybe
import Control.Monad
import Test.Hspec

import Reduce.Driver (hsreduce)
import qualified Reduce.Passes.TypeFamilies as TypeFamilies
import qualified Reduce.Passes.Imports          as Imports
import qualified Reduce.Passes.Pragmas          as Pragmas
import qualified Reduce.Passes.Exports          as Exports
import qualified Reduce.Passes.Decls                   as Decls
import qualified Reduce.Passes.Parameters       as Parameters
import qualified Reduce.Passes.Stubbing                as Stubbing 
    ( contexts,
      simplifyDeriving,
      simplifyDerivingClause,
      localBinds,
      tyVarBndr,
    )
import qualified Reduce.Passes.Typeclasses    as Typeclasses
import qualified Reduce.Passes.DataTypes      as DataTypes
import qualified Reduce.Passes.Functions      as Functions
    (
      rmvRHSs,
      rmvMatches,
      rmvGuards,
      inline,
      etaReduceMatches,

    )
import qualified Reduce.Passes.Expr  as Expr
import qualified Reduce.Passes.Types as Types
import Util.Util


main :: IO ()
main = hspec $ do
    let 
        root    = "test-cases/regressions/"
        test    = fromJust . parseRelFile $ root <> "interesting.sh"
        sources = 
            map (\(src, a, m, e) -> (fromJust . parseRelFile . (root <>) $ src, a, m, e)) 
                [ ("Imports",       
                    runPass Imports.rmvImports,             
                    Nothing,                
                    "\nmodule Imports where\n")
                , ("Pragmas",       
                    Pragmas.reduce,             
                    Nothing,                
                    "\nmodule Pragmas where\n")
                -- Exports: removing explicit exports
                -- Exports2: removing exports starting from implicit export all
                , ("Exports",       
                    Exports.reduce,             
                    Nothing,                
                    "\nmodule Exports (\n    ) where\ndata Arst = Brst | Crst\na = 3\nb = const True\nc = \"arst\"\n")
                , ("Exports2",      
                    Exports.reduce,             
                    Nothing,                
                    "\nmodule Exports (\n    ) where\ndata Arst = Brst | Crst\na = 3\nb = const True\nc = \"arst\"\n")
                -- *****
                -- Decls
                -- *****
                , ("Decls",         
                    runPass (Decls.rmvDecls Nothing),                 
                    Nothing,                
                    "{-# LANGUAGE GADTs #-}\nmodule Decls where\n")
                -- deleting only some of the fun ids in a line
                , ("FunIds",        
                    mapM_ runPass [Decls.rmvSigs Nothing, Decls.rmvDecls Nothing],   
                    (Just "funids.sh"),     
                    "\nmodule FunIds (\n        foo\n    ) where\nfoo x = 3\n")
                , ("Cons",          
                    runPass (Decls.simplifyDecl Nothing),                 
                    Nothing,                
                    "{-# LANGUAGE GADTs #-}\nmodule Cons where\nnewtype Unit\nnewtype RUnit\ndata Arst\ndata Car\ndata Expr a\n")
                -- ********
                -- Stubbing
                -- ********
                -- , ("Undefined", minireduce (fastTry Stubbing.expr2Undefined), Nothing, "module Undefined where\nfoo x = undefined\n")
                , ("Unit",          
                    mapM_ runPass [Types.type2Unit, Expr.expr2Undefined],            
                    Nothing,                
                    "\nmodule Unit where\narst :: ()\narst = undefined\n")
                , ("Contexts",   
                    runPass Stubbing.contexts,           
                    Nothing,                
                    "\nmodule Contexts where\narst :: () => a -> a\narst = undefined\n")
                , ("Deriving",   
                    runPass Stubbing.simplifyDeriving,           
                    Nothing,                
                    "\nmodule Deriving where\ndata Arst = Arst\n")
                , ("Deriving2",   
                    runPass Stubbing.simplifyDerivingClause,           
                    Nothing,                
                    "\nmodule Deriving where\ndata Arst\n  = Arst\n  deriving ()\n")
                , ("TyVarBndr",   
                    runPass Stubbing.tyVarBndr,           
                    Nothing,                
                    "{-# LANGUAGE KindSignatures, PolyKinds #-}\nmodule TyVarBndr where\ndata Arst a b\n")
                , ("LocalBinds",   
                    runPass Stubbing.localBinds,           
                    Nothing,                
                    "\nmodule LocalBinds where\narst = undefined\n")
                , ("Matches",   
                    runPass Functions.rmvMatches,           
                    Nothing,                
                    "\nmodule Matches where\n")
                , ("Guards",   
                    runPass Functions.rmvGuards,           
                    Nothing,                
                    "\nmodule Guards where\narst = \"crst\"\n")
                , ("RHSs",   
                    runPass Functions.rmvRHSs,           
                    Nothing,                
                    "\nmodule RHSs where\narst | 3 > 5 = \"arst\"\n")
                , ("TypeFamilies",   
                    TypeFamilies.apply,           
                    Just "typefamilies.sh",                
                    "{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DeriveGeneric, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, InstanceSigs, PolyKinds, RankNTypes, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}\nimport GHC.Generics\nimport GHC.TypeLits\narst :: Int -> String\narst = undefined\nbrst :: String\nbrst = undefined\ntype family F a b where\n  F a b = a\ntype family G a b where\n  G a b = String\ntype family MaybeAdd b where\n  MaybeAdd b = 'Just (b)\ntype family AnotherLookupParam (p :: Nat) :: Maybe Nat where\n  AnotherLookupParam n = MaybeAdd 1\ntype family LookupParam (p :: Nat) :: Maybe Nat where\n  LookupParam n = ('Just 0)\ntype family IfEq (b :: k) (t :: l) (f :: l) :: l where\n  IfEq a t _ = t\nmain = undefined\n")
                , ("Expr",   
                    mapM_ runPass [Expr.filterExprSubList, Expr.simplifyExpr],
                    Just "expr.sh",                
                    "\nmodule Expr where\nmain = do brst\nbrst = undefined\ncrst = \"arst\"\n")
                , ("Functions",   
                    mapM_ runPass [Functions.etaReduceMatches, Functions.inline],
                    Nothing,                
                    "\nmodule Functions where\narst = \"arst\"\nbrst = \"arst\"\ndrst = erst\nerst v = \"the end\"\n")
                , ("ConArgs",       
                    runPass DataTypes.rmvConArgs,       
                    Nothing,                
                    "\nmodule Arst (\n    ) where\ndata Arst a b = Arst {}\ne :: Arst () () -> Arst () ()\ne (Arst) = Arst\n")
                , ("InlineTypes",   
                    runPass DataTypes.inline,           
                    Nothing,                
                    "\nmodule Inline where\ndata Arst = Arst String\ntype Brst = Int\nf :: String -> ()\nf (\"arst\") = ()\ng :: Int -> ()\ng 3 = ()\n")
                , ("Params",        
                    runPass Parameters.reduce,          
                    Just "params.sh",
                    "\nmodule Params where\nbrst = arst '1' \"3\"\narst :: Char -> String -> ()\narst '4' \"6\" = undefined\ncrst = undefined <@@> [3]\n_ <@@> rhs = undefined\ntoListOf l = foldrOf l\nfoldrOf l = undefined . l\n")
                , ("Typeclasses",   
                    runPass Typeclasses.rmvTyClMethods,
                    Just "typeclasses.sh",                
                    "\nmodule Typeclasses where\nmain = do putStrLn $ arst (3 :: Int)\nclass Arst a where\n  arst :: a -> String\ninstance Arst Int where\n  arst = show\n")
                , ("MultiParams",   
                    runPass Typeclasses.handleMultiParams,
                    Just "multiparams.sh",                
                    "{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}\nmodule MultiParams where\nclass Arst where\n  inRelation :: a -> b -> Bool\ninstance Arst where\n  inRelation _ _ = True\n")
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

            timeout (10 * 1000 * 1000) (hsreduce [a] 1 (fromRelFile realTest) (fromRelFile src <> ".hs")) >>= \case
                Nothing -> assertFailure "test case timed out"
                Just () -> return ()

            fileContent <- readFile newFilePath

            return (drop (length root) filePath, (fileContent, expected))


        forM_ results (\(filePath, (fileContent, expected)) -> it filePath $ fileContent `shouldBe` expected)

-- realFloor :: T.Text
-- realFloor = "Not in scope: ‘GHC.Real.floor’\nNo module named ‘GHC.Real’ is imported."
--
-- hiddenModule :: T.Text
-- hiddenModule = "Could not load module ‘Data.HashMap.Base’\nit is a hidden module in the package ‘unordered-containers-0.2.10.0’\nit is a hidden module in the package ‘unordered-containers-0.2.10.0’\nUse -v to see a list of the files searched for."
--
-- "Not in scope:\n  type constructor or class \u2018Data.Aeson.Types.ToJSON.ToJSON\u2019\nPerhaps you meant one of these:\n  \u2018Data.Aeson.Types.GToJSON\u2019 (imported from Data.Aeson.Types),\n  \u2018Data.Aeson.Types.GToJSON\u2019 (imported from Data.Aeson.Types),\n  \u2018Data.Aeson.Types.ToJSON\u2019 (imported from Data.Aeson.Types)\nNo module named \u2018Data.Aeson.Types.ToJSON\u2019 is imported."
--
-- -- noSuchModule :: T.Text
-- -- noSuchModule = "Not in scope: ‘Data.Binary.Put.runPutM’\nNo module named ‘Data.Binary.Put’ is imported."
--
--
dataConstructorNotInScope :: T.Text
dataConstructorNotInScope = "\8226 Data constructor not in scope:\n    Closed :: (UnBounded Integer :+ ()) -> EndPoint (UnBounded r :+ ())\n\8226 Perhaps you meant one of these:\n    ‘Data.Range.Closed’ (imported from Data.Range),\n    variable ‘Data.Range.isClosed’ (imported from Data.Range)"

--
--
simple :: T.Text
simple = "Not in scope:\n type constructor or class ‘Text.ProtocolBuffers.Extensions.GPB’\n Perhaps you meant ‘Text.ProtocolBuffers.Header.GPB’ (imported from Text.ProtocolBuffers.Header)\n No module named ‘Text.ProtocolBuffers.Extensions’ is imported."

--
-- noModuleNamed :: T.Text
-- noModuleNamed = "Not in scope:\n type constructor or class ‘Text.ProtocolBuffers.TextMessage.TextType’\n Perhaps you meant one of these:\n ‘Text.ProtocolBuffers.Header.TextType’ (imported from Text.ProtocolBuffers.Header),\n ‘Text.ProtocolBuffers.WireMessage.Get’ (imported from Text.ProtocolBuffers.WireMessage)\n No module named ‘Text.ProtocolBuffers.TextMessage’ is imported."
--
-- importedFrom :: T.Text
-- importedFrom = "Not in scope: type constructor or class ‘Data’\nPerhaps you meant ‘Text.ProtocolBuffers.Header.Data’ (imported from Text.ProtocolBuffers.Header)"

variableNotInScope :: T.Text
variableNotInScope =
    "\8226 Variable not in scope:\n    maybe\n      :: t0\n         -> t1 -> Maybe Int -> Either Int Utf8_TextProtocolBuffersBasic\n  "
        <> "\8226 Perhaps you meant ‘Prelude.maybe’ (imported from Prelude)"

--
--
--
-- -- getModuleName :: T.Text -> T.Text
-- -- getModuleName = T.intercalate "." . fromMaybe [] . safeInit . T.words . T.map (\c -> if c == '.' then ' ' else c) . trace'' "getModuleName" show
--
-- -- this isn't exactly like the init from Prelude
-- -- safeInit :: [a] -> Maybe [a]
-- -- safeInit [] = Nothing
-- -- safeInit xs = Just $ init xs

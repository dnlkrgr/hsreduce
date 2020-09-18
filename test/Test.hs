module Main where

import qualified Data.Text as T
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
    ( contexts,
      simplifyDeriving,
      simplifyDerivingClause,
      localBinds,
      rmvRHSs,
      rmvMatches,
      rmvGuards,
      tyVarBndr,
    )
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
                [ ("Imports",       
                    runPass Imports.rmvImports,             
                    Nothing,                
                    "module Imports where")
                , ("Pragmas",       
                    Pragmas.reduce,             
                    Nothing,                
                    "module Pragmas where")
                -- Exports: removing explicit exports
                -- Exports2: removing exports starting from implicit export all
                , ("Exports",       
                    Exports.reduce,             
                    Nothing,                
                    "module Exports (\n    ) where\ndata Arst = Brst | Crst\na = 3\nb = const True\nc = \"arst\"")
                , ("Exports2",      
                    Exports.reduce,             
                    Nothing,                
                    "module Exports (\n    ) where\ndata Arst = Brst | Crst\na = 3\nb = const True\nc = \"arst\"")
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
                    "module FunIds (\n        foo\n    ) where\nfoo x = 3")
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
                    "module Unit where\narst :: ()\narst = undefined")
                , ("Params",        
                    runPass Parameters.reduce,          
                    Nothing,                
                    "module Params where\nbrst = arst\narst :: ()\narst = undefined")
                , ("ConArgs",       
                    runPass DataTypes.rmvConArgs,       
                    Nothing,                
                    "module Arst (\n    ) where\ndata Arst a b = Arst {}\ne :: Arst () () -> Arst () ()\ne (Arst) = Arst")
                , ("InlineTypes",   
                    runPass DataTypes.inline,           
                    Nothing,                
                    "module Inline where\ndata Arst = Arst String\ntype Brst = Int\nf :: String -> ()\nf (\"arst\") = ()\ng :: Int -> ()\ng 3 = ()")
                , ("Contexts",   
                    runPass Stubbing.contexts,           
                    Nothing,                
                    "module Contexts where\narst :: () => a -> a\narst = undefined")
                , ("Deriving",   
                    runPass Stubbing.simplifyDeriving,           
                    Nothing,                
                    "module Deriving where\ndata Arst = Arst")
                , ("Deriving2",   
                    runPass Stubbing.simplifyDerivingClause,           
                    Nothing,                
                    "module Deriving where\ndata Arst\n  = Arst\n  deriving ()")
                , ("TyVarBndr",   
                    runPass Stubbing.tyVarBndr,           
                    Nothing,                
                    "{-# LANGUAGE KindSignatures, PolyKinds #-}\nmodule TyVarBndr where\ndata Arst a b\n")
                , ("LocalBinds",   
                    runPass Stubbing.localBinds,           
                    Nothing,                
                    "module LocalBinds where\narst = undefined")
                , ("Matches",   
                    runPass Stubbing.rmvMatches,           
                    Nothing,                
                    "module Matches where")
                , ("Guards",   
                    runPass Stubbing.rmvGuards,           
                    Nothing,                
                    "module Guards where\narst = \"crst\"")
                , ("RHSs",   
                    runPass Stubbing.rmvRHSs,           
                    Nothing,                
                    "module RHSs where\narst | 3 > 5 = \"arst\"")
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

            hsreduce [a] 1 (fromRelFile realTest) (fromRelFile src <> ".hs")
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

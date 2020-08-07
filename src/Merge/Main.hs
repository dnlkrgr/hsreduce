module Main (main) where

import System.Environment
import HsAllInOne

main :: IO ()
main =  getArgs >>= \case
    [x]     -> do
        hsmerge x
        dieForGhcSins
    _       -> putStrLn $ unlines [
                      "Example Usage:" 
                    , "1. Call hsmerge in the cabal project directory you want to merge"
                    , "2. hsmerge expects an hie.yaml file"
                    , "3. Example usage: \"hsmerge src/Text/Docx.hs\""
                    ]

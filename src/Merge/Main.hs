module Main (main) where

import System.Environment
import Merge.HsAllInOne

main :: IO ()
main = do 
    x <- head <$> getArgs
    hsmerge x
    dieForGhcSins

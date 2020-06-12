module Main (main) where

import System.Environment
import Merge.HsAllInOne

main :: IO ()
main = hsmerge =<< head <$> getArgs

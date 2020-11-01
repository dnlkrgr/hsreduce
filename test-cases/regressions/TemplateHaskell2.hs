{-# language TemplateHaskell #-}

module TemplateHaskell where

import Language.Haskell.TH
import TemplateHaskellUtil

cnst1 = $(cnst 1 "x")
cnst2 = $(cnst 2 "str")
cnst20 = $(cnst 20 "foo")


main :: IO Int
main = return ()
-- main = do print (cnst1 11)
-- 
--           print (cnst2 11 12)
-- 
--           runQ(cnst 1 "x") >>= print
-- 
--           runQ(cnst 2 "str") >>= print
-- 
--           runQ(cnst 20 "foo") >>= print
-- 
--           runQ(cnst 1 "x") >>= putStrLn.pprint
-- 
--           runQ(cnst 2 "str") >>= putStrLn.pprint
-- 
--           runQ(cnst 20 "foo") >>= putStrLn.pprint

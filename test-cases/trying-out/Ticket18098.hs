{-# language GADTs #-}
module Arst where

import Data.Void

data Expr a where
    Brst :: Int -> Expr Int
    Crst :: String -> Expr String
    Drst :: Expr Void

arst :: Expr () -> String
arst (Brst n) = show n
arst (Crst s) = s
-- arst :: Arst () -> String
-- arst Brst = "int"
-- arst Crst = "string"

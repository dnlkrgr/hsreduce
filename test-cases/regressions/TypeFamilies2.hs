{-# LANGUAGE TypeFamilies #-}

module TypeFamilies2 where

arst :: F Int Char -> String
arst = undefined

type family F a b where
  F a b = a

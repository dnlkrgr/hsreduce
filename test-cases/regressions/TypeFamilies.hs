{-# language TypeFamilies #-}

main = undefined

type family F a b where
  F a b = a

arst :: F Int Char -> String
arst = undefined

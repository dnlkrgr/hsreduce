{-# language KindSignatures #-}
{-# language PolyKinds #-}
module TyVarBndr where

data Arst (a :: k) (b :: * -> *)

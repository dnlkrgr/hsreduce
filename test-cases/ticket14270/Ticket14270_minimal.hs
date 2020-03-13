{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeInType #-}
module DataTypeableInternal where

data TypeRep (a :: c) where
  TrFun :: TypeRep (() -> ())

data IsApp c where
  IsApp :: TypeRep e -> IsApp (e ())

g :: TypeRep a -> IsApp a
g TrFun = IsApp undefined 

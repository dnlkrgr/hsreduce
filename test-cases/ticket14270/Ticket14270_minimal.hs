{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeInType #-}
module DataTypeableInternal (g) where

data TypeRep (a :: c) where
  TrFun :: a -> TypeRep b -> TypeRep (a -> b)

data IsApp c where
  IsApp :: TypeRep e -> TypeRep f -> IsApp (e f)

g :: TypeRep a -> Maybe (IsApp a)
g (TrFun _ _) = Just (IsApp undefined undefined)

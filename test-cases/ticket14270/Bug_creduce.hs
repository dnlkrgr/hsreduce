{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeInType #-}
module DataTypeableInternal
where
import GHC.Fingerprint 
data TypeRep (a :: c) where
  TrFun ::
    Fingerprint ->
    a ->
    TypeRep b ->
    TypeRep (a -> b)
d =
    undefined
data IsApp c where
  IsApp ::
    TypeRep e ->
    TypeRep f ->
    IsApp (e f)
g ::
  TypeRep a ->
  Maybe (IsApp a)
g (TrFun h a b) = Just (IsApp (d a) b)

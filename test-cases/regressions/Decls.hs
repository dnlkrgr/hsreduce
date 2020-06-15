{-# language GADTs #-}
module Decls where

newtype Unit = Unit ()

newtype RUnit = RUnit { getUnit :: () }

data Arst = Brst | Crst

data Car = Car {
      numWheels :: Int
    , color :: String
    , arst :: ()
    }
     

data Expr a where
    I   :: Int  -> Expr Int
    B   :: Bool -> Expr Bool
    Add :: Expr Int -> Expr Int -> Expr Int
    Mul :: Expr Int -> Expr Int -> Expr Int
    Eq  :: Eq a => Expr a -> Expr a -> Expr Bool

class MyClass a where
    brst :: a -> Int

instance Show Arst where
    show = undefined

default ()

foo, goo :: Int -> Int
foo x = 3
goo y = 5

bar = undefined


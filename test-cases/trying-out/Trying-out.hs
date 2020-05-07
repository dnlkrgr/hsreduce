{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language Rank2Types, RankNTypes #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- shifting things into turbo
{-# LANGUAGE TypeFamilies          #-}
{-# language PolyKinds #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE GADTs  #-}
{-# LANGUAGE TypeApplications  #-}
module Main where 

import Data.Kind
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString (ByteString)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Text (Text)
import Data.Data (gmapT)

data Empty

data T = T {-# UNPACK #-} !Float

data Dumb = Dumb | AlsoDumb | MoreDumb | T4 | T5

data DumbProduct = StillDumb Int String

data DumbRec = DumbRec {
  a :: Int
  , b :: String
  }

data Expr a where
    I   :: Int  -> Expr Int
    B   :: Bool -> Expr Bool
    Add :: Expr Int -> Expr Int -> Expr Int
    Mul :: Expr Int -> Expr Int -> Expr Int
    Eq  :: Eq a => Expr a -> Expr a -> Expr Bool


main :: IO ()
main = do
  let arst = myFun Nothing
  return ()

myFun :: Maybe Int -> Int
myFun (Just y) = 
  let a = 2
      b = 3
      c = 5
      d = 7
      e = 9
  in 2 * y + a + c + d + e
myFun Nothing = 42
  where x = 43

instance Show Dumb where
  show Dumb = "Dumb"

-- constructing and updating records

rec1 = DumbRec 3 "arst"
rec2 = DumbRec { a = 3, b = "arst"}
rec3 = DumbRec { a = 3, b = undefined }
rec4 = rec1 { a = 4, b = undefined }

-- functions with undefined RHS / guards
brst :: a -> a
brst = undefined

crst
  | undefined > 0 = 3
  | otherwise = undefined

drst
  | undefined > 0 = 3
  | 2 < 3 = 4
  | otherwise = undefined

-- reduction of single case-of expressions
erst a b c d = case a of
  Nothing ->
    if b
      then c
      else d

-- function inlining
frst = grst Nothing

grst Nothing = 2 * n + 3 / 5
  where n = 5

hrst = irst 5 7

-- this is dumb; if the user does this, it's their fault
-- we take the first match
irst x y = 2 * x + 3 / y
irst x y = 5 * x + 3 / y

jrst = krst 3

krst n
  | n > 0 = "great"
  | True  = "not great"

lrst = mrst 3

mrst = (*2)

nrst = orst (Just 5) 2

orst Nothing  n = 2 * n + 3
orst (Just x) n = 2 * n + x

prst
  | False = (*2)
  | True  = (*5)

qrst = prst 3

-- data families

data family A -- == data family A :: Type
data instance A = Arst

data family B e
data instance B e = Brst | Crst

data family C d e
data instance C Int Char = Drst | Erst
data instance C Char String = Frst | Grst

data family D :: Type -> Type
-- == data family D a :: Type
data instance D Char = Hrst

data family MyMaybe a
data instance MyMaybe a = MyNothing | MyJust a

f :: Int -> MyMaybe String
f 0 = MyNothing
f _ = MyJust "great!"

data family E (m :: Type) :: Type
data instance E Int = Irst

data family F (m :: Type -> Type) :: Type
-- == data family F :: (Type -> Type) -> Type
data instance F Maybe = Jrst

-- newtype is also allowed:
data family G (e :: k)
newtype instance G e = N Int 

data family DExpr a
data instance DExpr a where
  DI :: Int -> DExpr Int
  DB :: Bool -> DExpr Bool

-- open type family
type family TA
type instance TA = Int

type family TB a
type instance TB Int = Char
-- type instance TB a = String

-- closed type family
type family TD a b where
  TD Int Bool = String
  TD Char (Maybe String) = String

-- associated type family
class CA a where
  type TC a :: Type

instance CA Int where
  type TC Int = String

class CB a b where
  type TCD a (b :: k)

instance CB Int Maybe where
  type TCD Int Maybe = Char

-- misc type families

type family F1 a where
  F1 Int = Bool

sillyId :: F1 Char -> F1 Char
sillyId x = x

data SillyGadt a where
  MkSillyGadt1 :: Int   -> SillyGadt (F1 Char)
  MkSillyGadt2 :: Float -> SillyGadt (F1 Double)

instance Show (SillyGadt a) where
  show (MkSillyGadt1 i) = "MkSillyGadt1 " ++ show i
  show (MkSillyGadt2 f) = "MkSillyGadt2 " ++ show f

-- type applications
isEven = (== 0) . (`mod` 2)

arst = map @Int @Bool isEven

answer_read = show (read @Int "3") -- "3" :: String
answer_show = show @Integer (read "5") -- "5" :: String
answer_showread = show @Int (read @Int "7") -- "7" :: String

-- weird stuff
data FunArrow = (:->) | (:~>)

(<&&>) = (&&)
infixr 8 <&&>

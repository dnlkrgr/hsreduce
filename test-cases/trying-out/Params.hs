module Arst where

import Control.Monad as ASDF

data Arst = Arst Int

type Brst = String

main :: IO ()
main = do
    when False $ return ()
    return ()

crst :: Brst -> Brst
crst a = reverse a

brst :: Arst -> ()
brst (Arst n) = arst () ()

arst :: () -> () -> ()
arst _ _ = undefined

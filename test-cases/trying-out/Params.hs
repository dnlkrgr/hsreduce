module Arst where

import Control.Monad as ASDF

data Arst = Arst Int

main :: IO ()
main = do
    when False $ return ()
    return ()

brst :: Arst -> ()
brst (Arst n) = arst () ()

arst :: () -> () -> ()
arst _ _ = undefined

{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecordPuns #-}
import Foo hiding (foo)
import qualified Foo

import Data.ByteString.Char8

foo = "Hello"

main = Prelude.putStrLn $
    mark $
    unpack (Data.ByteString.Char8.pack foo) <> Foo.foo
  where Record{ mark = mark } = r

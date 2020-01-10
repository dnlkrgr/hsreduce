module Foo where
x <> y = x ++ ", " ++ y
data Record = Record {mark :: String -> String}
r = Record {mark = (++ "!")}
foo = "world"
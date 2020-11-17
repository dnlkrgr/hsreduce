module Functions where

main = do
    print g
    print $ h 3
    print $ j 'a'

f :: String
f = (\x -> "ghi") 5

g :: String
g =  (\x y -> "ghi") 5 'b'

h v = i v
i v = "xyz"

j v = k "def"
k v = 3

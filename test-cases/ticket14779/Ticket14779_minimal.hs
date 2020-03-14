module DataFixed where
data DataType = DataType
                        { a :: DataRep
                        }
data Constr = Constr
                        { b :: String
                        }
data DataRep = AlgRep [Constr]
data       Int
  = Prefix
d l cs = DataType
                        { a = cs
                        }
e dt l f g = head [ h | (c,h) <- m dt `zip` [],
                     i c == l ]
m dt = case a dt of
                        AlgRep cons -> cons
i = b
j = d "" k
k = e j "MkFixed" [] Prefix

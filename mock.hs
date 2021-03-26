main :: IO ()
main = print $ ego I 

data Bird = M | C Bird Bird | I
   deriving (Show, Eq)

class Call a where
   call :: a -> a -> a

instance Call Bird where
   call M x = call x x
   call (C a b) x = call a (call b x)  
   call I x = x

fond :: Bird -> Bird -> Bool
fond a b = call a b == b

ego :: Bird -> Bool
ego a = call a a == a

agree :: Bird -> Bird -> Bird -> Bool
agree a b x = call a x == call b x

match :: Bird -> Bird -> Bird -> Bird -> Bool
match a b x y = call a x == y && call b y == x

happy :: Bird -> Bird -> Bird -> Bool
happy a x y = match a a x y

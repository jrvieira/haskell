import Control.Arrow

a = [-1,3,8,2,9,5]
b = [4,1,2,10,5,20]
t = 24 -- TARGET

main = print solution
 where
  solution = fmin $ (\x y -> ((x,y),abs$t-(x+y))) <$> a <*> b

fmin :: [(a,Int)] -> [a]
fmin = go [] maxBound
 where
  go acc _ [] = acc
  go acc n ((a,x):as)
   | x <  n = go (a:[] ) x as
   | x == n = go (a:acc) n as
   | True   = go    acc  n as

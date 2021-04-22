main :: IO ()
main = print $ length (s [0..pred n]) == 2^n
   where
   n = 32

s [] = [[]]
s (x:xs) = map (x :) ss ++ ss
   where
   ss = s xs


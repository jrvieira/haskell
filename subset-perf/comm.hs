main :: IO ()
main = print $ length (s [0..pred n]) == 2^n
   where
   n = 32

s [] = [[]]
s (x:xs) = ss x (s xs)
   where
   ss _ [] = []
   ss x (y:ys) = (x:y) : y : ss x ys

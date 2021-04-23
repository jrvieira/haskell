main :: IO ()
main = print $ length (s [0..pred n]) == 2^n
   where
   n = 27

s = ss []
      where
      ss a [] = [a]
      ss a (x:xs) = ss (x : a) xs ++ ss a xs


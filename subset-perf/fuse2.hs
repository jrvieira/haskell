subsets [] = [[]]
subsets (x:xs) = ss x (subsets xs)
   where
   ss _ [] = []
   ss x (y:ys) = (x:y) : y : ss x ys

subsets [] = []
subsets (x:xs) = [x] : foldr f [] (subsets xs)
   where
   f ys r = ys : (x : ys) : r


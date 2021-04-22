-- SUBSETS performance test
-- ghci: length $ sX [0..19] -- eee
-- bench: length $ sX [0..26] -- ubuntu vm imac

-- naive
s0 = s
   where
   s [] = [[]]
   s (x:xs) = map (x :) (s xs) ++ (s xs)

-- ~ 15 s
-- ~ 13 s

-- memoized
s1 = s
   where
   s [] = [[]]
   s (x:xs) = map (x :) ss ++ ss
      where
      ss = s xs
-- ~ 1.5 s
-- ~ 13 s

-- iterative
s2 = s
   where
   s [] = [[]]
   s (x:xs) = ss x (s xs)
      where
      ss _ [] = []
      ss x (y:ys) = (x:y) : y : ss x ys
-- ~ 3.2 s
-- ~ 1.5 s


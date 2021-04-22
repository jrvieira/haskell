-- SUBSETS performance test
-- ghci: length $ sX [0..19] -- eee
-- ghc -O0,O1,O2: length $ sX [0..26] -- ubuntu vm imac (bench)

-- naive
s0 = s
   where
   s [] = [[]]
   s (x:xs) = map (x :) (s xs) ++ (s xs)
-- ~ 15 s <- ghci probably computes (s xs) twice
-- ~ 56 s
-- ~ 14 s <- -O1 optimize with memoization
-- ~ 13 s

-- memoized
s1 = s
   where
   s [] = [[]]
   s (x:xs) = map (x :) ss ++ ss
      where
      ss = s xs
-- ~ 1.5 s <- memoization is awesome
-- ~ 15 s
-- ~ 13 a
-- ~ 13 s

-- iterative
s2 = s
   where
   s [] = [[]]
   s (x:xs) = ss x (s xs)
      where
      ss _ [] = []
      ss x (y:ys) = (x:y) : y : ss x ys
-- ~ 3 s <- no idea why this is slower than s1
-- ~ 1.8 s
-- ~ 1.5 s
-- ~ 1.5 s <- by far the fastest solution with compiler optimizations


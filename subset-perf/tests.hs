-- SUBSETS performance test
-- ghci: length $ sX [0..19]
-- ubuntu vm on imac: length $ sX [0..20]

-- naive
s0 = s
   where
   s [] = [[]]
   s (x:xs) = map (x :) (s xs) ++ (s xs)
-- (15.03 secs, 893,439,844 bytes)
-- (14.98 secs, 893,439,844 bytes)
-- (14.93 secs, 893,439,844 bytes)

-- memoized
s1 = s
   where
   s [] = [[]]
   s (x:xs) = map (x :) ss ++ ss
      where
      ss = s xs
-- (1.56 secs, 75,552,320 bytes)
-- (1.59 secs, 75,552,320 bytes)
-- (1.57 secs, 75,552,320 bytes)

-- cummulative
s2 = s
   where
   s [] = [[]]
   s (x:xs) = ss x (s xs)
      where
      ss _ [] = []
      ss x (y:ys) = (x:y) : y : ss x ys
-- (3.29 secs, 146,855,340 bytes)
-- (3.27 secs, 146,855,340 bytes)
-- (3.27 secs, 146,855,340 bytes)


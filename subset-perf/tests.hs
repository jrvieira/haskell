import Control.Monad

-- SUBSETS performance test
-- ghci: length $ sX [0..19] -- eee
-- ghc -O0,O1,O2: length $ sX [0..26] -- ubuntu vm imac (bench)

{- NOTE
   This problem is best solved by listing 2^n binary numbers:
   0 => [] , 1 => [0] , 10 => [1] , 11 => [0,1] , ...
   110 => [1,2] , 111 => [0,1,2] , ...
-}

-- naive
s0 = s
   where
   s [] = [[]]
   s (x:xs) = map (x :) (s xs) ++ (s xs)
-- ~15 s <- ghci probably computes (s xs) twice
-- ~56 s
-- ~14 s <- -O1 optimize with memoization
-- ~13 s

-- memoized
s1 = s
   where
   s [] = [[]]
   s (x:xs) = map (x :) ss ++ ss
      where
      ss = s xs
-- ~ 1 s <- memoization is awesome
-- ~15 s
-- ~13 a
-- ~13 s

-- iterative
s2 = s
   where
   s [] = [[]]
   s (x:xs) = ss x (s xs)
      where
      ss _ [] = []
      ss x (y:ys) = (x:y) : y : ss x ys
-- ~ 3 s <- ghci uses compiled map, so s1 is faster
-- ~ 2 s
-- ~ 1 s
-- ~ 1 s

-- monadic
s3 = filterM (const [True, False])
-- ~ 2 s

-- tomsmeding on irc
s4 = s
   where
   s = ss []
      where
      ss a [] = [a]
      ss a (x:xs) = ss (x : a) xs ++ ss a xs
-- ~ 7 s


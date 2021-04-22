{- PROBLEM

   Total number of distinct groupings
   of N elements separated in G groups:

   N = 3
   G = 2

   012
   12 0
   02 1
   01 2
   2 01
   1 02
   0 12
    012

   r = 8

   NOTE:

   12 0 == 21 0
   12 0 /= 0 12

-}

main :: IO ()
main = do
   mapM_ print $ subsets [0..4]

-- for G = 2 , solution == subsets N
subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = map (x :) subs ++ subs
   where
   subs = subsets xs

-- for G > 2
comb = undefined

{- CONCLUSION

   length (subs n g) == g ^ n

   n = number of places
   g = base

   ex: subs 4 2 = 0000 -> 1111 => 2^4 = 16
       subs 2 10 = 00 -> 99 => 10^2 = 100

   0001 => first 3 elements in group 0, last in group 1
   29 => first element in group 2, second in group 9 ...

-}


import Criterion.Main
import Control.Monad

main :: IO ()
main = defaultMain [
   bgroup "subsets" [
      bench "naive" $ whnf (length . s0) [0..n] ,
      bench "memo" $ whnf (length . s1) [0..n] ,
      bench "fuse" $ whnf (length . s2) [0..n] ,
      bench "monad" $ whnf (length . s3) [0..n] ,
      bench "tomsmeding" $ whnf (length . s4) [0..n]
      ]
   ]
   where
   n = 19

-- naive
s0 = subsets
   where
   subsets [] = [[]]
   subsets (x:xs) = map (x :) (subsets xs) ++ (subsets xs)

-- memoized
s1 = subsets
   where
   subsets [] = [[]]
   subsets (x:xs) = map (x :) ss ++ ss
      where
      ss = subsets xs

-- fused
s2 = subsets
   where
   subsets [] = [[]]
   subsets (x:xs) = ss x (subsets xs)
      where
      ss _ [] = []
      ss x (y:ys) = (x:y) : y : ss x ys

-- monadic
s3 = filterM (const [True, False])

-- tomsmeding on irc
s4 = subsets
   where
   subsets = ss []
      where
      ss a [] = [a]
      ss a (x:xs) = ss (x : a) xs ++ ss a xs


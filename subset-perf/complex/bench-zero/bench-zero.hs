{-# LANGUAGE BangPatterns #-}

import System.CPUTime
import Control.Monad
import Diagrams.Draw

main :: IO ()
main = do
   results <- sequence $ flip bench ns <$> [naive,memo,fuse,monad,tomsmeding]
   print results
   draw results
   where
   ns = [20..27]

bench f ns = mapM f' ns
   where
   f' n = do
      start <- getCPUTime
      let !done = length (f [0..pred n]) == 2^n
      end <- getCPUTime
      let elapse = end - start
      pure $ if done then elapse else 0

-- naive
naive = subsets
   where
   subsets [] = [[]]
   subsets (x:xs) = map (x :) (subsets xs) ++ (subsets xs)

-- memoized
memo = subsets
   where
   subsets [] = [[]]
   subsets (x:xs) = map (x :) ss ++ ss
      where
      ss = subsets xs

-- fused
fuse = subsets
   where
   subsets [] = [[]]
   subsets (x:xs) = ss x (subsets xs)
      where
      ss _ [] = []
      ss x (y:ys) = (x:y) : y : ss x ys

-- monadic
monad = filterM (const [True, False])

-- tomsmeding on irc
tomsmeding = subsets
   where
   subsets = ss []
      where
      ss a [] = [a]
      ss a (x:xs) = ss (x : a) xs ++ ss a xs


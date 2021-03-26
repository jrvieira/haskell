{-# LANGUAGE BangPatterns #-}

module Main where

import Data.List
import Data.Bits

main = print "ok"

fib0 :: Int -> Integer
fib0 = (f !!)
   where
   f = scanl (+) 0 (1 : f)

fib1 :: Int -> Integer
fib1 = (f !!)
   where
   f = 0 : 1 : zipWith (+) f (tail f)

-- strict eval ~= 2x faster
fib2 :: Int -> Integer
fib2 = go 0 1
  where
  go !a !b n | n == 0    = a
             | otherwise = go b (a + b) (n - 1)

-- slow
fib3 :: Int -> Integer
fib3 = f
   where
   f = (map go [0..] !!)
   go 0 = 0
   go 1 = 1
   go n = (f $ n - 2) + (f $ n - 1)

-- reallly fast
fib4 :: Int -> Integer
fib4 n = snd . foldl' fib_ (1, 0) . dropWhile not $ [testBit n k | k <- let s = bitSize n in [s-1,s-2..0]]
   where
   fib_ (f, g) p
      | p         = (f*(f+2*g), ss)
      | otherwise = (ss, g*(2*f-g))
      where
      ss = f*f+g*g

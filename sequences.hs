module Main (main) where

import Debug.Trace

main :: IO ()
main = sequence_ $ print.(primes !!) <$> [0..10]

primes :: [Int]
primes = go [2..] where
   go (p:ps) = p : go [p' | p' <- ps, cli (p,p') , mod p' p /= 0]

cli :: (Int,Int) -> Bool
cli tuple = trace (show tuple) True

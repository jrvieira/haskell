module Main (main) where

import Debug.Trace

main :: IO ()
main = print $ y (1:)

y f = let x = f x in x

module Main where

import Data.List
import Data.Function
import Data.Ord
import Control.Arrow
import Debug.Trace

input :: String
input = "bananabar"

main :: IO ()
main = do
   print $ encode input
   print . uncurry decode . encode $ input

encode :: Ord a => [a] -> ([a], Int)
encode [] = ([],0)
encode s = (map (last.snd) &&& origin) . zip [0..] . sort $ matrix
  where
    matrix = map skip [0..pred l]
    skip n = take l . drop (l-n) . cycle $ s
    l = length s
    origin ~((n,x):xs)
        | x == s = n
        | otherwise = origin xs

decode :: Ord a => [a] -> Int -> [a]
decode [] _ = []
decode code n = skip $ go matrix !! n
    where
        go m
            | length (m !! 0) == length code = m
            | otherwise = go . spread code . sort $ m
        matrix = map (:[]) code
        sorted = sort $ code
        skip s = take (length s) . drop 1 . cycle $ s

spread [] ys = ys
spread _ []  = []
spread (x:xs) (y:ys) = (x:y) : spread xs ys

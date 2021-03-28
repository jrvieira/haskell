module Main where

import Data.Maybe
import Data.Map.Strict as Map
import Data.Set as Set
import Data.List as List
import Debug.Trace

input :: [String]
input = ["tup","whi","tsu","ats","hap","tis","whs"]

main :: IO ()
main = print $ secret input

type Before = Map Char (Set Char)

secret :: [String] -> String
secret = order . List.foldl' before Map.empty
        
before :: Before -> String -> Before
before b [] = b
before b (c:rest) = before (insertWith Set.union c (Set.fromList rest) b) rest
 
order :: Before -> String
order b = sortBy position string
   where
      string = keys b
      position :: Char -> Char -> Ordering
      position c1 c2
         | after c1 c2 = LT
         | after c2 c1 = GT
         | otherwise = EQ
      after :: Char -> Char -> Bool
      after x y 
         | Set.member y $ fromJust $ Map.lookup x b = True
         | Set.member True $ Set.map (flip after y) (fromJust $ Map.lookup x b) = True
         | otherwise = False


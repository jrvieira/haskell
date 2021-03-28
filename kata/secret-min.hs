module Main where

import Data.List

main :: IO ()
main = print $ secret ["lfo","iyf","ayo","lox","lmd","box","iro","lab","lio","lrt","mrt","mdo","dit","bdo","rtx","tyf"]

secret :: [String] -> String
secret list
  | null $ concat list = ""
  | otherwise = letter : (secret $ map (delete letter) list)
  where
    letter = head [ x | (x:_) <- list , not $ any (elem x) $ map (drop 1) list ]

module Codewars.G964.Tortoise where

race :: Int -> Int -> Int -> Maybe (Int, Int, Int)
race v1 v2 g
  | v1 >= v2 = Nothing
  | otherwise = Just (h,m,s)
    where
      ss = div (60*60*g) (v2-v1)
      h = div ss (60*60)
      m = div (ss-h*60*60) 60
      s = ss-h*60*60-m*60

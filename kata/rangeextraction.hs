module Main where

input :: [Int]
input = [-6, -3, -2, -1, 0, 1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20, 44, 66, 77, 78, 79, 80, 100]

main :: IO ()
main = print $ solution input

data Range = Y Int | N Int

-- (Maybe expected,string)
solution :: [Int] -> String
solution = snd . foldr go (undefined, "")
   where
      go :: Int -> (Range, String) -> (Range, String)
      go i (_, "") = (N i, show i)
      go i (N p, str)
         | i == pred p = (Y i, "-" ++ str)
         | otherwise = (N i, show i ++ "," ++ str)
      go i (Y p, str)
         | i == pred p = (Y i, str)
         | otherwise   = (N i, show i ++ "," ++ show p ++ str)

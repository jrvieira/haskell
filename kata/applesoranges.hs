
main :: IO ()
main = interact $ unlines . map show . fruit . map (map read) . map words . lines

fruit :: [[Int]] -> [Int]
fruit i = [count (caughtFrom a) apples, count (caughtFrom b) oranges]
   where
      [home,trees,_,apples,oranges] = i
      [s,t] = home
      [a,b] = trees
      count f l = length $ filter f l
      caughtFrom tree d
         | drop < s = False
         | drop > t = False
         | otherwise = True 
         where drop = tree + d

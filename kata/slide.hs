module Main where

{-

   /3/
  \7\ 4
 2 \4\ 6
8 5 \9\ 3

longestSlideDown [[3], [7, 4], [2, 4, 6], [8, 5, 9, 3]]
=> 23

-}

data Tree = Node Tree Tree | Leaf Int

sample = [[75],
          [95, 64],
          [17, 47, 82],
          [18, 35, 87, 10],
          [20, 04, 82, 47, 65],
          [19, 01, 23, 75, 03, 34],
          [88, 02, 77, 73, 07, 63, 67],
          [99, 65, 04, 28, 06, 16, 70, 92],
          [41, 41, 26, 56, 83, 40, 80, 70, 33],
          [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
          [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
          [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
          [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
          [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
          [04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23]]

-- result: 1074

main :: IO ()

main' = print $ maximum . leaves . tree $ sample

tree :: [[Int]] -> Tree
tree = step 0 0
  where
  step acc _ []     = Leaf acc
  step acc i (x:xs) = Node (step acc' i xs) (step acc' (i+1) xs)
    where
    acc' = acc + x !! i

leaves t = go t []
  where
  go :: Tree -> [Int] -> [Int]
  go (Leaf x) = (x :)
  go (Node l r) = go l . go r

-- or:

main = print $ maximum . slide $ sample

slide :: [[Int]] -> [Int]
slide t = go t 0 0 []
  where
  go :: [[Int]] -> Int -> Int -> [Int]-> [Int]
  go (x:xs) i acc
    | null xs = (acc':)
    | otherwise = go xs i acc' . go xs (i+1) acc'
    where
    acc' = acc+(x!!i)

import Data.IntMap.Strict

main :: IO ()
main = do
   print $ dice [6]
   print $ dice [6,6]

dice :: [Int] -> [(Int,Int)]
dice ds = freq empty $ sum <$> sequence [[1..n] | n <- ds]
   where
   freq m [] = toList m
   freq m (x:xs) = freq (insertWith (+) x 1 m) xs


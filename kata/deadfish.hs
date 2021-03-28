module Main (main) where

main :: IO ()
main = getLine >>= print.parse 

parse :: String -> [Int]
parse s = deadfish 0 s []
  where
    deadfish :: Int -> [Char] -> [Int] -> [Int]
    deadfish _ [] acc = acc
    deadfish v (x:xs) acc
      | 'i' <- x = deadfish (v+1) xs acc
      | 'd' <- x = deadfish (v-1) xs acc
      | 's' <- x = deadfish (v^2) xs acc
      | 'o' <- x = v : deadfish v xs acc
      | otherwise = deadfish v xs acc

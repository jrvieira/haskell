
main :: IO ()
main = interact $ unlines . map (show . grade . read) . tail . lines

grade :: Int -> Int
grade x
   | x < 38 = x
   | roundup > 2 = x + roundup
   | otherwise = x
   where roundup = 5 - (mod x 5)

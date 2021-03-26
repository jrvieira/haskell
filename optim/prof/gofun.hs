main :: IO ()
main = print $ count odd [1..9999999]

count x = go 0
   where
   go n [] = n
   go n (a:as)
      | x a = go (succ n) as
      | otherwise = go n as

import Zero.Draw

main :: IO ()
main = do
   print $ take 11 parity
   print "drawing..."
   plot "parity" $ take 1000 parity

parity :: [Int]
parity = 0 : 0 : 1 : map go [3..]
   where
   go n
      | even n = 1 + parity !! div n 2
      | otherwise = 0

-- simple plot from list of ints
plot :: String -> [Int] -> IO ()
plot f = draw f . go 0 
   where
   go _ [] = []
   go n (x:xs) = [((n,y),True) | y <- [0..x]] ++ go (succ n) xs


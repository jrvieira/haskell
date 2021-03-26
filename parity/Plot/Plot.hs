module Plot.Plot (plot) where

plot :: [Int] -> IO ()
plot xs = mapM_ putStrLn (bar <$> xs)
 
bar :: Int -> String
bar n = show n ++ take n (repeat '*')

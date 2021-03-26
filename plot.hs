main :: IO ()
main = mapM_ putStrLn $ bar . (2^) <$> [0 .. 9]
 
bar :: Integer -> String
bar n = show n ++ (take (fromInteger n) $ repeat '*')

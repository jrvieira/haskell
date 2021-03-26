import Debug.Trace

-- inline trace
(#) = flip trace

main :: IO ()
main = print $ loop list

list = [0..6] ++ cycle [7..9]

loop [] = Nothing
loop (x:xs) = go 1 1 x xs # ("   plxy:ys")
 where
  go _ _ _ [] = Nothing
  go pow lam x (y:ys)
   | x == y     = Just (lam,x)        # ("go " ++ show pow ++ show lam ++ show x ++ show y ++ " x=y")
   | pow == lam = go (2*pow) 1 y ys   # ("go " ++ show pow ++ show lam ++ show x ++ show y ++ " p=l | go 2p 1   y ys")
   | otherwise  = go pow (1+lam) x ys # ("go " ++ show pow ++ show lam ++ show x ++ show y ++ "     | go p  l+1 x ys")


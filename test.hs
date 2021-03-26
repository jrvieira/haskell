main :: IO ()
main = do
   print a
   print b
   print c
   where
   (a,b)
      | t0 <- (1>0) , t1 <- show 9 = (t0,t1)
      | otherwise = error "?"
   c
      | False = False
      | 9 <- read b = True

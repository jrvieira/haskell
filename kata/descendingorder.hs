main :: IO ()
main = interact $ sort
   where
      sort l
         | sorted l = l
         | (a:aa:as) <- l , a < aa = sort $ aa:(sort(a:as))
         | (a:aa:as) <- l = sort $ a:(sort (aa:as))

sorted :: String -> Bool
sorted l
   | [] <- l = True
   | [a] <- l = True
   | (a:aa:as) <- l , a >= aa , sorted (aa:as) = True
   | otherwise = False

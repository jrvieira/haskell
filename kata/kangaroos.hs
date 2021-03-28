
main :: IO ()
main = interact $ koo' . map read . words

koo' :: [Int] -> String
koo' i = if mod (x2-x1) (j1-j2) == 0 then "YES" else "NO"
   where
      [x1,j1,x2,j2] = i

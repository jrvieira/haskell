import Control.Parallel

main :: IO ()
main = do
   print $ "ok"
   let list = reverse [9..77] ++ reverse [7..99]
   print $ sort list
   print $ psort list

sort :: [Int] -> [Int]
sort [] = []
sort (x:xs) = lesser ++ x : greater
   where
   lesser = sort [y | y <- xs , y < x]
   greater = sort [y | y <- xs , y >= x]

psort :: [Int] -> [Int]
psort [] = []
psort (x:xs) = par (force greater) (pseq (force lesser) (lesser ++ x : greater))
   where
   lesser = psort [y | y <- xs , y < x]
   greater = psort [y | y <- xs , y >= x]


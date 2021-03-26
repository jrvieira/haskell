import Control.Arrow
import Data.Foldable

main :: IO ()
main = do
   print $ range [7]
   print $ range [] 
   print $ range [7,5,9,3,7,2,5] 
   print $ range [8,5,6,7,6,9,8]
   print $ range [-3,3]

range :: [Int] -> Int
range [] = 0
range ls = uncurry (-) . foldl' maxmin (minBound,maxBound) $ ls
   where
   maxmin (mx,mn) = max mx &&& min mn


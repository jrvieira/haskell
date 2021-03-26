import Control.Arrow
import Data.List

main :: IO ()
main = print $ range ([1..7] ++ [-7,7000000])

--range = uncurry (-) . (maximum &&& minimum)

range :: [Int] -> Int
range = uncurry (-) . foldl' maxmin (minBound,maxBound)
   where
   maxmin (mx,mn) = max mx &&& min mn

--range = foldMap' (Just . (Min &&& Max))

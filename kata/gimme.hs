import Data.List
import Data.Maybe

main = print $ gimme (9,1,12)

gimme :: (Ord a) => (a, a, a) -> Int
gimme (a, b, c) = fromJust $ elemIndex (minimum ms) ms
  where
  m = realToFrac (a+b+c) / 3
  d x = abs (realToFrac x - m)
  ms = [d a,d b,d c]

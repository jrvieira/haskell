import Data.List hiding ( delete )
import Data.Map.Strict hiding ( take, null, filter )
import Control.Arrow

import Data.Ratio

-- https://www.youtube.com/watch?v=a1DUUnhk3uE

main :: IO ()
main = do
   mapM_ print $ (\(u,t) -> show (u % t) ++ " = " ++ show u ++ "/" ++ show t) <$> take 7 x
   where
   x = (length . filter ((== 1) . length) &&& length) . cycles <$> [1..]

{- The number of permutations that are fully cyclic is
   1/n of the total, where n is the number of elements.
-}

cycles :: Int -> [[Int]]
cycles n = cycl <$> poss
   where
   poss :: [Map Int Int]
   poss = fromList . zip [0..] <$> permutations [0..pred n]
   cycl :: Map Int Int -> [Int]
   cycl = go 0 [0]
      where
      go :: Int -> [Int] -> Map Int Int -> [Int]
      go i r@ ~(c:cs) m
         | null m = r
         | Just x <- m !? i = go x (succ c:cs) m'
         | otherwise = go (head $ keys $ m') (0:r) m'
         where
         m' :: Map Int Int
         m' = delete i m


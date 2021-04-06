import Zero.Draw
import Data.List
import Data.Numbers.Primes

main :: IO ()
main = do
-- input <- map head . lines <$> readFile "h.txt"
-- mapM_ print $ permutations input
   -- elegant way to split text on empty line
-- print $ fmap tail . break null . lines $ "alpha\nbeta\ngamma\ndelta\n\nepsilon\nzeta\neta\ntheta\n\niota\nkappa\nlambda\nmu\n\nnu\nxi\nomicron\npi\n\nro\nsigma\ntau\n\nphi\nshi\npsi\nomega"
   -- pime gaps
-- plot "pd" $ take 9000 pd
-- mapM_ print $ take 25 lpd
-- mapM_ print $ take 25 dlpd
-- mapM_ print $ take 25 glpd
   mapM_ print $ take 25 $ zip3 dlpd lpd glpd
   mapM_ print dlpd

-- difference between consecutive primes
pd :: [Int]
pd = let ps = primes in zipWith (-) ps (0 : ps)

-- largest difference found so far
lpd :: [Int]
lpd = peaks 0 pd
   where
   peaks mx xs = peak : peaks peak xs'
      where
      (peak:xs') = dropWhile (<= mx) xs

-- difference between largest differences
dlpd :: [Int]
dlpd = zipWith (-) lpd (0 : lpd)

{- dlpd =
   2
   2
   2
   2
   6
   4
   2
   2
   12
   2
   8
   8
   20
   14
   10
   16
   2
   4
   14
   16
   6
   26
   30
   10
   2
   12
   14
   2
   32
   6
   4
   28
   16
   18
   28
   2
   10
   62
   8
   4
-}

-- gap between largest differences
glpd :: [Int]
glpd = gaps pd
   where
   gaps xs = length gap : gaps xs'
      where
      (gap,xs') = break (> head xs) xs 

-- simple plot from list of ints
plot :: String -> [Int] -> IO ()
plot f = draw f . go 0 
   where
   go _ [] = []
   go n (x:xs) = [((n,y),True) | y <- [0..x]] ++ go (succ n) xs


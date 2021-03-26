import Data.Numbers.Primes
import Zero.Draw

main :: IO ()
main = plot "primed" $ take 1000 primed

primed :: [Int]
primed = zipWith (-) primes (0 : primes)

-- simple plot from list of ints
plot :: String -> [Int] -> IO ()
plot f = draw f . go 0
   where
   go _ [] = []
   go n (x:xs) = [((n,y),True) | y <- [0..x]] ++ go (succ n) xs


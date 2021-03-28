
{- BETWEEN TWO SETS

given two sets a , b
find set n such that

every a[α] is a factor of every n[μ] is a factor of every b[β]

-}

main :: IO ()
main = interact $ show . length . factors . map (map read) . map words . lines

f :: [Int] -> Int
-- least common multiplier
f [] = 1
f [α] = α
f (α:α':τ)
   | δ == 0 = f $ αM : τ
   | otherwise = f $ div (αM*αm) δ : τ
   where
      αM = max α α'
      αm = min α α'
      δ = mod αM αm

factors :: [[Int]] -> [Int]
factors i = [ μ | μ <- [fa,2*fa .. minimum b] , all (\x -> mod x μ == 0) b ] -- this could be optimized by first calculating gcd b
   where
      (_:a:b:_) = i
      fa = f a

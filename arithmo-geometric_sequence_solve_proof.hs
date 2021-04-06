import Control.Applicative

main = do
   print $ ρ <$> [1..21]
   print $ ε <$> [1..21]
   print $ γ <$> [1..21]
   print $ all (liftA2 (==) ρ ε) $ [1..21]

δ = 1
x = 2
y = 1

-- recursive form
ρ(n) | n == 0    = 0
     | otherwise = x * ρ(n-1) + y

-- general case
-- γ(n) = ρ(n) + 1
ρ'(n) = γ(n) - 1
γ (n) = x**(n+1-δ) * ρ(δ-1)

-- explicit form : solved for δ = 1, x = 2, y = 1s
ε(n) = 2**n - 1

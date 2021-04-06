import Debug.Trace

main :: IO ()
main = mapM_ print $ take 900 fsd

infix 0 #
(#) = flip trace

fsd :: [Int]
fsd = 1 : go 1 1
 where
  go :: Int -> Int -> [Int]
  go n x
   | f > 1     = let y = div x f   in y : go m y    # "case A <- " ++ show n
   | otherwise = let y = x + m + 1 in y : go m y    # "case B <- " ++ show n
    where
     f = gcd m x
     m = n + 1

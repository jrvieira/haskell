import Criterion.Main
import Control.Monad

main :: IO ()
main = defaultMain [
   bgroup "naive"      $ benches (length . naive     ) ns,
   bgroup "memo"       $ benches (length . memo      ) ns,
   bgroup "fuse"       $ benches (length . fuse      ) ns,
   bgroup "monad"      $ benches (length . monad     ) ns,
   bgroup "tomsmeding" $ benches (length . tomsmeding) ns
   ]
   where
   ns = [7,10,13,16,19]

benches f ns = β <$> ns
   where
   β n = bench (show n) $ whnf f [0..pred n]

-- naive
naive = subsets
   where
   subsets [] = [[]]
   subsets (x:xs) = map (x :) (subsets xs) ++ (subsets xs)

-- memoized
memo = subsets
   where
   subsets [] = [[]]
   subsets (x:xs) = map (x :) ss ++ ss
      where
      ss = subsets xs

-- fused
fuse = subsets
   where
   subsets [] = [[]]
   subsets (x:xs) = ss x (subsets xs)
      where
      ss _ [] = []
      ss x (y:ys) = (x:y) : y : ss x ys

-- monadic
monad = filterM (const [True, False])

-- tomsmeding on irc
tomsmeding = subsets
   where
   subsets = ss []
      where
      ss a [] = [a]
      ss a (x:xs) = ss (x : a) xs ++ ss a xs


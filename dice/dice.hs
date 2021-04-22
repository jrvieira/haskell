import Data.IntMap.Strict
import Data.Ratio

type Dice = [Int]

d :: Int -> Dice
d n = [1..n]

main :: IO ()
main = do
   draw $ stat [d 2]
   draw $ ltat [d 3,d 3,d 3,d 3,d 3]

type Poss = [(Int,Int)]

stat :: [Dice] -> Poss
stat = freq empty . fmap sum . sequence
   where
   freq m [] = toList m
   freq m (x:xs) = freq (insertWith (+) x 1 m) xs

draw :: [(Int,Int)] -> IO ()
draw xs = print total >> mapM_ (putStrLn . graph) xs
   where
   graph (v,n) = replicate n '|' <> " " <> show v <> " (" <> show n <> ") " <> show (n % total)
   total = sum $ snd <$> xs


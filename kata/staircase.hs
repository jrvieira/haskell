main :: IO ()
main = interact $ unlines . staircase . read

staircase :: Int -> [String]
staircase n = map step [1..n]
   where
   step x = take (n-x) (repeat ' ') ++ take x (repeat '#')

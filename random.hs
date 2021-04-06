import System.Random

-- print random int from 9 to 11

main :: IO ()
main = do
   r <- rand 9 11
   print r
   -- rand 9 11 >>= print

rand :: Int -> Int -> IO Int
rand min max = do
   gen <- newStdGen
   let (r , _) = randomR (min , max) gen :: (Int , StdGen)
   pure r

import Text.Printf

roundToStr :: (PrintfArg a, Floating a) => Int -> a -> String
roundToStr = printf "%0.*f"

main :: IO ()
main = interact $ unlines . map (roundToStr 6) . plusminus . map (map read) . map words . lines

plusminus :: [[Int]] -> [Double]
plusminus i = [pos/size, neg/size, zer/size]
    where
    (head:sample:_) = i
    size = fromIntegral (head !! 0)
    pos = fromIntegral $ length $ filter (>0) sample
    neg = fromIntegral $ length $ filter (<0) sample
    zer = fromIntegral $ length $ filter (==0) sample

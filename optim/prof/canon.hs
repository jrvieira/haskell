main :: IO ()
main = print $ count odd [1..9999999]

count x = length . filter x

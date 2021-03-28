module Main where

main :: IO ()
main = mapM_ print $ finds "banana" "bbananana"

finds :: String -> String -> [String]
finds [] [] = [[]]
finds (_:_) [] = []
finds [] (t:tt) = ('-':) <$> finds [] tt
finds (c:cc) (t:tt)
   | c == t = ((c:) <$> finds cc tt) ++ (('-':) <$> finds (c:cc) tt)
   | c /= t = ('-':) <$> finds (c:cc) tt

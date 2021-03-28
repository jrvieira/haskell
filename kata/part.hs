module Main where

main :: IO ()
main = mapM_ putStrLn . verify $ (length.part) <$> [0..111]

part :: Int -> [[Int]]
part = (mapPart !!)
   where
   go 0 = [[0]]
   go 1 = [[1]]
   go n = concat $ (\x -> map (x:) (takeWhile ((<=x).head) $ mapPart !! (n-x))) <$> [1..n]
   mapPart = map go [0..]

----
----

verify :: [Int] -> [String]
verify = map report . zip p
   where
   p = [1,1,2,3,5,7,11,15,22,30,42,56,77,101,135,176,231,297,385,490,627,792,1002,1255,1575,1958,2436,3010,3718,4565,5604,6842,8349,10143,12310,14883,17977,21637,26015,31185,37338,44583,53174,63261,75175,89134,105558,124754,147273,173525]
   report (l,p)
      | l == p = show p ++ " " ++ clr Green "v"
      | otherwise = show p ++ " " ++ clr Red (show l)

{- Color -}

data Color = Reset | Default | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White | Bold | Dim | Italic | Strike | Underline | Reverse

code :: Color -> Int
code Reset = 0
code Default = 10
code Black = 30
code Red = 31
code Green = 32
code Yellow = 33
code Blue = 34
code Magenta = 35
code Cyan = 36
code White = 37
code Bold = 1
code Dim = 2
code Italic = 3
code Strike = 9
code Underline = 4
code Reverse = 7

instance Show Color where
    show c = "\x1b[" ++ (show . code) c ++ "m"

type ColorStr = Color -> String -> String

clr :: ColorStr
clr c s = show c ++ s ++ show Reset

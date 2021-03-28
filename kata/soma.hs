module Main where

{-

sum [1..n] == n * (n+1) / 2

PROOF:

   o xxxx
   oo xxx
   ooo xx
   oooo x
   …

-}

main :: IO ()
main = mapM_ putStrLn . verify $ map soma [0..1111]

soma :: Int -> Int
soma n = n * (n+1) `div` 2

----
----

verify :: [Int] -> [String]
verify = map report . zip p
   where
   p = 0:[ p!!(n-1) + n | n <- [1..] ]
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

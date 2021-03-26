module Color
   ( Color (..)
   , clr
   ) where

data Color = Reset | Default | K | R | G | Y | B | M | C | W | Bold | Dim | Italic | Strike | Underline | Reverse 

code :: Color -> Int
code Reset = 0
code Default = 10
code K = 30
code R = 31
code G = 32
code Y = 33
code B = 34
code M = 35
code C = 36
code W = 37
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

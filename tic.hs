module Main (main) where

import Data.List
import System.Environment

{- DATA -}

data Status = N | O | X
   deriving Eq

instance Show Status where
   show N = clr Dim "·"
   show O = clr Y "o"
   show X = clr B "x"

data Board = Board Status Status Status Status Status Status Status Status Status

instance Show Board where
   show (Board a b c d e f g h i) =
      "\n" ++ show a ++ " " ++ show b ++ " " ++ show c ++
      "\n" ++ show d ++ " " ++ show e ++ " " ++ show f ++
      "\n" ++ show g ++ " " ++ show h ++ " " ++ show i ++
      "\n"

-- tree
data Step = Step Board [Step] | Leaf Board
   deriving Show

{- MAIN -}

main :: IO ()
--main = play . step $ seed
main = do
   let s = stat . step $ seed
   print $ count X s
   print $ count O s
   print $ count N s

seed :: Board
seed = Board N N N N N N N N N

{- PLAY -}

-- hash for pseudo-random game
hash :: Int
hash = 2

play :: Step -> IO ()
play (Step b p) = do { print b ; play $ p !! mod hash (length p) }
play (Leaf b) = print b

{- STAT -}

vict :: Board -> Status
vict board
   | Board _ _ _ a b c _ _ _ <- board , a == b , b == c , c /= N = c
   | Board a b c _ _ _ _ _ _ <- board , a == b , b == c , c /= N = c
   | Board _ _ _ _ _ _ a b c <- board , a == b , b == c , c /= N = c
   | Board a _ _ b _ _ c _ _ <- board , a == b , b == c , c /= N = c
   | Board _ a _ _ b _ _ c _ <- board , a == b , b == c , c /= N = c
   | Board _ _ a _ _ b _ _ c <- board , a == b , b == c , c /= N = c
   | Board a _ _ _ b _ _ _ c <- board , a == b , b == c , c /= N = c
   | Board _ _ a _ b _ c _ _ <- board , a == b , b == c , c /= N = c
   | all (/= N) (toList board) = N
   | otherwise = error "game over"

stat :: Step -> [Status]
stat (Step _ p) = concat . map stat $ p
stat (Leaf b)   = [vict b]

{- TIC -}

toList :: Board -> [Status]
toList (Board a b c d e f g h i) = [a,b,c,d,e,f,g,h,i]

toBoard :: [Status] -> Board
toBoard [a,b,c,d,e,f,g,h,i] = Board a b c d e f g h i
toBoard _ = error "invalid list toBoard"

over :: Board -> Bool
over board
   | Board _ _ _ a b c _ _ _ <- board , a == b , b == c , c /= N = True
   | Board a b c _ _ _ _ _ _ <- board , a == b , b == c , c /= N = True
   | Board _ _ _ _ _ _ a b c <- board , a == b , b == c , c /= N = True
   | Board a _ _ b _ _ c _ _ <- board , a == b , b == c , c /= N = True
   | Board _ a _ _ b _ _ c _ <- board , a == b , b == c , c /= N = True
   | Board _ _ a _ _ b _ _ c <- board , a == b , b == c , c /= N = True
   | Board a _ _ _ b _ _ _ c <- board , a == b , b == c , c /= N = True
   | Board _ _ a _ b _ c _ _ <- board , a == b , b == c , c /= N = True
   | otherwise = all (/= N) (toList board)

step :: Board -> Step
step = step' (cycle [X,O])
   where
      step' [] _ = error "impossible end of cycle"
      step' (t:tt) board
         | over board = Leaf board
         | otherwise = Step board $ map (step' tt) (next t board)

next :: Status -> Board -> [Board]
next status = map toBoard . poss status . toList

poss :: Status -> [Status] -> [[Status]]
poss status = poss' []
   where
      poss' _ [] = []
      poss' pre (p:pp)
         | N <- p = (pre++status:pp) : poss' (pre++[p]) pp
         | otherwise = poss' (pre++[p]) pp

{- ZERO -}

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

{- COLOR -}

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

module Sim where

import Zero
import Color
--import System.Console.ANSI
--import Control.Concurrent
import Data.List
import Data.Maybe

import System.IO
import System.Environment
import System.Directory
import Codec.Picture

dimensions :: (Int,Int)
dimensions = (600,600)

w :: Int
w = fst dimensions
h :: Int
h = snd dimensions

fps :: Int
fps = 1

data Dir = N | W | E | S | NW | NE | SW | SE deriving (Eq,Show)
data Entity = None | Block | Dart Dir deriving (Eq,Show)
type Frame = [Entity]

type Glyph = Char
type Screen = [Glyph]

scan :: Glyph -> Entity
scan g = fst . fromJust . find ((== g) . snd) $ code

load :: Screen -> Frame
load = map scan

code :: [(Entity,Glyph)]
code = [
   (None,'.'),
   (Block,'x')
   ]

-- pixels

black :: Pixel8
black = 0

white :: Pixel8
white = 255

pixel :: Frame -> Int -> Int -> Pixel8
pixel f x y
   | Block <- f !! (x+y*w) = black
   | _ <- f !! (x+y*w) = white

-- io

run :: Screen -> IO ()
run seed = mapM_ (draw b) [0..]
   where b = buffer . load $ seed

draw :: [Frame] -> Int -> IO ()
draw b i = do
   createDirectoryIfMissing True "io"
   done <- doesFileExist file
   if done then
      putStrLn $ clr White (file ++ " skipped") -- print existing file to console
   else do
      savePngImage file $ ImageY8 $ generateImage (pixel $ b !! i) w h
      putStrLn $ clr Green file -- print drawn file to console
   where
      file = "io/" ++ show w ++ "x" ++ show h ++ "tapestry_" ++ show i ++ ".png"

buffer :: Frame -> [Frame]
buffer = iterate next

next :: Frame -> Frame
next f = map carpet $ take (w*h) [0..]
   where
      up = adjacent N f
      left = adjacent W f
      right = adjacent E f
      down = adjacent S f
      sim i = if odd $ count Block (adjacents f i) then Block else None
      invert i = if f # i == Block then None else Block
      conway i = case f # i of
         Block -> if count Block (adjacents' f i) `elem` [2,3] then Block else None
         _ -> if count Block (adjacents' f i) == 3 then Block else None
      smooth i
         | count None (adjacents' f i) >= 5 = None
         | otherwise = Block
      carpet i = case f !! i of
         Block -> if count Block (adjacents' f i) `elem` [3,4,5] then Block else None
         _ -> if count Block (adjacents' f i) `elem` [2,3,4,5] then Block else None
--
navigate :: Dir -> Frame -> Frame
navigate d f = map (adjacent d f) [0..]
--

type Position = (Int,Int)

s = w*h

sop :: Position -> Int
sop (x,y) = mod x w + mod y (div s w) * w

pos :: Int -> Position
pos i = (posx i,posy i)

posx :: Int -> Int
posx i = mod i w

posy :: Int -> Int
posy i = mod (div i w) (div s w)

up :: Int -> Int
up i = sop (posx i , posy i - 1)

dn :: Int -> Int
dn i = sop (posx i , posy i + 1)

lf :: Int -> Int
lf i = sop (posx i - 1 , posy i)

rt :: Int -> Int
rt i = sop (posx i + 1 , posy i)

adjacent :: Dir -> Frame -> Int -> Entity
adjacent d f i
   | N <- d = f # (up i)
   | W <- d = f # (lf i)
   | E <- d = f # (rt i)
   | S <- d = f # (dn i)
   | NW <- d = f # (up.lf $ i)
   | NE <- d = f # (up.rt $ i)
   | SW <- d = f # (dn.rt $ i)
   | SE <- d = f # (dn.lf $ i)
      where
         s = (w*h)
         x = mod i w
         y = div i w

adjacents :: Frame -> Int -> [Entity]
adjacents f i = map ($ i) $ map ($ f) $ map adjacent [N,W,E,S]

adjacents' :: Frame -> Int -> [Entity]
adjacents' f i = map ($ i) $ map ($ f) $ map adjacent [N,W,E,S,NW,NE,SW,SE]

-- modular index
(#) :: [a] -> Int -> a
[] # _ = error "[] # _"
t # i = t !! index
   where
      index = if i >= (w*h) || i < 0 then mod i (w*h) else i

-- count
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

module Sim (run,render,load,next) where

import Zero
import System.Console.ANSI
import Control.Concurrent
import Data.List
import Data.List.Split
import Data.Maybe

s :: (Int,Int)
s = (25,18)

w :: Int
w = fst s
h :: Int
h = snd s

fps :: Int
fps = 1

type Glyph = Char
type Screen = [Glyph]

data Dir = N | W | E | S | NW | NE | SW | SE deriving Eq
data Entity = None | Block | Dart Dir deriving Eq
type Frame = [Entity]

code :: [(Entity,Glyph)]
code = [
   (None,'.'),
   (Block,'x'),
   (Dart N,'*'),
   (Dart W,'*'),
   (Dart E,'*'),
   (Dart S,'*')
   ]

scan :: Glyph -> Entity
scan g = fst . fromJust . find ((== g) . snd) $ code

draw :: Entity -> Glyph
draw e = snd . fromJust . find ((== e) . fst) $ code

load :: Screen -> Frame
load = map scan

render :: Frame -> Screen
render = unlines . chunksOf w . map draw

run :: Screen -> IO ()
run = mapM_ play . buffer . load
   where
      play f = do
         let screen = render $ f
         --seq screen
         (threadDelay $ div 1000000 fps)
         clearScreen
         putStrLn "-- DEMO --"
         putStrLn screen
         pure ()

buffer :: Frame -> [Frame]
buffer = iterate next

next :: Frame -> Frame
next f = map conway $ take (w*h) [0..]
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
--
navigate :: Dir -> Frame -> Frame
navigate d f = map (adjacent d f) [0..]
--

adjacent :: Dir -> Frame -> Int -> Entity
adjacent d f i
   | N <- d = f # (i - w)
   | W <- d = f # (i - x + mod (x - 1) w)
   | E <- d = f # (i - x + mod (x + 1) w)
   | S <- d = f # (i + w)
   | NW <- d = f # ((i - w) - x + mod (x - 1) w)
   | NE <- d = f # ((i - w) - x + mod (x + 1) w)
   | SW <- d = f # ((i + w) - x + mod (x - 1) w)
   | SE <- d = f # ((i + w) - x + mod (x + 1) w)
      where
         x = mod i w
         y = div i w

adjacents :: Frame -> Int -> [Entity]
adjacents f i = map ($ i) $ map ($ f) $ map adjacent [N,W,E,S]

adjacents' :: Frame -> Int -> [Entity]
adjacents' f i = map ($ i) $ map ($ f) $ map adjacent [N,W,E,S,NW,NE,SW,SE]

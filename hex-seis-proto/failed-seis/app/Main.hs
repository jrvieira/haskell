module Main where

import Terminal.Game

import Verse.Type
import Verse.Verse

import Data.List ( intersperse )

main :: IO ()
main = do
   playGame $ Game 1 start logic draw quit

data State = Σ {
   τ :: Some ,
   ν :: Verse ,
   κ :: Either String Char ,
   σ :: String ,
   ψ :: Bool }

start :: State
start = Σ (Some 0) verse (Left "press any key") "" False

logic :: GEnv -> State -> Event -> State
logic (GEnv (w,h) fps) st ev
   | KeyPress k <- ev = st { κ = Right k }
   | Tick <- ev = st { τ = succ $ τ st }

draw :: GEnv -> State -> Plane
draw (GEnv (w,h) fps) st = p_verse
   & (2,1) %.< p_tick
   & (1,1) %.< p_stats
   where
   p_tick = makeOpaque $ color Yellow Dull $ word $ unwords ["τ" , show (τ st) , kstat]
   p_stats = color White Vivid $ word $ show w <> "x" <> show h <> " " <> show fps <> "fps"
   p_verse = textBox (pred $ 2 * width) height $ intersperse ' ' $ map render [0..w*h]
   kstat
      | Right k <- κ st = [k]
      | Left s <- κ st = s
   (γ,vn,iv) = ν st
   render :: Int -> Char
   render i
      | Just v <- iv i , (x,n,ns) <- vn v = symbol x
      | otherwise = '.'

quit :: State -> Bool
quit st = ψ st

